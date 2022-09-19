import { CSS, Container, Col, Row, Card, Spacer, Button, Input, Loading, Text, Image, Link, Checkbox, Modal, Collapse } from '@nextui-org/react'
import type { NextPage } from 'next'
import { useSession } from 'next-auth/react'
import dynamic from 'next/dynamic'
import { useRecoilRefresher_UNSTABLE, useRecoilValueLoadable } from 'recoil'
import { RequireAuth } from '../../../../components/require_auth'
import { RequireProject } from '../../../../components/require_project'
import { appListQuery, appSelector, projectSelector } from '../../../../feutil/state'
import { loadProjectProps, ProjectProps } from '../../../../service/util'
import NextLink from 'next/link'
import type { App, Machine } from '../../../../models'
import { Footer } from '../../../../components/footer'
import { useRouter } from 'next/router'
import { AppCard } from '../../../../components/app_card'
import React, { useCallback, useEffect, useState } from 'react'
import { loadJson } from '../../../../feutil/network'
import { AppInfo } from '../../../../service/api_types'
import { InferAttributes } from 'sequelize'
import { MemoConfigEditor } from '../../../../components/config_editor'
import { AppConfig } from '../../../../models/App'
import { useAsync, useInterval } from 'react-use'
import { MachineInfo } from '../../../../service/fly'
import { useLatch } from '../../../../feutil/latch'
import { BackToAllApps } from '../../../../components/back_to_all_apps'

export const getServerSideProps = loadProjectProps;

const DeleteModal: React.FC<{ app: AppInfo, isOpen: boolean, busy: boolean, onClose: () => void, doDelete: () => void }> = ({ app, isOpen, busy, onClose, doDelete }) => {
  const [name, setName] = React.useState("");

  return (
    <Modal
      closeButton
      aria-labelledby="modal-confirm-delete-title"
      open={isOpen}
      onClose={onClose}
    >
      <Modal.Header>
        <Text id="modal-confirm-delete-title" size={18}>
          Delete this app?
        </Text>
      </Modal.Header>
      <Modal.Body>
        <Row>
          <Text size={14}>Enter the name of this app to confirm:</Text>
        </Row>
        <Input
          clearable
          bordered
          fullWidth
          color="primary"
          size="lg"
          value={name}
          onChange={(e) => setName(e.target.value)}
        />
      </Modal.Body>
      <Modal.Footer>
        <Button auto flat onClick={onClose}>
          Close
        </Button>
        <Button auto color="error" disabled={busy || name !== app.name} onClick={doDelete}>
          {busy ? <Loading color="currentColor" size="sm" /> : "Delete"}
        </Button>
      </Modal.Footer>
    </Modal>
  )
}

const MemoDeleteModal = React.memo(DeleteModal);

const machineStateToBgColor: Record<string, CSS['bgColor']> = {
  "started": "$success",
  "stopped": "gray",
  "created": "$warning",
  "stopping": "$warning",
}

const MachineStateCard: React.FC<{ machine: InferAttributes<Machine> }> = ({ machine }) => {
  const info = useAsync(() => loadJson<MachineInfo>(`/api/machine/info`, {
    projectId: machine.projectId,
    id: machine.id,
  }), [machine.projectId, machine.id]);

  if (info.loading || !info.value) return (
    <Row css={{ h: 80 }}>
      <Loading color="currentColor" />
      <Text css={{ pl: 20 }} weight="semibold">Loading machine status</Text>
    </Row>
  )

  return (
    <Col css={{ h: 80 }}>
      <Row>
        <Text css={{ color: "gray" }} size="$sm">State</Text>
      </Row>
      <Row align="center" css={{ pt: 10 }}>
        <Col css={{ w: "auto" }}>
          <Row css={{ bgColor: machineStateToBgColor[info.value.state] || "$error", h: 10, w: 10, borderRadius: "50%" }}></Row>
        </Col>
        <Col css={{ w: "auto", pl: 20 }}>
          <Text css={{ fontFamily: "$mono" }} size="$sm">{info.value.updated_at}</Text>
        </Col>
        <Col css={{ w: "auto", pl: 10 }}><Text weight="semibold" css={{ fontFamily: "$mono" }}>{info.value.state}</Text></Col>
      </Row>
    </Col>
  )
}

const MemoMachineStateCard = React.memo(MachineStateCard);

const InitializationWatcher: React.FC<{ machine: InferAttributes<Machine> }> = ({ machine }) => {
  const appListRefresh = useRecoilRefresher_UNSTABLE(appListQuery(machine.projectId));
  useInterval(() => {
    appListRefresh();
  }, 5000);

  return (
    <></>
  )
}

const SingleApp: NextPage<ProjectProps> = ({ projectId, userId }) => {
  const router = useRouter();
  const appId = parseInt(router.query.appId as string);
  const appValue = useRecoilValueLoadable(appSelector([projectId, appId]));
  const appLatch = useLatch(appValue.state === "hasValue" ? appValue.contents : null);
  const [deleteOpen, setDeleteOpen] = useState(false);
  const [deleteBusy, setDeleteBusy] = useState(false);
  const appListRefresh = useRecoilRefresher_UNSTABLE(appListQuery(projectId));
  const machine = appLatch?.machines[0] || null;
  const machineReady = !!machine?.flyId;
  const buildConfigRef: React.MutableRefObject<() => AppConfig | string> = React.useRef(() => "not ready");

  const closeDelete = useCallback(() => {
    if (deleteBusy) return;
    setDeleteOpen(false);
  }, [deleteBusy]);
  const doDelete = useCallback(async () => {
    setDeleteBusy(true);
    try {
      await loadJson("/api/app/delete", { projectId, id: appId });
      appListRefresh();
      router.push(`/projects/${projectId}/apps`);
    } catch (e) {
      console.log(e);
      setDeleteBusy(false);
    }
  }, [projectId, appId, router, appListRefresh]);

  return (
    <Container xs css={{ pt: 80 }}>
      <BackToAllApps projectId={projectId} />
      <Col>
        <Row align="center">
          <Col css={{ width: "auto" }}>
            <Text h1 size={36}>Manage App</Text>
          </Col>
        </Row>
        {!!appLatch && <>
          <AppCard app={appLatch} inSingleAppPage />

          {!!machine && !machineReady && <Row css={{ pt: 40 }}>
            <Loading color="currentColor"></Loading>
            <Text css={{ pl: 20 }} weight="semibold">Initializing</Text>
            <InitializationWatcher machine={machine} />
          </Row>}


          {machineReady && <Row css={{ pt: 40 }}>
            <MemoMachineStateCard machine={machine!} />
          </Row>}

          <Col css={{ pt: 40 }}>
            <Collapse.Group accordion={false} css={{ px: 0 }}>
              <Collapse title="App Config">
                <MemoConfigEditor readonly spec={appLatch.spec} initialConfig={appLatch.config} buildConfigRef={buildConfigRef} />
              </Collapse>
              <Collapse title="Danger Zone">
                <Row>
                  <Button color="error" onClick={() => setDeleteOpen(true)}>Delete App</Button>
                </Row>
              </Collapse>
            </Collapse.Group>
          </Col>
          <MemoDeleteModal app={appLatch} isOpen={deleteOpen} busy={deleteBusy} onClose={closeDelete} doDelete={doDelete} />
        </>}
      </Col>
      <Footer projectId={projectId} userId={userId} />
    </Container>
  )
}

export default dynamic(() => Promise.resolve(SingleApp), {
  ssr: false
});
