import { Container, Col, Row, Card, Spacer, Button, Input, Loading, Text, Image, Link, Checkbox, Modal } from '@nextui-org/react'
import type { NextPage } from 'next'
import { useSession } from 'next-auth/react'
import dynamic from 'next/dynamic'
import { useRecoilRefresher_UNSTABLE, useRecoilValueLoadable } from 'recoil'
import { RequireAuth } from '../../../../components/require_auth'
import { RequireProject } from '../../../../components/require_project'
import { appListQuery, appSelector, projectSelector } from '../../../../feutil/state'
import { loadProjectProps, ProjectProps } from '../../../../service/util'
import NextLink from 'next/link'
import type { App } from '../../../../models'
import { Footer } from '../../../../components/footer'
import { useRouter } from 'next/router'
import { AppCard } from '../../../../components/app_card'
import React, { useCallback, useState } from 'react'
import { loadJson } from '../../../../feutil/network'

export const getServerSideProps = loadProjectProps;

const DeleteModal: React.FC<{ app: App, isOpen: boolean, busy: boolean, onClose: () => void, doDelete: () => void }> = ({ app, isOpen, busy, onClose, doDelete }) => {
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
          {busy ? <Loading color="currentColor" size="sm" /> : "Confirm"}
        </Button>
      </Modal.Footer>
    </Modal>
  )
}

const MemoDeleteModal = React.memo(DeleteModal);

const SingleApp: NextPage<ProjectProps> = ({ projectId, userId }) => {
  const router = useRouter();
  const appId = parseInt(router.query.appId as string);
  const app = useRecoilValueLoadable(appSelector([projectId, appId]))
  const [deleteOpen, setDeleteOpen] = useState(false);
  const [deleteBusy, setDeleteBusy] = useState(false);
  const appListRefresh = useRecoilRefresher_UNSTABLE(appListQuery(projectId));

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
    <RequireAuth>
      <RequireProject>
        <Container xs css={{ pt: 80 }}>
          <Col>
            <Row align="center">
              <Col css={{ width: "auto" }}>
                <Text h1 size={36}>Manage App</Text>
              </Col>
            </Row>
            {app.state === "hasValue" && !!app.contents && <>
              <AppCard app={app.contents} inSingleAppPage />
              <Row css={{ pt: 20 }}>
                <Button color="error" onClick={() => setDeleteOpen(true)}>Delete App</Button>
              </Row>
              <MemoDeleteModal app={app.contents} isOpen={deleteOpen} busy={deleteBusy} onClose={closeDelete} doDelete={doDelete} />
            </>}
          </Col>
          <Footer projectId={projectId} userId={userId} />
        </Container>
      </RequireProject>
    </RequireAuth>
  )
}

export default dynamic(() => Promise.resolve(SingleApp), {
  ssr: false
});
