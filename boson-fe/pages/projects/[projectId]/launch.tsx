import { Container, Col, Row, Card, Spacer, Button, Input, Loading, Text } from '@nextui-org/react'
import loading from '@nextui-org/react/types/loading'
import type { GetServerSideProps, NextPage } from 'next'
import { useSession } from 'next-auth/react'
import dynamic from 'next/dynamic'
import { useRouter } from 'next/router'
import React, { useCallback, useEffect, useState } from 'react'
import { useRecoilValue, useRecoilValueLoadable } from 'recoil'
import { RequireAuth } from '../../../components/require_auth'
import { RequireProject } from '../../../components/require_project'
import { loadJson } from '../../../feutil/network'
import { firstProjectSelector, projectListQuery, projectSelector } from '../../../feutil/state'
import type { AppConfig, AppSpec } from '../../../models/App'
import { loadProjectProps } from '../../../service/util'

export const getServerSideProps = loadProjectProps;

const Launch: NextPage<{ projectId: string }> = ({ projectId }) => {
  const [name, setName] = useState("");
  const [spec, setSpec] = useState(null as AppSpec | null);
  const [loading, setLoading] = useState(false);
  const [lastError, setLastError] = useState("");
  const router = useRouter();
  const specUrl = typeof router.query.spec === "string" ? router.query.spec : ""
  const [pendingSpecUrl, setPendingSpecUrl] = useState("");
  const [cpus, setCpus] = useState("1");
  const [memoryMB, setMemoryMB] = useState("256");
  const [env, setEnv] = useState({} as Record<string, string>);

  useEffect(() => {
    if (!spec) return;

    setCpus("1");
    setMemoryMB("" + (spec.minMemoryMB || 256));
    setEnv({});
  }, [spec])

  const doLoadSpec = useCallback(() => {
    router.push({
      pathname: router.asPath.split("?")[0],
      query: { spec: pendingSpecUrl },
    }, undefined, { shallow: true });
    return false;
  }, [pendingSpecUrl]);

  useEffect(() => {
    if (!specUrl) return;
    loadJson("/api/app/specproxy?url=" + encodeURIComponent(specUrl))
      .then(x => { setSpec(x); setLastError("") })
      .catch(e => setLastError("" + e));
  }, [specUrl]);

  const doLaunch = useCallback(() => {
    const config: AppConfig = {
      cpus: parseInt(cpus),
      memoryMB: parseInt(memoryMB),
      env,
    };

    if (!Number.isSafeInteger(config.cpus)) {
      setLastError("Invalid CPU count");
      return;
    }

    if (!Number.isSafeInteger(config.memoryMB)) {
      setLastError("Invalid memory");
      return;
    }

    setLoading(true);

    loadJson("/api/app/create", {
      projectId,
      name,
      spec,
      config,
    })
      .then(x => {
        router.push(`/projects/${projectId}/apps/${x.id}`);
        setLastError("");
      })
      .catch(e => setLastError("" + e))
      .finally(() => {
        setLoading(false);
      })
  }, [name, spec, cpus, memoryMB, env]);

  return (
    <RequireAuth>
      <RequireProject>
        <Container xs css={{ pt: 80 }}>
          <Col>
            <Row css={{ pb: 20 }}><Text h1 size={36}>Launch a new app</Text></Row>

            {!specUrl && <Col>
              <Row css={{ pb: 40 }}>
                <Input css={{ flexGrow: 1 }} underlined labelPlaceholder="Specification URL" value={pendingSpecUrl} onChange={e => setPendingSpecUrl(e.target.value)} />
              </Row>
              <Row css={{ pb: 20 }}>
                <Button css={{ flexGrow: 1 }} disabled={loading} onClick={doLoadSpec}>
                  {loading ? <Loading color="currentColor" size="sm" /> : "Continue"}</Button>
              </Row>
              {!!lastError && <Row css={{ pb: 20 }}><Text color="error">{lastError}</Text></Row>}
            </Col>}

            {!!specUrl && !spec && <>
              {!lastError && <Row css={{ pb: 20 }}><Loading color="currentColor" size="sm" /></Row>}
              {!!lastError && <Row css={{ pb: 20 }}><Text color="error">{lastError}</Text></Row>}
            </>}

            {
              !!spec && <Col>
                <Row css={{ pb: 40 }}>
                  <Input css={{ flexGrow: 1 }} underlined labelPlaceholder="Name" value={name} onChange={e => setName(e.target.value)} />
                </Row>
                <Row css={{ pb: 40 }}><Text as="h2" size={20} color="primary">Machine</Text></Row>
                <Row css={{ pb: 40 }}>
                  <Input css={{ flexGrow: 1 }} underlined labelPlaceholder="CPUs" value={cpus} onChange={e => setCpus(e.target.value)} />
                </Row>
                <Row css={{ pb: 40 }}>
                  <Input css={{ flexGrow: 1 }} underlined labelPlaceholder="Memory (MiB)" value={memoryMB} onChange={e => setMemoryMB(e.target.value)} />
                </Row>
                <Row css={{ pb: 40 }}><Text as="h2" size={20} color="primary">App settings</Text></Row>
                <Col>
                </Col>
                <Row css={{ pb: 20 }}>
                  <Button css={{ flexGrow: 1 }} disabled={loading} onClick={doLaunch}>
                    {loading ? <Loading color="currentColor" size="sm" /> : "Launch"}</Button>
                </Row>
                {!!lastError && <Row css={{ pb: 20 }}><Text color="error">{lastError}</Text></Row>}
              </Col>
            }
            <Row></Row>

          </Col>
        </Container>
      </RequireProject>
    </RequireAuth>
  )
}

export default dynamic(() => Promise.resolve(Launch), {
  ssr: false
});
