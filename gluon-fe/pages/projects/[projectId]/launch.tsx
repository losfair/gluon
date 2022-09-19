import { Container, Col, Row, Button, Input, Loading, Text, Image, Link } from '@nextui-org/react'
import type { NextPage } from 'next'
import dynamic from 'next/dynamic'
import { useRouter } from 'next/router'
import React, { useCallback, useEffect, useMemo, useState } from 'react'
import { useRecoilRefresher_UNSTABLE } from 'recoil'
import { BackToAllApps } from '../../../components/back_to_all_apps'
import { MemoConfigEditor } from '../../../components/config_editor'
import { Footer } from '../../../components/footer'
import { RequireAuth } from '../../../components/require_auth'
import { RequireProject } from '../../../components/require_project'
import { loadJson } from '../../../feutil/network'
import { appListQuery } from '../../../feutil/state'
import type { AppConfig, AppSpec } from '../../../models/App'
import { loadProjectProps, ProjectProps } from '../../../service/util'
import { Base64 } from 'js-base64';

export const getServerSideProps = loadProjectProps;

function generateDefaultAppName(spec: AppSpec): string {
  const prefix = (spec.name || spec.image).toLowerCase().replace(/[^a-z0-9]/g, "-");
  // Generate a random 4-character hex string
  const suffix = Math.floor(Math.random() * 0x10000).toString(16).padStart(4, "0");
  return `${prefix}-${suffix}`;
}

const Launch: NextPage<ProjectProps> = ({ projectId, userId }) => {
  const [name, setName] = useState("");
  const [spec, setSpec] = useState(null as AppSpec | null);
  const [loading, setLoading] = useState(false);
  const [lastError, setLastError] = useState("");
  const router = useRouter();
  const specUrl = typeof router.query.spec === "string" ? router.query.spec : ""
  const [pendingSpecUrl, setPendingSpecUrl] = useState("");
  const initialConfig: AppConfig = useMemo(() => {
    // Generate values
    const env: Record<string, string> = {};
    for (const [k, v] of Object.entries(spec?.env || {})) {
      switch (v.generate) {
        case "random-base64-16": {
          const bytes = new Uint8Array(16);
          crypto.getRandomValues(bytes);
          env[k] = Base64.fromUint8Array(bytes);
          break;
        }
        case "random-base64-24": {
          const bytes = new Uint8Array(24);
          crypto.getRandomValues(bytes);
          env[k] = Base64.fromUint8Array(bytes);
          break;
        }
        case "random-base64-32": {
          const bytes = new Uint8Array(32);
          crypto.getRandomValues(bytes);
          env[k] = Base64.fromUint8Array(bytes);
          break;
        }
        default:
          break;
      }
    }
    return {
      cpus: 1,
      memoryMB: spec?.minMemoryMB || 256,
      env,
    }
  }, [spec]);
  const buildConfigRef: React.MutableRefObject<() => AppConfig | string> = React.useRef(() => "buildConfigRef not set");
  const appListRefresh = useRecoilRefresher_UNSTABLE(appListQuery(projectId));

  useEffect(() => {
    if (spec) {
      setName(generateDefaultAppName(spec));
    }
  }, [spec]);

  const doLoadSpec = useCallback(() => {
    router.push({
      pathname: router.asPath.split("?")[0],
      query: { spec: pendingSpecUrl },
    }, undefined, { shallow: true });
    return false;
  }, [pendingSpecUrl]);

  useEffect(() => {
    if (!specUrl) {
      setSpec(null);
      return;
    }
    loadJson("/api/app/specproxy?url=" + encodeURIComponent(specUrl))
      .then(x => { setSpec(x); setLastError("") })
      .catch(e => setLastError("" + e));
  }, [specUrl]);

  const doLaunch = useCallback(() => {
    const config = buildConfigRef.current();

    if (typeof config === "string") {
      setLastError(config);
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
        appListRefresh();
        router.push(`/projects/${projectId}/apps/${x.id}`);
        setLastError("");
      })
      .catch(e => setLastError("" + e))
      .finally(() => {
        setLoading(false);
      })
  }, [name, spec, appListRefresh]);

  return (
    <Container xs css={{ pt: 80 }}>
      <BackToAllApps projectId={projectId} />
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
            <Row align="center" css={{ pb: 40 }}>
              {!!spec.icon && <Col css={{ width: "auto" }}><Image width={60} height={60} src={spec.icon} /></Col>}
              <Col css={{ flexGrow: 1, width: "auto", pl: 16 }}>
                <Row><Text size={20} weight="semibold">{spec.name || spec.image}</Text></Row>
                {!!spec.description && <Row><Text size={16}>{spec.description}</Text></Row>}
              </Col>
              <Col css={{ width: "auto", pl: 16 }}>
                <Row>
                  {!!spec.homepage && <Col css={{ pr: 12 }}>
                    <Link href={spec.homepage} target="_blank" title="Homepage">
                      <svg style={{ width: 20 }} xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" className="w-6 h-6">
                        <path strokeLinecap="round" strokeLinejoin="round" d="M2.25 12l8.954-8.955c.44-.439 1.152-.439 1.591 0L21.75 12M4.5 9.75v10.125c0 .621.504 1.125 1.125 1.125H9.75v-4.875c0-.621.504-1.125 1.125-1.125h2.25c.621 0 1.125.504 1.125 1.125V21h4.125c.621 0 1.125-.504 1.125-1.125V9.75M8.25 21h8.25" />
                      </svg>
                    </Link>
                  </Col>}
                  <Col>
                    <Link href={specUrl} target="_blank" title="Spec">
                      <svg style={{ width: 20 }} xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" className="w-6 h-6">
                        <path strokeLinecap="round" strokeLinejoin="round" d="M17.25 6.75L22.5 12l-5.25 5.25m-10.5 0L1.5 12l5.25-5.25m7.5-3l-4.5 16.5" />
                      </svg>
                    </Link>
                  </Col>
                </Row>
              </Col>
            </Row>
            <Row css={{ pb: 40 }}>
              <Input css={{ flexGrow: 1 }} underlined labelPlaceholder="Name" value={name} onChange={e => setName(e.target.value)} />
            </Row>
            <MemoConfigEditor spec={spec} initialConfig={initialConfig} buildConfigRef={buildConfigRef} />
            <Row css={{ pb: 20 }}>
              <Button css={{ flexGrow: 1 }} disabled={loading} onClick={doLaunch}>
                {loading ? <Loading color="currentColor" size="sm" /> : "Launch"}</Button>
            </Row>
            {!!lastError && <Row css={{ pb: 20 }}><Text color="error">{lastError}</Text></Row>}
          </Col>
        }
      </Col>
      <Footer projectId={projectId} userId={userId} />
    </Container>
  )
}

export default dynamic(() => Promise.resolve(Launch), {
  ssr: false
});
