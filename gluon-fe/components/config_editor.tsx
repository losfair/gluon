import { Row, Input, Col, Text } from "@nextui-org/react";
import React from "react";
import { useEffect, useState } from "react";
import type { AppConfig, AppSpec } from "../models/App";

const ConfigEditor: React.FC<{
  spec: AppSpec,
  initialConfig: AppConfig,
  buildConfigRef: React.MutableRefObject<() => AppConfig | string>,
}> = ({ spec, initialConfig, buildConfigRef }) => {
  const [cpus, setCpus] = useState("" + initialConfig.cpus);
  const [memoryMB, setMemoryMB] = useState("" + initialConfig.memoryMB);
  const [env, setEnv] = useState(() => ({ ...initialConfig.env || {} }));
  useEffect(() => {
    buildConfigRef.current = () => {
      const config: AppConfig = {
        cpus: parseInt(cpus),
        memoryMB: parseInt(memoryMB),
        env,
      };

      if (!Number.isSafeInteger(config.cpus)) {
        return "Invalid CPU count";
      }

      if (!Number.isSafeInteger(config.memoryMB)) {
        return "Invalid memory";
      }
      return config;
    };
  }, [cpus, memoryMB, env]);

  return (<>
    <Row css={{ pb: 40 }}><Text as="h2" size={20} color="primary">Machine</Text></Row>
    <Row css={{ pb: 40 }}>
      <Input css={{ flexGrow: 1 }} underlined labelPlaceholder="CPUs" value={cpus} onChange={e => setCpus(e.target.value)} />
    </Row>
    <Row css={{ pb: 40 }}>
      <Input css={{ flexGrow: 1 }} underlined labelPlaceholder="Memory (MiB)" value={memoryMB} onChange={e => setMemoryMB(e.target.value)} />
    </Row>
    <Row css={{ pb: 20 }}><Text as="h2" size={20} color="primary">App settings</Text></Row>
    <Col>
      {(!spec.env || !Object.keys(spec.env).length) && <Row css={{ pb: 40 }}><Text css={{ color: "gray" }}>No settings</Text></Row>}
      {Object.entries(spec.env || {}).map(([k, v]) => {
        if (v.hidden) return null;
        console.log(v);
        let elem: React.ReactNode;
        const type = v.type || "text";
        const title = v.title || k;
        const hasDefault = typeof v.default === "string";

        switch (type) {
          case "text": {
            elem = <Input
              css={{ flexGrow: 1 }}
              underlined
              label={title}
              placeholder={hasDefault ? v.default! : undefined}
              required={!!v.required}
              value={env[k] || ""}
              onChange={e => setEnv({ ...env, [k]: e.target.value })} />;
            break;
          }
          case "secret": {
            elem = <Input.Password
              css={{ flexGrow: 1 }}
              underlined
              label={title}
              placeholder={hasDefault ? v.default! : undefined}
              required={!!v.required}
              value={env[k] || ""}
              onChange={e => setEnv({ ...env, [k]: e.target.value })} />;
            break;
          }
          default: {
            elem = <Text>{k}: Unknown type {type}</Text>;
          }
        }
        return <Col key={k} css={{ pb: 20 }}>
          <Row>
            {elem}
          </Row>
          {!!v.description && <Row><Text size={14} css={{ color: "gray" }}>{v.description}</Text></Row>}
        </Col>
      })
      }
    </Col>
  </>);
}

export const MemoConfigEditor = React.memo(ConfigEditor);
