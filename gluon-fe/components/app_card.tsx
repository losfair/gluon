import { Card, Row, Col, Image, Text } from "@nextui-org/react"
import React from "react"
import type { App } from "../models"
import NextLink from "next/link"
import { AppInfo } from "../service/api_types"

export const AppCard: React.FC<{ app: AppInfo, inSingleAppPage?: boolean }> = ({ app, inSingleAppPage }) => {
  return (
    <Card key={app.id} css={{ mt: 30, h: 100 }} isHoverable={!inSingleAppPage} variant="bordered">
      {!!inSingleAppPage ?
        <AppCardRow app={app} inSingleAppPage /> :
        <NextLink href={`/projects/${app.projectId}/apps/${app.id}`} passHref>
          <a style={{ display: "flex", height: "100%" }}><AppCardRow app={app} /></a>
        </NextLink>}
    </Card>
  )
}

const AppCardRow: React.FC<{ app: AppInfo, inSingleAppPage?: boolean }> = ({ app, inSingleAppPage }) => {
  return (
    <Row
      key={app.id}
      css={{ align: "center", h: "100%" }}
      align="center">
      {!!app.spec.icon && <Col css={{ width: "auto", pl: 16 }}><Image width={60} height={60} src={app.spec.icon} /></Col>}
      <Col css={{ flexGrow: 1, width: "auto", pl: 16 }}>
        <Row><Text size={20} weight="semibold">{app.spec.name || app.spec.image}</Text></Row>
        <Row>
          <Text css={{ color: "gray", fontFamily: "$mono", fontWeight: "$semibold" }}>ID: </Text>
          <Text css={{ color: "gray", fontFamily: "$mono", pl: 8 }}>{app.id}</Text>
          <Text css={{ color: "gray", fontFamily: "$mono", fontWeight: "$semibold", pl: 24 }}>Name: </Text>
          <Text css={{ color: "gray", fontFamily: "$mono", pl: 8 }}>{app.name}</Text>
        </Row>
      </Col>
    </Row>
  );
}