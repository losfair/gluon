import { Card, Row, Col, Image, Text } from "@nextui-org/react"
import React from "react"
import type { Project } from "../models"
import NextLink from "next/link"
import { InferAttributes } from "sequelize"

export const ProjectCard: React.FC<{ project: InferAttributes<Project> }> = ({ project }) => {
  return (
    <Card css={{ mt: 30, h: 100 }} isHoverable variant="bordered">
      <NextLink href={`/projects/${project.id}/apps`} passHref>
        <a style={{ display: "flex", height: "100%" }}><ProjectCardRow project={project} /></a>
      </NextLink>
    </Card>
  )
}

const ProjectCardRow: React.FC<{ project: InferAttributes<Project> }> = ({ project }) => {
  return (
    <Row
      css={{ align: "center", h: "100%" }}
      align="center">
      <Col css={{ flexGrow: 1, width: "auto", pl: 16 }}>
        <Row><Text size={20} weight="semibold">{project.name}</Text></Row>
        <Row>
          <Text css={{ color: "gray", fontFamily: "$mono", fontWeight: "$semibold" }}>ID: </Text>
          <Text css={{ color: "gray", fontFamily: "$mono", pl: 8 }}>{project.id}</Text>
        </Row>
      </Col>
    </Row>
  );
}