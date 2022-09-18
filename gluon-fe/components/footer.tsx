import { Code, Col, Row, Text } from "@nextui-org/react"

export const Footer: React.FC<{ projectId?: string, userId?: string }> = ({ projectId, userId }) => {
  return (
    <Col css={{ pt: 100, pb: 50 }}>
      {!!projectId && <Row><Text css={{ fontFamily: "$mono", color: "gray" }} size="$xs">Project ID: {projectId}</Text></Row>}
      {!!userId && <Row><Text css={{ fontFamily: "$mono", color: "gray" }} size="$xs">User ID: {userId}</Text></Row>}
      <Row><Text css={{ fontFamily: "$mono", color: "gray" }} size="$xs">Gluon</Text></Row>
    </Col>
  )
}
