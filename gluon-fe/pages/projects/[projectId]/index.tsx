import type { GetServerSideProps, NextPage } from 'next'

export const getServerSideProps: GetServerSideProps = async (context) => {
  const projectId = context.params?.projectId;
  if (typeof projectId !== "string") {
    return {
      notFound: true,
    };
  }

  return {
    redirect: {
      destination: `/projects/${projectId}/apps`,
      permanent: false,
    }
  };
}

const Dashboard: NextPage<any> = ({ }: { projectId: string }) => {
  return (
    <></>
  )
}

export default Dashboard;
