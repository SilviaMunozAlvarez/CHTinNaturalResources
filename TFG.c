#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>

#define NormRANu (2.3283063671E-10F)
#define PI acos(-1.0)

unsigned int irr[256];
unsigned int ir1;
unsigned char ind_ran, ig1, ig2, ig3;

/** VARIABLES **/
#define l_max 2	    //maximum initial cognitive level
#define N 5		    //number of agents   ****
#define p 10        //price of a unit of the resource
//#define alpha 0.27           //number of babies per unit of time, we choose a year
#define optimum 300000       //number of fish per year per agent in an optimal scenario
int U=N*4*optimum;           //carrying capacity, maximum number of specimen. 4:=number of necessary years to extinction
float zeta=1./optimum;       //time that takes to fish a fish in years, we suppose that we fish 1000 (medium fish) per day --> aprox. 300000 per year. To large fish its value decrease
int N_steps=200;            // =200 enough time to achieve stationary dynamic
int period_time;           // here we choose short(2), medium(5) or large(10) period in years.
float c=optimum*p/4;         //cost of a unit of effort  *****

/// DIFFERENT DISTRIBUTIONS ///
//int Levels[N]={0,0,1,1,1,1,1,1,2,2};
//int Rest_2[N-1]={0,0,0,1,1,1,1,1,1};

//int Levels[N]={0,1,1,1,2};
//int Rest_2[N-1]={0,1,1,1};

//int Levels[N]={1,2};
//int Rest_2[N-1]={1};

/** STRUCT **/
typedef struct player{
	int cognit_level;			//cognitive level of the player
	int rest_level[N-1];		//supposed level of the rest of the players
	int Npeople_level[l_max+1];	//number of people with certain cognitive level
    float Effort;               //number between 0 & 1   --> AQUI ES DONDE SE PUEDE INCLUIR EL BEHAVIOUR
    float Revenue;              //obtained revenue per time unit
    float Benefit;              //benefit per unit obtained

}		player;

/** SUBALGORITHMS**/
/**GENERATOR OF RANDOM NUMBERS IN THE INTERVAL [0,1) **/
float Random(void){
    float r;

    ig1=ind_ran-24;
    ig2=ind_ran-55;
    ig3=ind_ran-61;
    irr[ind_ran]=irr[ig1]+irr[ig2];
    ir1=(irr[ind_ran]^irr[ig3]);
    ind_ran++;
    r=ir1*NormRANu;
    return r;
}
void ini_ran(int SEED){
    int INI, FACTOR, SUM, i;
    srand(SEED);

    INI=SEED;
    FACTOR=67397;
    SUM=7364893;

    for(i=0; i<256; i++){
        INI=INI*FACTOR+SUM;
        irr[i]=INI;
        ind_ran=ig1=ig2=ig3=0;
    }
}

/**SUPPOSED COGNITIVE LEVEL OF THE REST OF THE PLAYERS**/
void Supposed_level (player *agent, int i){
	int  l;

	if (agent[i].cognit_level<2)
        for (int j=0; j<N-1; j++){
            agent[i].rest_level[j]=0;
            agent[i].Npeople_level[0]++;
        }
	else
        for (int j=0; j<N-1; j++){
            agent[i].rest_level[j]=agent[i].cognit_level-1;//Rest_2[j];
            l=agent[i].rest_level[j];
            agent[i].Npeople_level[l]+=1;
        }
}

/**DECIDE HOW MUCH HARVEST**/                                        // THIS FUNCTION IS RECURSIVE
float Amount_Harvest(player *agent, int i, float R, int max_l, float alpha){
    float Total_Revenue, max_benefit=0., max_effort=0., R2=R, Benefit_period;          //R2:=supposed changes in R value
    player aux[1]; //We use an auxiliary structure for the recursive function (it will be each of supposed agents with less cognitive level)

    //printf("How much harvest Agent %d:\n",i);

    /**INITIALIZATION OF VALUES**/
        aux[0].cognit_level=0;
        for (int j=0; j<N-1; j++){
            aux[0].rest_level[j]=0;}
        for (int j=0; j<=max_l; j++){
            aux[0].Npeople_level[j]=0;}     // We need to initialize this vector

        //printf("Cognitive Level of %d: %d\n",i, agent[i].cognit_level);
        //printf("CL of aux: %d\n Rest0:%d\n Rest1:%d\n Rest2:%d\n Rest3:%d\n \n \n",aux[0].cognit_level,  aux[0].Npeople_level[0], aux[0].Npeople_level[1], aux[0].Npeople_level[2], aux[0].Npeople_level[3]);

    for (float eff=0.01; eff<1.0; eff+=0.01){                                        //eff= effort of agent i
        Benefit_period=0.;
        R2=R;
        for(int t=0; t<=period_time; t++){      // We calculate the benefit get in a specific period time in function of the effort done by an agent and his assumptions
            //printf("Tiempo: %d\n",t);
            Total_Revenue=eff*R2/(U*zeta);                                  // His own revenue
            Total_Revenue+=agent[i].Npeople_level[0]*0.5*R2/(U*zeta);       //supposed actions of the players with cognitive level 0

            if (agent[i].cognit_level>=2){
                 for(int k=1; k<max_l; k++){                                // This loop is useful only if the maximum cognitive level is > 2
                        //printf("k: %d\n",k);
                    for (int j=0; j<agent[i].Npeople_level[k]; j++){        // Loop to sum the supposed revenue of the rest of the agents with cognitive level >= 1
                        //printf("j: %d\n",j);
                        aux[0].cognit_level=k;
                        Supposed_level(aux, 0);
                        Total_Revenue+=Amount_Harvest(aux, 0, R2, k, alpha)*R2/(U*zeta);
                    }
                 }
            }

        Benefit_period+=p*eff*R2/(U*zeta)-c*eff;
        R2+=alpha*R2*(1.-R2/U)-Total_Revenue;
        }
        //printf(" Effort:%.2f\n Beneficio:%.2f \n R2=%.2f \n",eff, Benefit_period,R2);

        if (Benefit_period>max_benefit){                                    // We save the optimum effort to achieve the maximum benefit
            max_benefit=Benefit_period;
            max_effort=eff;
        }
    }
    return max_effort;
}


int main(){

for(int simu=0; simu<50; simu++)           // For done several simulations, specially for the calculus of average variables
{
    //printf("Simulacion: %d\n",simu);

    ini_ran(time(NULL));	                 //initial value of the seed.
    player Agent[N];
    float R=U*1., Total_Revenue, Total_Benefit, Effort, alpha, Mean_R, Mean_B;
    FILE *F;
    char FileName[64];
    float Vect_alphas[4]={0.1,0.15,0.25,0.4};
    float Years[5]={1,3,5,8,10};

/**INITIALIZATION AT ZERO OF VALUES**/{
	for (int i=0; i<N; i++){
		Agent[i].cognit_level=Agent[i].Revenue=Agent[i].Benefit=Agent[i].Effort=0.;
		if(i<N-1)
            Agent[i].rest_level[i]=0;
		for (int j=0; j<=l_max; j++){
			Agent[i].Npeople_level[j]=0;
		}
	}
}

/**INITIALIZATION OF VARIABLES**/{
    for (int i=0; i<N; i++){
		Agent[i].cognit_level=0;//Levels[i];
        Supposed_level(Agent, i);
    }
}

/**PRUEBA REPARTO NIVELES COGNITIVOS (NUEVO)**/{
/*for(int i=0; i<N; i++){
    printf("Nivel cognitivo del agente %d: %d\n", i, Agent[i].cognit_level);
    printf("\t Supuesto nivel cognitivo del resto:\n");
    for (int j=0; j<N-1; j++){
        printf("\t %d\n",Agent[i].rest_level[j]);
    }
}*/}

/**GRAPHIC: RESOURCE IN FUNTION OF TIME FOR CERTAIN EFFORT**/{
/*for(float alpha=0.05; alpha<0.48; alpha+=0.1){                                   //only for knowing the optimal value of alpha
    printf("Alpha: %.2f\n", alpha);
    for (float k=0.00; k<1.0; k+=0.01){
            printf("k=%.2f\n",k);
        for(int i=0; i<N; i++){
            Agent[i].Effort=k;
        }
        R=U*1.;

        for (int t=0; t<N_steps; t++){
            Total_Revenue=0.0;
            Total_Benefit=0.0;
            if (R<0.){
                    printf("Tiempo: %d \n", t);
                return 0;
            }

            for(int i=0; i<N; i++){
                Agent[i].Revenue=Agent[i].Effort*R/(U*zeta);
                Agent[i].Benefit=p*Agent[i].Revenue-c*Agent[i].Effort;
                Total_Revenue+=Agent[i].Revenue;
                Total_Benefit+=Agent[i].Benefit;
                //printf("Effort: %.2f \t Revenue: %.2f \t Benefit: %.2f \n", Agent[i].Effort, Agent[i].Revenue, Agent[i].Benefit);
            }
            sprintf(FileName, "Benefit&Resource_alpha%.2f.txt",alpha);
            F=fopen(FileName,"a");
            fprintf(F," %d %.2f %.2f %.2f %.2f\n",t,R,Total_Benefit,k*100,alpha);
            fclose(F);

            R+=alpha*R*(1.-R/U)-Total_Revenue;
            Total_Benefit=Total_Benefit/N;
        }
                 //for knowing optimal alpha
        sprintf(FileName, "BenefitVsEffort_alpha%.2f+R.txt",alpha);
        F=fopen(FileName,"a");
        fprintf(F,"%.2f %.2f %.2f %.2f\n",k*100,Total_Benefit, alpha, R);
        fclose(F);

    }
}                                */
}

//for(int i=0; i<N; i++){ printf("Agente: %d\n Level:%d\n Rest0:%d\n Rest1:%d\n Rest2:%d\n Rest3:%d\n Rest4:%d\n \n \n", i, Agent[i].cognit_level, Agent[i].Npeople_level[0],Agent[i].Npeople_level[1],Agent[i].Npeople_level[2],Agent[i].Npeople_level[3],Agent[i].Npeople_level[4]);   }

/**DYNAMIC**/{
//for(period_time=5; period_time<11; period_time+=4)
period_time=8;
//for (int k=0; k<5; k++)
    {
    //period_time=Years[k];
    //for(int j=0; j<4; j++){ alpha=Vect_alphas[j];
        alpha=0.15;
        R=U*1.;
        ///For average calculus
        Mean_B=Effort=Mean_R=0.;

        for (int t=0; t<N_steps; t++){
            //if(t%20==0){
            printf("Time: %d\n",t);//}
            Total_Revenue=0.0;
            ///For graphics in function of time
            //Total_Benefit=Effort=0.;
            if (R<0.){
                    printf("Tiempo: %d \n", t);
                return 0;
            }
            for(int i=0; i<N; i++){
                if(Agent[i].cognit_level==0)
                    Agent[i].Effort=Random();
                else{
                    Agent[i].Effort=Amount_Harvest(Agent, i, R, Agent[i].cognit_level, alpha);}
                //printf("Agente: %d , Level: %d --> Effort: %.2f \n",i,Agent[i].cognit_level, Agent[i].Effort);

                Agent[i].Revenue=Agent[i].Effort*R/(U*zeta);
                Agent[i].Benefit=p*Agent[i].Revenue-c*Agent[i].Effort;
                Total_Revenue+=Agent[i].Revenue;
                ///For graphics in function of time
                /*Total_Benefit+=Agent[i].Benefit;
                Effort+=Agent[i].Effort;*/
                //printf("Effort: %.2f  Benefit: %.2f  R: %.2f Mean_R: %.2f\n", Agent[i].Effort, Agent[i].Benefit, R,Mean_R);

                ///For average calculus
                if(t>99){
                    Mean_B+=Agent[i].Benefit;
                    Effort+=Agent[i].Effort;
                    Mean_R+=R;
                    //printf("Effort: %.2f  Benefit: %.2f  R: %.2f Mean_R: %.2f\n", Agent[i].Effort, Agent[i].Benefit, R,Mean_R);
                }
            }
            ///For graphics in function of time
            //sprintf(FileName, "DynamicOf2_Alpha=%.2f_Years=%d_N=%d.txt",alpha,period_time,N);
            /*F=fopen("Dynamic.txt","a");
            fprintf(F,"%d %f Esfuerzo %d %d %d\n%d %f Recurso %d %d %d\n%d %f Beneficio %d %d %d\n",t,Effort/(N*1.),period_time,N,Agent[0].cognit_level,t,R/(U*1.),period_time,N,Agent[0].cognit_level,t,Total_Benefit/optimum/N*5,period_time,N,Agent[0].cognit_level); //Normalized Resource and benefit
            fclose(F);*/
            //printf("%d %f %f %.2f %d %d %d %.4f\n",t,R/(U*1.),Total_Benefit/optimun/N*5,alpha,period_time,N,0, Effort/(N*1.)); //Normalized Resource and benefit

            /*sprintf(FileName, "Effort_Years=%d_N=%d_Level=%d.txt",period_time,N,Agent[0].cognit_level);
            F=fopen(FileName,"a");
            fprintf(F,"%d %f Esfuerzo %d %d %d\n",t,Effort/N*100,period_time,N,Agent[0].cognit_level); //Normalized Resource and benefit
            fclose(F);

            sprintf(FileName, "Resource_Years=%d_N=%d_Level=%d.txt",period_time,N,Agent[0].cognit_level);
            F=fopen(FileName,"a");
            fprintf(F,"%d %f Recurso %d %d %d\n",t,R/(U*1.)*100,period_time,N,Agent[0].cognit_level); //Normalized Resource and benefit
            fclose(F);

            sprintf(FileName, "Benefit_Years=%d_N=%d_Level=%d.txt",period_time,N,Agent[0].cognit_level);
            F=fopen(FileName,"a");
            fprintf(F,"%d %f Beneficio %d %d %d\n",t,Total_Benefit*5/(optimum),period_time,N,Agent[0].cognit_level); //Normalized Resource and benefit
            fclose(F);*/

            R+=alpha*R*(1.-R/U)-Total_Revenue;
        }

        ///For average calculus
        //printf("%d %.2f %.2f %.2f %d\n",Levels[0],Effort,Mean_B, MEan_R,U);
        //printf("%d %.2f %.2f %.2f\n",Levels[0],Effort/1000,Total_Benefit/(1000000*1000), Mean_R/(U*1000.0));

        ///Extremos
        sprintf(FileName, "Promedios_%d_Ext_N=%d.txt",period_time,N);
        F=fopen(FileName,"a");
        fprintf(F,"%d %.2f %f %f %d %d\n",Agent[0].cognit_level,Effort/(N*(N_steps-100)), Mean_B/(N*5*optimum), Mean_R/(1.0*U*N*(N_steps-100)), period_time, N);
        fclose(F);
        ///Intermedios
        /*sprintf(FileName, "Promedios_%d_Int_N=%d.txt",period_time,N);
        F=fopen(FileName,"a");
        fprintf(F,"%d %.2f %f %f %d %d\n",3,Effort/(N*(N_steps-100)),Mean_B/(N*5*optimum), Mean_R/(1.0*U*N*(N_steps-100)), period_time, N);
        fclose(F);*/
    //}
}

}
}
}
