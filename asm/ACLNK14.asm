*          DATA SET ACLNK14    AT LEVEL 008 AS OF 05/28/09                      
*PHASE T61F14A                                                                  
ACLNK14  TITLE '- SCRIBE DOWNLOAD'                                              
         PRINT NOGEN                                                            
SVRDEF   LKSVR TYPE=D,IDF=Y,CODE=CODE,FILES=FILES,REQUEST=*,           *        
               SERVERTYPE=TSTASCR,ABENDLIST=FAILS,SLOWLIST=SLOWS,      *        
               WORKERKEY=ACRL,EODID=RU,SEGMENT=Y,APPEND=Y,LINKIO=Y,    *        
               SYSTEM=ACCSYSQ,BLOCKS=(B#SAVED,SAVED),SYSPHASE=SYSPHASE          
*                                                                               
CODE     NMOD1 0,**AL14**,RR=RE                                                 
         LR    R5,R1                                                            
         USING LP_D,R5             R5=A(LP_D)                                   
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
*                                                                               
         MVI   LP_AIND1,LP_AICOM+LP_AIUPL+LP_AIRLP                              
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BNZ   INIT02              YES                                          
*                                                                               
         L     R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     R8,LP_ABLK2                                                      
         L     RA,ATWA                                                          
         B     INIT04                                                           
*                                                                               
INIT02   L     R9,RSVRSAVE                                                      
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         SR    R8,R8                                                            
         ICM   R8,3,=AL2(WORKL)                                                 
         LA    R8,WORKD(R8)                                                     
         AHI   R8,7                                                             
         SRL   R8,3                                                             
         SLL   R8,3                                                             
         USING SAVED,R8            R8=A(SAVE W/S)                               
         ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
         L     RA,LP_ATWA                                                       
         ST    RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
         USING ACQD,REQCARDS                                                    
*                                                                               
INIT04   ST    R5,ALP              SAVE A(LP_D)                                 
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FOR RUNNING                                              *         
***********************************************************************         
         SPACE 1                                                                
RUNSTR   CLI   RUNPMODE,RRUNSTRQ   TEST 'FIRST FOR RUN' MODE                    
         BNE   PRCWRK                                                           
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    RUNSTRX                                                          
*                                                                               
         L     RF,RCOMFACS         LOAD FACILITIES OVERLAYS                     
         ST    RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('P#ROUTS1',0),0,0                                     
         MVC   AROUTS1,0(R1)                                                    
         MVC   LP_AUIR1,AROUTS1    SET A(INDEX ROUTINES 1)                      
         GOTOR (RF),DMCB,('P#ROUTS2',0),0,0                                     
         MVC   AROUTS2,0(R1)                                                    
         MVC   LP_AUIR2,AROUTS2    SET A(INDEX ROUTINES 2)                      
*                                                                               
         GOTOR (#WRKINI,AWRKINI)   INITIALIZE WORKING STORAGE                   
*                                                                               
         GOTOR RGETPHS,DMCB,0,(C'E',MONACCLA)                                   
         BE    RUNSTR10                                                         
         GOTOR RGETPHS,DMCB,0,(C'E',MONACCL)                                    
RUNSTR10 MVC   AMONACCL,0(R1)                                                   
*                                                                               
         GOTOR AMONACCL,DMCB,(X'DD',LP_D),0                                     
*                                                                               
RUNSTRX  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
         SPACE 1                                                                
PRCWRK   CLI   RUNPMODE,RPRCWRKQ   TEST 'PROCESS WORK' MODE                     
         BNE   RUNREQ                                                           
         XC    STRCALM(L'STRCALM+L'ENDCALM),STRCALM                             
         LA    R0,ACQD             SET REQUEST CARDS TO SPACES                  
         LHI   R1,ACQL                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,8,SPACES                                                      
         MVCL  R0,RE                                                            
*                                                                               
         TM    LP_FLAG,LP_FBTCH    TEST OFFLINE BATCH MODE                      
         JZ    EXITY                                                            
         GOTOR (#GETCPY,AGETCPY)   INITIALIZE SOFT DATE FIELDS                  
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* RUN A WORK REQUEST                                                  *         
***********************************************************************         
         SPACE 1                                                                
RUNREQ   CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         JNE   EXITY                                                            
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCMST,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCARC,(4,0),0                               
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,7,AACT                                                        
         BZ    RUNREQ02                                                         
         USING LW_D,RE                                                          
         CLI   LW_TYPE,LW_TSINQ    IS THIS A SINGLE ENTRY?                      
         BNE   *+14                                                             
         MVC   ACQUNT(L'ACQUNT+L'ACQLDG+L'ACQACT),LW_DATA1                      
         B     RUNREQ02                                                         
         MVI   ACQACT,FF           SIGNIFY THAT IT'S A LIST                     
         LA    RE,LW_DATA1-LW_D(RE)        POINT TO # OF ENTRIES                
         STCM  RE,7,ACQACT+1                                                    
         DROP  RE                                                               
*                                                                               
RUNREQ02 SR    RE,RE                                                            
         ICM   RE,7,AEXACT                                                      
         BZ    RUNREQ04                                                         
         USING LW_D,RE                                                          
         CLI   LW_TYPE,LW_TSINQ    IS THIS A SINGLE ENTRY?                      
         BNE   *+14                                                             
         MVC   ACQUNT(L'ACQUNT+L'ACQLDG+L'ACQACT),LW_DATA1                      
         B     RUNREQ04                                                         
         MVI   ACQACT,FF           SIGNIFY THAT IT'S A LIST                     
         LA    RE,LW_DATA1-LW_D(RE)        POINT TO # OF ENTRIES                
         STCM  RE,7,ACQACT+1                                                    
         ICM   R0,3,0(RE)                                                       
         AHI   RE,2                BUMP PAST NUMBER OF ENTRIES TO DATA          
         NI    2(RE),X'FF'-X'40'   TURN OFF X'40' OF 1ST BYTE OF ACCT           
         AHI   RE,L'ACQUNT+L'ACQLDG+L'ACQACT                                    
         BCT   R0,*-8                                                           
         DROP  RE                                                               
*                                                                               
RUNREQ04 OC    STRCALM(L'STRCALM+L'ENDCALM),STRCALM                             
         BZ    RUNREQ06                                                         
         CLC   ACQSTART(L'ACQSTART+L'ACQEND),SPACES                             
         BNE   RUNREQ06                                                         
         MVC   ACQSTART(L'STRCALM),STRCALM                                      
         MVC   ACQEND(L'ENDCALM),ENDCALM                                        
*                                                                               
RUNREQ06 LA    R0,DTETABQ          CHECK FOR VALID DATES                        
         LA    R2,DTETAB                                                        
         SR    R1,R1                                                            
         SR    RF,RF                                                            
RUNREQ08 LA    RE,REQCARDS                                                      
         IC    RF,0(R2)            ADD IN DISPLACEMENT                          
         AR    RE,RF                                                            
         IC    R1,1(R2)            GET LENGTH                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,RE),0(RE)                                                    
         BZ    RUNREQ10                                                         
         EX    R1,*+8                                                           
         BNE   RUNREQ12                                                         
         CLC   0(0,RE),EFFS                                                     
RUNREQ10 EX    R1,*+8                                                           
         B     RUNREQ12                                                         
         MVC   0(0,RE),SPACES                                                   
RUNREQ12 AHI   R2,L'DTETAB                                                      
         BCT   R0,RUNREQ08                                                      
*                                                                               
         LHI   R0,(ACQL/L'ACQCARD1)-1                                           
         LA    RE,ACQD+ACQL-(L'ACQCARD1*2)                                      
         LA    RF,ACQD+ACQL-(L'ACQCARD1)                                        
RUNREQ14 CLC   0(L'ACQCARD1,RF),SPACES                                          
         BNE   RUNREQ16                                                         
         SHI   RE,L'ACQCARD1                                                    
         SHI   RF,L'ACQCARD1                                                    
         BCT   R0,RUNREQ14                                                      
         LHI   RF,1                ONLY ONE REQUEST CARD                        
         B     RUNREQ18                                                         
*                                                                               
RUNREQ16 LR    RF,R0               SET CONTINUATION COLUMNS                     
         AHI   RF,1                RF=NUMBER OF REQUEST CARDS                   
RUNREQ18 LA    R1,ACQCONT2-ACQCARD2(RE)                                         
         CHI   R0,1                                                             
         BNE   *+8                                                              
         LA    R1,ACQCONT1-ACQCARD1(RE)                                         
         MVI   0(R1),ACQCONTQ                                                   
         BE    RUNREQ20                                                         
         SHI   RE,L'ACQCARD1                                                    
         BCT   R0,RUNREQ18                                                      
                                                                                
RUNREQ20 LR    R0,RF               R0=NUMBER OF REQUEST CARDS                   
*                                                                               
         L     RF,RMASTC           CALL MONACCL PASSING REQUEST                 
         MVC   ACQPROG,MCPROG-MASTD(RF)                                         
         GOTOR AMONACCL,DMCB,(X'DD',LP_D),((R0),ACQD)                           
*                                                                               
         J     EXITY                                                            
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACCOUNT/CONTRA-ACCOUNT FILTER VALUE                        *         
***********************************************************************         
         SPACE 1                                                                
VALFLT   LM    R2,R4,LP_AINP                                                    
         MVC   0(1,R4),0(R2)                                                    
         CHI   R3,1                                                             
         BER   RE                                                               
         CLI   0(R2),C'*'                                                       
         JNE   EXITN                                                            
         MVC   0(1,R4),1(R2)                                                    
         NI    0(R4),FF-C' '                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITN    LHI   RE,0                                                             
         J     EXITCC                                                           
EXITY    LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
*&&US                                                                           
MONACCLA DC    C'MONACCLA'                                                      
MONACCL  DC    C'MONACCL '                                                      
*&&                                                                             
*&&UK                                                                           
MONACCL  DC    C'MONACCR '                                                      
*&&                                                                             
DMKEY    DC    C'DMKEY   '                                                      
*                                                                               
MQHVLIT  DC    C'MQ Header'                                                     
MQKVLIT  DC    C'MQ Key'                                                        
MQQVLIT  DC    C'MQ Queue'                                                      
*                                                                               
DTETAB   DS    0CL2                                                             
         DC    AL1(ACQSTART-ACQD,L'ACQSTART-1)   ACQSTART W/ DAY                
         DC    AL1(ACQEND-ACQD,L'ACQEND-1)       ACQEND W/ DAY                  
         DC    AL1(ACQSTART-ACQD,L'ACQMOSST-1)   ACQSTART W/O DAY               
         DC    AL1(ACQEND-ACQD,L'ACQMOSND-1)     ACQEND W/O DAY                 
         DC    AL1(ACQMOSST-ACQD,L'ACQMOSST-1)   MOS START                      
         DC    AL1(ACQMOSND-ACQD,L'ACQMOSND-1)   MOS END                        
         DC    AL1(ACQTYP1+(ACQDTSTR-ACQTYP1)-ACQD,L'ACQDTSTR-1)                
         DC    AL1(ACQTYP1+(ACQDTEND-ACQTYP1)-ACQD,L'ACQDTEND-1)                
         DC    AL1(ACQTYP2+(ACQDTSTR-ACQTYP1)-ACQD,L'ACQDTSTR-1)                
         DC    AL1(ACQTYP2+(ACQDTEND-ACQTYP1)-ACQD,L'ACQDTEND-1)                
         DC    AL1(ACQTYP3+(ACQDTSTR-ACQTYP1)-ACQD,L'ACQDTSTR-1)                
         DC    AL1(ACQTYP3+(ACQDTEND-ACQTYP1)-ACQD,L'ACQDTEND-1)                
         DC    AL1(ACQTYP4+(ACQDTSTR-ACQTYP1)-ACQD,L'ACQDTSTR-1)                
         DC    AL1(ACQTYP4+(ACQDTEND-ACQTYP1)-ACQD,L'ACQDTEND-1)                
         DC    AL1(ACQTYP5+(ACQDTSTR-ACQTYP1)-ACQD,L'ACQDTSTR-1)                
         DC    AL1(ACQTYP5+(ACQDTEND-ACQTYP1)-ACQD,L'ACQDTEND-1)                
         DC    AL1(ACQTYP6+(ACQDTSTR-ACQTYP1)-ACQD,L'ACQDTSTR-1)                
         DC    AL1(ACQTYP6+(ACQDTEND-ACQTYP1)-ACQD,L'ACQDTEND-1)                
         DC    AL1(ACQTYP7+(ACQDTSTR-ACQTYP1)-ACQD,L'ACQDTSTR-1)                
         DC    AL1(ACQTYP7+(ACQDTEND-ACQTYP1)-ACQD,L'ACQDTEND-1)                
         DC    AL1(ACQTYP8+(ACQDTSTR-ACQTYP1)-ACQD,L'ACQDTSTR-1)                
         DC    AL1(ACQTYP8+(ACQDTEND-ACQTYP1)-ACQD,L'ACQDTEND-1)                
DTETABQ  EQU   (*-DTETAB)/L'DTETAB                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SYSTEMS FILE LIST                                                   *         
***********************************************************************         
         SPACE 1                                                                
FILES    DS    0C                  ** SYSTEM/FILE LIST **                       
         DC    C'ACCOUNT'                                                       
         DC    C'N'                                                             
ACCDIR   DC    C'ACCDIR '                                                       
         DC    C'N'                                                             
ACCMST   DC    C'ACCMST '                                                       
         DC    C'N'                                                             
ACCARC   DC    C'ACCARC '                                                       
         DC    C'NCTFILE '                                                      
         DC    C'NGENDIR '                                                      
         DC    C'NGENFIL '                                                      
         DC    C'X'                                                             
         EJECT                                                                  
***********************************************************************         
* PROGRAM ABEND NOTIFICATION                                          *         
***********************************************************************         
         SPACE 1                                                                
*&&US                                                                           
FAILS    DC    C'JSHA,RGUP:'                                                    
SLOWS    DC    C'JSHA,RGUP:'                                                    
*&&                                                                             
*&&UK                                                                           
FAILS    DC    C'JFOSDDLO,NSHEDDLO,PMARDDLO:'                                   
SLOWS    DC    C'JFOSDDLO,NSHEDDLO,PMARDDLO:'                                   
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* REQUEST MAPS                                                        *         
***********************************************************************         
         SPACE 1                                                                
REQACC   LKREQ H,I#SCRIBE,OUTACC                                                
*                                                                               
Foldr    LKREQ F,255,(D,B#SAVED,DUMMY),FLDR,TEXT=AC#GROUP,COL=*                 
Reqst    LKREQ F,254,(D,B#SAVED,DUMMY),RQST,TEXT=AC#REQ,COL=*                   
RDesc    LKREQ F,253,(D,B#SAVED,DUMMY),RDSC,TEXT=AC#DESC,LOWERCASE=Y,  +        
               COL=*                                                            
RInfo    LKREQ F,252,(D,B#SAVED,DUMMY),RINF,TEXT=AC#OTHRI,LOWERCASE=Y, +        
               COL=*                                                            
MqHdr    LKREQ F,251,(D,B#SAVED,DUMMY),MQHV,TEXT=(*,MQHVLIT),COL=*              
MqKey    LKREQ F,250,(D,B#SAVED,DUMMY),MQKV,TEXT=(*,MQKVLIT),COL=*              
MqQue    LKREQ F,249,(D,B#SAVED,DUMMY),MQQV,TEXT=(*,MQQVLIT),COL=*              
Methd    LKREQ F,1,(D,B#SAVED,REQCARDS+(ACQMTHD-ACQD)),CHAR,           +        
               OLEN=L'ACQMTHD,TEXT=AC#METH,COL=*                                
DtRng    LKREQ F,2,(D,B#SAVED,REQCARDS+(ACQSTART-ACQD)),EDAT,RANGE=Y,  +        
               OLEN=L'ACQSTART,TEXT=AC#DTRNG,COL=*                              
DtRwo    LKREQ F,3,(D,B#SAVED,STRCALM),EDYM,RANGE=Y,                   +        
               OLEN=L'STRCALM,TEXT=AC#DRWOD,COL=*                               
IncAL    LKREQ F,4,(I,B#SAVED,ACTIND),CHAR,LIST=Y,SORT=N,              +        
               OLEN=L'ACQUNT+L'ACQLDG+L'ACQACT,TEXT=AC#UNLDA,COL=*              
ExcAL    LKREQ F,9,(I,B#SAVED,EXCIND),CHAR,LIST=Y,SORT=N,              +        
               OLEN=L'ACQUNT+L'ACQLDG+L'ACQACT,TEXT=AC#EXCL,COL=*               
Frmat    LKREQ F,5,(D,B#SAVED,REQCARDS+(ACQAPPL-ACQD)),CHAR,           +        
               OLEN=L'ACQAPPL,TEXT=AC#FRMAT,COL=*                               
TType    LKREQ F,6,(D,B#SAVED,REQCARDS+(ACQTTYPE-ACQD)),CHAR,          +        
               OLEN=L'ACQTTYPE,TEXT=AC#TRNTP,COL=*                              
MoaRg    LKREQ F,7,(D,B#SAVED,REQCARDS+(ACQMOSST-ACQD)),EDYM,RANGE=Y,  +        
               OLEN=L'ACQMOSST,TEXT=AC#MOARA,COL=*                              
Opt#1    LKREQ F,10,(D,B#SAVED,REQCARDS+(ACQOPT1-ACQD)),CHAR,          +        
               OLEN=L'ACQOPT1,TEXT=AC#DL,COL=*                                  
Opt#2    LKREQ F,11,(D,B#SAVED,REQCARDS+(ACQOPT2-ACQD)),CHAR,          +        
               OLEN=L'ACQOPT2,TEXT=AC#SCRTY,COL=*                               
Opt#3    LKREQ F,12,(D,B#SAVED,REQCARDS+(ACQOPT3-ACQD)),CHAR,          +        
               OLEN=L'ACQOPT3,TEXT=AC#OPT#3,COL=*                               
Opt#4    LKREQ F,13,(D,B#SAVED,REQCARDS+(ACQOPT4-ACQD)),CHAR,          +        
               OLEN=L'ACQOPT4,TEXT=AC#OPT#4,COL=*                               
Opt#5    LKREQ F,14,(D,B#SAVED,REQCARDS+(ACQOPT5-ACQD)),CHAR,          +        
               OLEN=L'ACQOPT5,TEXT=AC#OPT#5,COL=*                               
Opt#6    LKREQ F,15,(D,B#SAVED,REQCARDS+(ACQOPT6-ACQD)),CHAR,          +        
               OLEN=L'ACQOPT6,TEXT=AC#OPT#6,COL=*                               
Opt#7    LKREQ F,16,(D,B#SAVED,REQCARDS+(ACQOPT7-ACQD)),CHAR,          +        
               OLEN=L'ACQOPT7,TEXT=AC#OPT#7,COL=*                               
Opt#8    LKREQ F,17,(D,B#SAVED,REQCARDS+(ACQOPT8-ACQD)),CHAR,          +        
               OLEN=L'ACQOPT8,TEXT=AC#OPT#8,COL=*                               
Opt#9    LKREQ F,18,(D,B#SAVED,REQCARDS+(ACQOPT9-ACQD)),CHAR,          +        
               OLEN=L'ACQOPT9,TEXT=AC#OPT#9,COL=*                               
Opt#A    LKREQ F,19,(D,B#SAVED,REQCARDS+(ACQOPT10-ACQD)),CHAR,         +        
               OLEN=L'ACQOPT10,TEXT=AC#OPT#A,COL=*                              
BilGp    LKREQ F,20,(D,B#SAVED,REQCARDS+(ACQBILGP-ACQD)),CHAR,         +        
               OLEN=L'ACQBILGP,TEXT=AC#BLGGP,COL=*                              
OffGp    LKREQ F,21,(D,B#SAVED,REQCARDS+(ACQOFGRP-ACQD)),CHAR,         +        
               OLEN=L'ACQOFGRP,TEXT=AC#OFFGP,COL=*                              
MedGp    LKREQ F,22,(D,B#SAVED,REQCARDS+(ACQMEDGP-ACQD)),CHAR,         +        
               OLEN=L'ACQMEDGP,TEXT=AC#MEDGP,COL=*                              
MedFl    LKREQ F,23,(D,B#SAVED,REQCARDS+(ACQMEDFL-ACQD)),CHAR,         +        
               OLEN=L'ACQMEDFL,TEXT=AC#MEDFL,COL=*                              
WCoGp    LKREQ F,24,(D,B#SAVED,REQCARDS+(ACQWCGRP-ACQD)),CHAR,         +        
               OLEN=L'ACQWCGRP,TEXT=AC#WCGRP,COL=*                              
TskWC    LKREQ F,25,(D,B#SAVED,REQCARDS+(ACQWRKLS-ACQD)),CHAR,         +        
               OLEN=L'ACQWRKLS,TEXT=AC#TASK,COL=*                               
BlgTy    LKREQ F,26,(D,B#SAVED,REQCARDS+(ACQBILTY-ACQD)),CHAR,         +        
               OLEN=L'ACQBILTY,TEXT=AC#BLGTY,COL=*                              
StuTy    LKREQ F,27,(D,B#SAVED,REQCARDS+(ACQSTUTY-ACQD)),CHAR,         +        
               OLEN=L'ACQSTUTY,TEXT=AC#STUTY,COL=*                              
UsrFl    LKREQ F,28,(D,B#SAVED,REQCARDS+(ACQUSFLD-ACQD)),CHAR,         +        
               OLEN=L'ACQUSFLD,TEXT=AC#RSUSF,COL=*                              
Authn    LKREQ F,29,(D,B#SAVED,REQCARDS+(ACQTYP8-ACQD)),CHAR,          +        
               OLEN=L'ACQTYP8+L'ACQFLT8,TEXT=AC#ATHN,COL=*                      
AFlt1    LKREQ F,30,(D,B#SAVED,REQCARDS+(ACQACTF1-ACQD)),(R,VALFLT),   +        
               OLEN=L'ACQACTF1,TEXT=AC#AFM9,COL=*                               
AFlt2    LKREQ F,31,(D,B#SAVED,REQCARDS+(ACQACTF2-ACQD)),(R,VALFLT),   +        
               OLEN=L'ACQACTF2,TEXT=AC#AFM10,COL=*                              
AFlt3    LKREQ F,32,(D,B#SAVED,REQCARDS+(ACQACTF3-ACQD)),(R,VALFLT),   +        
               OLEN=L'ACQACTF3,TEXT=AC#AFM11,COL=*                              
AFlt4    LKREQ F,33,(D,B#SAVED,REQCARDS+(ACQACTF4-ACQD)),(R,VALFLT),   +        
               OLEN=L'ACQACTF4,TEXT=AC#AFM12,COL=*                              
AFlt5    LKREQ F,34,(D,B#SAVED,REQCARDS+(ACQACTF5-ACQD)),(R,VALFLT),   +        
               OLEN=L'ACQACTF5,TEXT=AC#AFM13,COL=*                              
CFlt1    LKREQ F,35,(D,B#SAVED,REQCARDS+(ACQCFLT1-ACQD)),(R,VALFLT),   +        
               OLEN=L'ACQCFLT1,TEXT=AC#CTRF1,COL=*                              
CFlt2    LKREQ F,36,(D,B#SAVED,REQCARDS+(ACQCFLT2-ACQD)),(R,VALFLT),   +        
               OLEN=L'ACQCFLT2,TEXT=AC#CTRF2,COL=*                              
CFlt3    LKREQ F,37,(D,B#SAVED,REQCARDS+(ACQCFLT3-ACQD)),(R,VALFLT),   +        
               OLEN=L'ACQCFLT3,TEXT=AC#CTRF3,COL=*                              
CFlt4    LKREQ F,38,(D,B#SAVED,REQCARDS+(ACQCFLT4-ACQD)),(R,VALFLT),   +        
               OLEN=L'ACQCFLT4,TEXT=AC#CTRF4,COL=*                              
CFlt5    LKREQ F,39,(D,B#SAVED,REQCARDS+(ACQCFLT5-ACQD)),(R,VALFLT),   +        
               OLEN=L'ACQCFLT5,TEXT=AC#CTRF5,COL=*                              
Offic    LKREQ F,40,(D,B#SAVED,REQCARDS+(ACQOFFFL-ACQD)),CHAR,         +        
               OLEN=L'ACQOFFFL,TEXT=AC#OFFC,COL=*                               
AnOff    LKREQ F,41,(D,B#SAVED,REQCARDS+(ACQANOF-ACQD)),CHAR,          +        
               OLEN=L'ACQANOF,TEXT=AC#ANOFF,COL=*                               
LocSt    LKREQ F,42,(D,B#SAVED,REQCARDS+(ACQLOCS-ACQD)),CHAR,          +        
               OLEN=L'ACQLOCS,TEXT=AC#RSLST,COL=*                               
Recon    LKREQ F,44,(D,B#SAVED,REQCARDS+(ACQRECON-ACQD)),CHAR,         +        
               OLEN=L'ACQRECON,TEXT=AC#RCND,COL=*                               
ActSD    LKREQ F,45,(D,B#SAVED,REQCARDS+(ACQACTST-ACQD)),EDAT,         +        
               OLEN=L'ACQACTST,TEXT=AC#ACTST,COL=*                              
ActED    LKREQ F,46,(D,B#SAVED,REQCARDS+(ACQACTND-ACQD)),EDAT,         +        
               OLEN=L'ACQACTND,TEXT=AC#ACTND,COL=*                              
ExpJb    LKREQ F,47,(D,B#SAVED,REQCARDS+(ACQXJOB-ACQD)),EDAT,          +        
               OLEN=L'ACQXJOB,TEXT=AC#XJOB,COL=*                                
DrfOp    LKREQ F,48,(D,B#SAVED,REQCARDS+(ACQDRFOP-ACQD)),CHAR,         +        
               OLEN=L'ACQDRFOP,TEXT=AC#DRFIT,COL=*                              
RevOp    LKREQ F,49,(D,B#SAVED,REQCARDS+(ACQREVOP-ACQD)),CHAR,         +        
               OLEN=L'ACQREVOP,TEXT=AC#RVRSL,COL=*                              
LngCd    LKREQ F,50,(D,B#SAVED,REQCARDS+(ACQLANG-ACQD)),CHAR,          +        
               OLEN=L'ACQLANG,TEXT=AC#LANG,COL=*                                
Curry    LKREQ F,52,(D,B#SAVED,REQCARDS+(ACQCURR-ACQD)),CHAR,          +        
               OLEN=L'ACQCURR,TEXT=AC#CURRY,COL=*                               
Type1    LKREQ F,53,(D,B#SAVED,REQCARDS+(ACQTYP1-ACQD)),CHAR,          +        
               OLEN=L'ACQTYP1+L'ACQFLT1,TEXT=AC#TYPE1,COL=*                     
DtRng1   LKREQ F,54,(D,B#SAVED,REQCARDS+(ACQDTSTR-ACQD)),EDAT,RANGE=Y, +        
               OLEN=L'ACQDTSTR,TEXT=AC#DTRNG,COL=*                              
Type2    LKREQ F,57,(D,B#SAVED,REQCARDS+(ACQTYP2-ACQD)),CHAR,          +        
               OLEN=L'ACQTYP2+L'ACQFLT2,TEXT=AC#TYPE2,COL=*                     
DtRng2   LKREQ F,58,(D,B#SAVED,REQCARDS+(ACQTYP2+(ACQDTSTR-ACQTYP1)-ACQ+        
               D)),EDAT,OLEN=L'ACQDTSTR,RANGE=Y,TEXT=AC#DTRNG,COL=*             
Type3    LKREQ F,61,(D,B#SAVED,REQCARDS+(ACQTYP3-ACQD)),CHAR,          +        
               OLEN=L'ACQTYP3+L'ACQFLT3,TEXT=AC#TYPE3,COL=*                     
DtRng3   LKREQ F,62,(D,B#SAVED,REQCARDS+(ACQTYP3+(ACQDTSTR-ACQTYP1)-ACQ+        
               D)),EDAT,OLEN=L'ACQDTSTR,RANGE=Y,TEXT=AC#DTRNG,COL=*             
Type4    LKREQ F,65,(D,B#SAVED,REQCARDS+(ACQTYP4-ACQD)),CHAR,          +        
               OLEN=L'ACQTYP4+L'ACQFLT4,TEXT=AC#TYPE4,COL=*                     
DtRng4   LKREQ F,66,(D,B#SAVED,REQCARDS+(ACQTYP4+(ACQDTSTR-ACQTYP1)-ACQ+        
               D)),EDAT,OLEN=L'ACQDTSTR,RANGE=Y,TEXT=AC#DTRNG,COL=*             
Type5    LKREQ F,69,(D,B#SAVED,REQCARDS+(ACQTYP5-ACQD)),CHAR,          +        
               OLEN=L'ACQTYP5+L'ACQFLT5,TEXT=AC#TYPE5,COL=*                     
DtRng5   LKREQ F,70,(D,B#SAVED,REQCARDS+(ACQTYP5+(ACQDTSTR-ACQTYP1)-ACQ+        
               D)),EDAT,OLEN=L'ACQDTSTR,RANGE=Y,TEXT=AC#DTRNG,COL=*             
Type6    LKREQ F,73,(D,B#SAVED,REQCARDS+(ACQTYP6-ACQD)),CHAR,          +        
               OLEN=L'ACQTYP6+L'ACQFLT6,TEXT=AC#TYPE6,COL=*                     
DtRng6   LKREQ F,74,(D,B#SAVED,REQCARDS+(ACQTYP6+(ACQDTSTR-ACQTYP1)-ACQ+        
               D)),EDAT,OLEN=L'ACQDTSTR,RANGE=Y,TEXT=AC#DTRNG,COL=*             
Type7    LKREQ F,77,(D,B#SAVED,REQCARDS+(ACQTYP7-ACQD)),CHAR,          +        
               OLEN=L'ACQTYP7+L'ACQFLT7,TEXT=AC#TYPE7,COL=*                     
DtRng7   LKREQ F,74,(D,B#SAVED,REQCARDS+(ACQTYP7+(ACQDTSTR-ACQTYP1)-ACQ+        
               D)),EDAT,OLEN=L'ACQDTSTR,RANGE=Y,TEXT=AC#DTRNG,COL=*             
Type8    LKREQ F,81,(D,B#SAVED,REQCARDS+(ACQTYP8-ACQD)),CHAR,          +        
               OLEN=L'ACQTYP8+L'ACQFLT8,TEXT=AC#TYPE8,COL=*                     
DtRng8   LKREQ F,74,(D,B#SAVED,REQCARDS+(ACQTYP8+(ACQDTSTR-ACQTYP1)-ACQ+        
               D)),EDAT,OLEN=L'ACQDTSTR,RANGE=Y,TEXT=AC#DTRNG,COL=*             
*                                                                               
         LKREQ E                                                                
*                                                                               
         LKREQ X                                                                
*                                                                               
OUTACC   DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
* SAVED STORAGE                                                       *         
***********************************************************************         
         SPACE 1                                                                
SAVED    DSECT                     ** DSECT TO COVER SAVED STORAGE **           
*                                                                               
DUMMY    DS    0X                  DUMMY OUTPUT FOR DDLINK VALUES               
*                                                                               
AMONACCL DS    A                   A(MONACCL)                                   
*                                                                               
ACTIND   DS    XL1                 ACCOUNT INDEX                                
AACT     DS    AL3                 ADDRESS OF ACCOUNT BLOCK                     
EXCIND   DS    XL1                 EXCLUDED ACCOUNT INDEX                       
AEXACT   DS    AL3                 ADDRESS OF EXCLUDED ACCOUNT BLOCK            
*                                                                               
REQCARDS DS    (ACQL)C             REQUEST VALUES                               
*                                                                               
STRCALM  DS    CL4                 START CALENDAR MONTH (YYMM)                  
ENDCALM  DS    CL4                 END CALENDAR MONTH (YYMM)                    
*                                                                               
* ACLNKWRKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACLNKWRKD                                                      
         PRINT ON                                                               
* ACQD                                                                          
         PRINT OFF                                                              
       ++INCLUDE ACQD                                                           
ACQL     EQU   *-ACQD                                                           
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACLNK14   05/28/09'                                      
         END                                                                    
