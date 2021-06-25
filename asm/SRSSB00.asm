*          DATA SET SRSSB00    AT LEVEL 015 AS OF 02/25/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE T13000A                                                                  
         PRINT NOGEN                                                            
         TITLE '$SSB - DISPLAY SYSTEM STATUS'                                   
SSB      RSECT                                                                  
         NMOD1 WORKL,**$SSB**,CLEAR=YES,RR=RE                                   
         USING WORKD,RC            RC=A(W/S)                                    
         LARL  R8,LITERALS                                                      
         USING LITERALS,R8                                                      
         ST    RE,RELO                                                          
         ST    RD,SAVERD                                                        
*                                                                               
         USING SRPARMD,R1          R1=A(PARMS)                                  
         L     RA,SRQATWA                                                       
         USING SRSSBFFD,RA         RA=A(TWA)                                    
*                                                                               
         L     R9,SRQASYSF                                                      
         USING SYSFACD,R9          R9=A(SYSFACS)                                
         MVC   ASSB,VSSB                                                        
         MVC   AUTL,VUTL                                                        
         MVC   ATCB,VTCB                                                        
         MVC   ATICTOC,VTICTOC                                                  
         MVC   ADATAMGR,VDATAMGR                                                
         MVC   ACALLOV,VCALLOV                                                  
         MVC   ALOCKSPC,VLOCKSPC                                                
         MVC   AWCTYPE,VWCTYPE                                                  
         MVC   ADMOD000,VDMOD000                                                
         DROP  R9                                                               
*                                                                               
         L     R9,ASSB                                                          
         USING SSBD,R9             R9=A(SSB)                                    
*                                                                               
         BRAS  RE,INIT             INITIALISE                                   
         DROP  R1                                                               
*                                                                               
         BRAS  RE,CHKSVC           CHECK SERVICE REQUEST FIELD FOR I/P          
         BRAS  RE,VALP1            VALIDATE USER INPUT                          
         BRAS  RE,DISPLAY          DISPLAY DATA                                 
         J     XMOD                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY =SSB INFO                                        *         
* FIRST PART OF SCREEN IS ORDERED INTO A ROW COLUMN ARRAY             *         
* EACH ELEMENT HAS ITS OWN LITTLE SELF CONTAINED DISPLAY ROUTINE      *         
* THE POSITION ON SCREEN IS SET IN A TABLE - MAPTAB BELOW             *         
* EMPTY COLUMNS ARE SHOWN BY A PLACEHOLDER REFERENCE - DUMYRTN        *         
***********************************************************************         
DISPLAY  NTR1                                                                   
*                                                                               
         BRAS  RE,CLRTWA           FIRST CLEAR OUT THE SCREEN                   
*                                                                               
         USING FHD,R3                                                           
*                                                                               
         L     R4,AMAPTAB          SCREEN MAPPING TABLE                         
         LA    R3,SRVL3H           FIRST DISPLAY LINE                           
*                                                                               
DSP06    LA    R2,FHDA             DATA PORTION OF FIELD                        
         USING SCRLD,R2                                                         
         LLC   R0,FHLN             CALCULATE # OF COLUMNS THAT FIT              
         AHI   R0,L'SCRLGAP        ADD THIS TO ALLOW LAST WITH NO GAP           
         LHI   R1,FHDAD                                                         
         TM    FHAT,FHATXH                                                      
         JZ    *+8                                                              
         LHI   R1,FHDAD+FHDAD                                                   
         SR    R0,R1                                                            
         SRDL  R0,32                                                            
         LHI   RF,SCRLDLQ                                                       
         DR    R0,RF                                                            
         LR    R0,R1               R0=NUM OF COLUMNS ON LINE                    
*                                                                               
DSP08    CLI   0(R4),X'FF'         END OF MAPPING TABLE                         
         JE    DSP10               YES                                          
*                                                                               
         CLI   GLOBAL,YES          GLOBAL SSB                                   
         JNE   DSP09                                                            
         XR    R3,R3               SET R3 TO SSBG                               
         LAM   AR3,AR3,SSBALET                                                  
         SAC   512                                                              
         L     R3,DHASSBG-DMDHDR(,R3)                                           
*                                                                               
DSP09    ICM   RF,15,0(R4)         DO THIS ROUTINE                              
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         SAC   0                                                                
*                                                                               
         AHI   R4,4                NEXT MAPPING TABLE ENTRY                     
         AHI   R2,SCRLDLQ          NEXT COLUMN ON LINE                          
         JCT   R0,DSP08                                                         
*                                                                               
         XR    RF,RF               NEXT FIELD ON SCREEN                         
         ICM   RF,1,FHLN                                                        
         JZ    DSP10               SCREEN IS FULL                               
         JXH   R3,RF,DSP06                                                      
*                                                                               
DSP10    J     EXITOK                                                           
*                                                                               
TPSTHDR  MVC   SCRLHDR(15),=CL15'Tempest  Info -'                               
         BR    RE                                                               
TMPSHDR  MVC   SCRLHDR(15),=CL15'Tempstr  Info -'                               
         BR    RE                                                               
*                                                                               
DUMYRTN  BR    RE                  DUMMY PLACEHOLDER IN MAPTAB                  
         EJECT                                                                  
***********************************************************************         
* SECURITY VIOLATES - R2=A(SCRLD OUTPUT AREA)                         *         
***********************************************************************         
         USING SCRLD,R2                                                         
SECVRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Violates'                                           
         LLC   R1,SSBSECV                                                       
         EDIT  (R1),(4,SCRLTXT),ZERO=NOBLANK,ALIGN=LEFT                         
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CPU USED IN TOTAL - R2=A(SCRLD OUTPUT AREA)                         *         
***********************************************************************         
         USING SCRLD,R2                                                         
CPUURTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'CPU Used'                                           
         MVC   SCRLTXT,SPACES                                                   
         LG    GR1,SSBCPUSE                                                     
         DSG   GR0,=FD'1000'       CONVERT MICROSECS TO MILLISECS               
         CVDG  GR1,GRUB                                                         
         OC    GRUB(8),GRUB        TEST FOR OVERFLOW                            
         JZ    *+8                                                              
         MVI   SCRLHDR+7,C'?'                                                   
         EDIT  (P8,GRUB+8),(10,LEFT),ALIGN=LEFT                                 
         CHI   R0,9                                                             
         JH    *+14                                                             
         MVC   SCRLTXT,LEFT                                                     
         J     EXITOK                                                           
         MVC   SCRLTXT-1(10),LEFT                                               
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CPU USED BY TASKS - R2=A(SCRLD OUTPUT AREA)                         *         
***********************************************************************         
         USING SCRLD,R2                                                         
CPUTRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'CPU Task'                                           
         MVC   SCRLTXT,SPACES                                                   
         LG    GR1,SSBCPUTK                                                     
         DSG   GR0,=FD'1000'       CONVERT MICROSECS TO MILLISECS               
         CVDG  GR1,GRUB                                                         
         OC    GRUB(8),GRUB        TEST FOR OVERFLOW                            
         JZ    *+8                                                              
         MVI   SCRLHDR+7,C'?'                                                   
         EDIT  (P8,GRUB+8),(10,LEFT),ALIGN=LEFT                                 
         CHI   R0,9                                                             
         JH    *+14                                                             
         MVC   SCRLTXT,LEFT                                                     
         J     EXITOK                                                           
         MVC   SCRLTXT-1(10),LEFT                                               
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* USER INPUT GO/STOP - R2=A(SCRLD OUTPUT AREA)                        *         
***********************************************************************         
         USING SCRLD,R2                                                         
USERRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'User I/P'                                           
         MVC   SCRLTXT,SPACES                                                   
         MVC   SCRLTXT(02),=CL02'Go'                                            
         TM    SSBSTAT1,SSBUII                                                  
         JZ    *+10                                                             
         MVC   SCRLTXT(04),=CL04'Stop'                                          
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* START TIME - R2=A(SCRLD OUTPUT AREA)                                *         
***********************************************************************         
STTMRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Started '                                           
         L     R0,SSBSTIME         R0=START TIME IN 1/100 SEC                   
         L     R1,SSBSDATE         R1=START DATE                                
         TM    SSBSTAT1,SSBSRSRT                                                
         JZ    *+18                                                             
         MVC   SCRLHDR,=CL8'Restart '                                           
         L     R0,SSBRTIME         R0=START TIME IN 1/100 SEC                   
         L     R1,SSBSDATE         R1=START DATE                                
*                                                                               
         ST    R1,FULL                                                          
         GOTO1 VDATCON,DMCB,(6,FULL),(0,DUB)                                    
         GOTO1 VGETDAY,DMCB,DUB,FULL                                            
*                                                                               
         MVC   SCRLTXT,SPACES                                                   
         MVI   SCRLTXT+2,C'.'                                                   
         MVI   SCRLTXT+5,C'.'                                                   
*                                                                               
         SRDL  R0,32                                                            
         D     R0,=F'100'                                                       
         XR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  SCRLTXT+6(2),DUB    SECONDS                                      
*                                                                               
         XR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  SCRLTXT+3(2),DUB    MINUTES                                      
*                                                                               
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  SCRLTXT+0(2),DUB    HOURS                                        
*                                                                               
         MVI   SCRLTXT+5,C'/'      OVERWRITE SECONDS WITH DAY                   
         MVC   SCRLTXT+6(3),FULL                                                
         NC    SCRLTXT+7(2),=X'BFBF'                                            
*                                                                               
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* RESTART COUNT - R2=A(SCRLD OUTPUT AREA)                             *         
***********************************************************************         
RSTMRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Restarts'                                           
         MVC   SCRLTXT,SPACES                                                   
         MVC   SCRLTXT(04),=CL04'None'                                          
         XR    R0,R0                                                            
         ICM   R0,1,SSBRCNT                                                     
         JZ    EXITOK                                                           
*                                                                               
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         MVC   SCRLTXT,SPACES                                                   
         UNPK  SCRLTXT(2),DUB                                                   
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DOWN TIME - R2=A(SCRLD OUTPUT AREA)                                 *         
***********************************************************************         
DWTMRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Downtime'                                           
         MVC   SCRLTXT(04),=CL04'None'                                          
         XR    R0,R0                                                            
         ICM   R0,1,SSBRCNT                                                     
         JZ    EXITOK                                                           
*                                                                               
         MVC   SCRLTXT,SPACES                                                   
         MVI   SCRLTXT+2,C'.'                                                   
         MVI   SCRLTXT+5,C'.'                                                   
*                                                                               
         XR    R0,R0                                                            
         ICM   R1,3,SSBDTIME        R1=DOWN TIME IN SECONDS                     
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SCRLTXT+6(2),DUB                                                 
*                                                                               
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SCRLTXT+3(2),DUB                                                 
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SCRLTXT+0(2),DUB                                                 
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SIN DISPLAY ROUTINE - R2=A(SCRLD OUTPUT AREA)                       *         
***********************************************************************         
DSINRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Sin     '                                           
         EDIT  SSBSIN,(8,SCRLTXT),ALIGN=LEFT                                    
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MONSOON STATUS ROUTINE - R2=A(SCRLD OUTPUT AREA)                    *         
***********************************************************************         
MONSRTN  NTR1                                                                   
         CLI   SSBJESIO,C' '       IS MONSOON RUNNING?                          
         JNE   MSRTN02                                                          
         MVC   SCRLHDR,=CL8'Monsoon'                                            
         MVC   SCRLTXT,=CL10'Running'                                           
         J     EXITOK                                                           
*                                                                               
MSRTN02  MVC   SCRLHDR,=CL8'JES Job '                                           
         OC    SSBJESNO,SSBJESNO   TEST JES JOB NUM                             
         JNZ   MSRTN04                                                          
         MVC   SCRLTXT(04),=CL04'None'                                          
         J     EXITOK                                                           
*                                                                               
MSRTN04  MVC   SCRLTXT,SSBMVSID    MVS JOB ID & JES NUMBER                      
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SOON STATUS ROUTINE - R2=A(SCRLD OUTPUT AREA)                       *         
***********************************************************************         
SOONRTN  NTR1                                                                   
         CLI   SSBJESIO,C' '       IS MONSOON RUNNING?                          
         JNE   SRTN02                                                           
         MVC   SCRLHDR,=CL8'Soons  '                                            
         MVC   SCRLTXT,=CL10'Enabled'                                           
         TM    SSBJFLG2,SSBJFSN    ARE SOON REQUESTS DISABLED?                  
         JZ    *+10                                                             
         MVC   SCRLTXT,=CL10'Disabled'                                          
         J     EXITOK                                                           
*                                                                               
SRTN02   OC    SSBJESNO,SSBJESNO   TEST JES JOB NUM                             
         JZ    EXITOK                                                           
         MVC   SCRLHDR,=CL8'Job Id '                                            
         MVC   SCRLTXT(02),=CL02'(J'                                            
         LA    R3,SCRLTXT+2                                                     
         EDIT  SSBJESNO,(5,0(R3)),ALIGN=LEFT                                    
         AR    R3,R0                                                            
         MVI   0(R3),C')'                                                       
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* TASK COUNT ROUTINE - R2=A(SCRLD OUTPUT AREA)                        *         
***********************************************************************         
TSKSRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Num Tsks'                                           
         EDIT  SSBTASKS,(2,SCRLTXT),ALIGN=LEFT                                  
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ACTIVE TASKS ROUTINE - R2=A(SCRLD OUTPUT AREA)                      *         
***********************************************************************         
TSKARTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'PopCount'                                           
         EDIT  SSBTPOPC,(6,SCRLTXT),ALIGN=LEFT                                  
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DAY NUMBER ROUTINE - R2=A(SCRLD OUTPUT AREA)                        *         
***********************************************************************         
DAYNRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Day Num '                                           
         EDIT  SSBDAYNO,(2,SCRLTXT),ALIGN=LEFT                                  
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DUMP COUNT ROUTINE - R2=A(SCRLD OUTPUT AREA)                        *         
***********************************************************************         
DMPNRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'# Dumps'                                            
         BRAS  RE,ARSOFF                                                        
         LAM   AR3,AR3,SSBTBLET                                                 
         XR    R3,R3                                                            
         SAC   512                                                              
         ICM   R3,15,TABSDUMP-FATABSD(R3)                                       
         USING TORDUMPD,R3                                                      
         LHI   RF,TDLENQ                                                        
         AR    R3,RF                                                            
         LR    R0,RF               FIRST SLOT EMPTY - SAVE FOR NO MATCH         
*                                                                               
DPNR02   OC    TDSYSNA,TDSYSNA     FINISHED CHECKING SLOTS?                     
         JNZ   *+10                                                             
         LR    R3,R0               DEFAULT IS FIRST SLOT                        
         J     DPNR04                                                           
*                                                                               
         CLC   TDSYSNA,SSBSYSN4    MATCH FACPAK NAME                            
         JE    DPNR04                                                           
         JXH   R3,RF,DPNR02        NEXT SLOT                                    
*                                                                               
DPNR04   ICM   R0,15,TDDUMPMX                                                   
         ICM   R1,15,TDDUMPFS                                                   
         AHI   R0,1                RANGE, NOT DIFFERENCE                        
         SR    R0,R1                                                            
         BRAS  RE,ARSOFF                                                        
         DROP  R3                                                               
*                                                                               
         EDIT  (R0),(2,SCRLTXT),ALIGN=LEFT,ZERO=NOBLANK                         
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CURRENT DUMP NUMBER ROUTINE - R2=A(SCRLD OUTPUT AREA)               *         
***********************************************************************         
DMPTRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'LastDump'                                           
         BRAS  RE,ARSOFF                                                        
         LAM   AR3,AR3,SSBTBLET                                                 
         XR    R3,R3                                                            
         SAC   512                                                              
         ICM   R3,15,TABSDUMP-FATABSD(R3)                                       
         USING TORDUMPD,R3                                                      
         LHI   RF,TDLENQ                                                        
         AR    R3,RF                                                            
         LR    R0,RF               FIRST SLOT EMPTY - SAVE FOR NO MATCH         
*                                                                               
DPTR02   OC    TDSYSNA,TDSYSNA     FINISHED CHECKING SLOTS?                     
         JNZ   *+10                                                             
         LR    R3,R0               DEFAULT IS FIRST SLOT                        
         J     DPTR04                                                           
*                                                                               
         CLC   TDSYSNA,SSBSYSN4    MATCH FACPAK NAME                            
         JE    DPTR04                                                           
         JXH   R3,RF,DPTR02        NEXT SLOT                                    
*                                                                               
DPTR04   ICM   R0,15,TDDUMPNO                                                   
         EDIT  (R0),(3,SCRLTXT),ALIGN=LEFT,ZERO=NOBLANK                         
         BRAS  RE,ARSOFF                                                        
         J     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* CURRENT DUMP NUMBER ROUTINE - R2=A(SCRLD OUTPUT AREA)               *         
***********************************************************************         
DTIMRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'DumpTime'                                           
         MVC   SCRLTXT(9),=CL9'Not Today'                                       
         BRAS  RE,ARSOFF                                                        
         LAM   AR3,AR3,SSBTBLET                                                 
         XR    R3,R3                                                            
         SAC   512                                                              
         ICM   R3,15,TABSDUMP-FATABSD(R3)                                       
         USING TORDUMPD,R3                                                      
         LHI   RF,TDLENQ                                                        
         AR    R3,RF                                                            
         LR    R0,RF               FIRST SLOT EMPTY - SAVE FOR NO MATCH         
*                                                                               
DTME02   OC    TDSYSNA,TDSYSNA     FINISHED CHECKING SLOTS?                     
         JNZ   *+10                                                             
         LR    R3,R0               DEFAULT IS FIRST SLOT                        
         J     DTME04                                                           
*                                                                               
         CLC   TDSYSNA,SSBSYSN4    MATCH FACPAK NAME                            
         JE    DTME04                                                           
         JXH   R3,RF,DTME02        NEXT SLOT                                    
*                                                                               
DTME04   MVC   FULL,TDDUMPTM       SAVE DUMP TIME                               
         BRAS  RE,ARSOFF                                                        
         DROP  R3                                                               
*                                                                               
         OI    FULL+3,X'0F'        FORCE +VE PACKED NUMBER                      
         CP    FULL,=P'0'          IGNORE IF NOT SET                            
         JE    EXITOK                                                           
*                                                                               
         MVC   SCRLTXT,SPACES                                                   
         MVC   WORK(10),=X'402021214B21214B2121'                                
         ED    WORK(10),FULL                                                    
         OC    WORK(10),=C'  00.00.00'                                          
         MVC   SCRLTXT(8),WORK+2                                                
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LOOP TIMER - R2=A(SCRLD OUTPUT AREA)                                *         
***********************************************************************         
LOOPRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'T1 Timer'                                           
         MVC   FULL,SSBT1          LOOP TIMER                                   
         NI    FULL,X'7F'          RESET EXPIRATION BIT                         
         EDIT  (B4,FULL),(4,SCRLTXT),ALIGN=LEFT                                 
         LA    RE,SCRLTXT          YES                                          
         AR    RE,R0                                                            
         MVI   0(RE),C's'                                                       
         AHI   RE,1                                                             
         TM    SSBT1,X'80'         IS THE TIMER SET TO POP?                     
         JZ    EXITOK                                                           
         MVC   0(4,RE),=C'*POP'                                                 
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* GLOBAL TIMER - R2=A(SCRLD OUTPUT AREA)                              *         
***********************************************************************         
GLOBRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'T2 Timer'                                           
         MVC   FULL,SSBT2          GLOBAL TIMER                                 
         NI    FULL,X'7F'          RESET EXPIRATION BIT                         
         EDIT  (B4,FULL),(4,SCRLTXT),ALIGN=LEFT                                 
*                                                                               
         LA    RE,SCRLTXT                                                       
         AR    RE,R0                                                            
         MVI   0(RE),C's'                                                       
         AHI   RE,1                                                             
         TM    SSBT2,X'80'         IS THE TIMER SET TO POP?                     
         JZ    EXITOK                                                           
         MVC   0(4,RE),=C'*POP'                                                 
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SMTP NAME - R2=A(SCRLD OUTPUT AREA)                                 *         
***********************************************************************         
SMTPRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'SMTP Id '                                           
         MVC   SCRLTXT(L'SSBSMTP),SSBSMTP                                       
         CLC   SSBSMTP,SPACES                                                   
         JH    EXITOK                                                           
         MVC   SCRLTXT(7),=CL07'Not Set'                                        
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MQ MAX - R2=A(SCRLD OUTPUT AREA)                                    *         
***********************************************************************         
MXMQRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Max MQ  '                                           
         ICM   R0,15,SSBMQMAX                                                   
         JNZ   *+14                                                             
         MVC   SCRLTXT(7),=CL07'Not Set'                                        
         J     EXITOK                                                           
         EDIT  (R0),(7,SCRLTXT),ALIGN=LEFT                                      
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MQ NOW - R2=A(SCRLD OUTPUT AREA)                                    *         
***********************************************************************         
CUMQRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Current '                                           
         ICM   R0,15,SSBMQNUM                                                   
         EDIT  (R0),(7,SCRLTXT),ZERO=NOBLANK,ALIGN=LEFT                         
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MQ TODAY - R2=A(SCRLD OUTPUT AREA)                                  *         
***********************************************************************         
TDMQRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Total   '                                           
         ICM   R0,15,SSBMQTDY                                                   
         EDIT  (R0),(7,SCRLTXT),ZERO=NOBLANK,ALIGN=LEFT                         
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* BSAM TODAY - R2=A(SCRLD OUTPUT AREA)                                *         
***********************************************************************         
BSAMRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'FABSAM  '                                           
         ICM   R0,15,SSBBSAM                                                    
         EDIT  (R0),(7,SCRLTXT),ZERO=NOBLANK,ALIGN=LEFT                         
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VTAM NAME - R2=A(SCRLD OUTPUT AREA)                                 *         
***********************************************************************         
VTAMRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'VTAM Id '                                           
         MVC   SCRLTXT(L'SSBVTID),SSBVTID                                       
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* POLLING COUNT - R2=A(SCRLD OUTPUT AREA)                                       
***********************************************************************         
PCNTRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Poll Cnt'                                           
         MVC   SCRLTXT,=C'00000 00%'                                            
         TM    SSBSTAT4,SSBSAOR                                                 
         JZ    *+10                                                             
         MVC   SCRLTXT,=C'N/A      '                                            
         XR    R4,R4                                                            
         L     R5,SSB#MSGQ                                                      
         OC    SSB#POLL,SSB#POLL                                                
         JZ    EXITOK              DON'T DIVIDE BY ZERO                         
         MHI   R5,100                                                           
         D     R4,SSB#POLL                                                      
         ST    R5,FULL                                                          
         EDIT  (4,SSB#MSGQ),(5,SCRLTXT),ZERO=NOBLANK,ALIGN=LEFT                 
         LA    R4,SCRLTXT+6                                                     
         EDIT  (4,FULL),(2,SCRLTXT+6),0,ZERO=NOBLANK                            
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FA-MO MQ TRANSFER STATUS MESSAGE - R2=A(SCRLD OUTPUT AREA)          *         
***********************************************************************         
FAMORTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'FA-MO Mq'                                           
         MVC   SCRLTXT(8),=CL8'Disabled'                                        
         TM    SSBSTAT5,SSB5FAMO                                                
         JZ    *+10                                                             
         MVC   SCRLTXT(8),=CL8'Active  '                                        
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MQIO STATUS - R2=A(SCRLD OUTPUT AREA)                               *         
***********************************************************************         
MQIORTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'MQStatus'                                           
         MVC   SCRLTXT(8),=CL8'Disabled'                                        
         OC    SSBMQION,SSBMQION                                                
         JZ    EXITOK                                                           
         MVC   SCRLTXT(8),=CL8'Debug   '                                        
         LHI   RF,2                                                             
         CLM   RF,15,SSBMQION                                                   
         JE    EXITOK                                                           
         MVC   SCRLTXT(8),=CL8'Active  '                                        
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MQIO OPERATIONS - R2=A(SCRLD OUTPUT AREA)                           *         
***********************************************************************         
MQIOREQ  NTR1                                                                   
         MVC   SCRLHDR,=CL8'MQ Reqs'                                            
         EDIT  (B4,SSBMQREQ),(9,SCRLTXT),0,ALIGN=LEFT,ZERO=NOBLANK              
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* HIDE SELECTED CTFILE RECORDS - R2=A(SCRLD OUTPUT AREA)              *         
***********************************************************************         
HIDERTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'CT Hide '                                           
         MVC   SCRLTXT(8),=CL8'Disabled'                                        
         TM    SSBSTAT5,SSBCTHID                                                
         JZ    EXITOK                                                           
         MVC   SCRLTXT(8),=CL8'Hidden  '                                        
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LOCKWORD (CPU/ASID) - R2=A(SCRLD OUTPUT AREA)                       *         
***********************************************************************         
LOCKRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Lockword'                                           
         LA    R0,SSBLOCK                                                       
         GOTO1 VHEXOUT,DMCB,(R0),SCRLTXT,L'SSBLOCK,X'18000000'                  
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ZIP ASID - R2=A(SCRLD OUTPUT AREA)                                  *         
***********************************************************************         
ZIPARTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Zip Info'                                           
         LA    R0,SSBZASID                                                      
         GOTO1 VHEXOUT,DMCB,(R0),SCRLTXT,L'SSBZASID,X'18000000'                 
         MVC   SCRLTXT+4(4),=C'-???'                                            
         CLI   SSBZSTAT,SSBZSBSY                                                
         JNE   *+10                                                             
         MVC   SCRLTXT+5(3),=C'Bsy'                                             
         CLI   SSBZSTAT,SSBZSDED                                                
         JNE   *+10                                                             
         MVC   SCRLTXT+5(3),=C'Ded'                                             
         CLI   SSBZSTAT,SSBZSSLP                                                
         JNE   *+10                                                             
         MVC   SCRLTXT+5(3),=C'Slp'                                             
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MEDIA DATASPACE ROUTINE - R2=A(SCRLD OUTPUT AREA)                   *         
***********************************************************************         
MEDDRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Med Dspc'                                           
         MVC   SCRLTXT,=CL9'Disabled'                                           
         OC    SSBMEDTB,SSBMEDTB                                                
         JZ    *+16                                                             
         MVC   SCRLTXT,=CL9'Enlb xxxx'                                          
         MVC   SCRLTXT+5(4),SSBMEDTN                                            
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DARE HIGH D/A - R2=A(SCRLD OUTPUT AREA)                             *         
***********************************************************************         
DADARTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Dare D/A'                                           
         LA    R0,SSBDARDA                                                      
         GOTO1 VHEXOUT,DMCB,(R0),SCRLTXT,L'SSBDARDA,X'18000000'                 
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DARE PQ TABLE - R2=A(SCRLD OUTPUT AREA)                             *         
***********************************************************************         
DAPQRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Dare PQ '                                           
         LA    R0,SSBDARPQ                                                      
         GOTO1 VHEXOUT,DMCB,(R0),SCRLTXT,L'SSBDARPQ,X'18000000'                 
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* EDICTA HIGH ADDRESS - R2=A(SCRLD OUTPUT AREA)                       *         
***********************************************************************         
EADARTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'EDCTA DA'                                           
         LA    R0,SSBDAREA                                                      
         GOTO1 VHEXOUT,DMCB,(R0),SCRLTXT,L'SSBDAREA,X'18000000'                 
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* EDICTR HIGH ADDRESS - R2=A(SCRLD OUTPUT AREA)                       *         
***********************************************************************         
ERDARTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'EDCTR DA'                                           
         LA    R0,SSBDARER                                                      
         GOTO1 VHEXOUT,DMCB,(R0),SCRLTXT,L'SSBDARER,X'18000000'                 
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* HIGH CORE ADDRESS - R2=A(SCRLD OUTPUT AREA)                         *         
***********************************************************************         
HICORTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Highcore'                                           
         LA    R0,SSBHIADR         HIGH CORE                                    
         GOTO1 VHEXOUT,DMCB,(R0),SCRLTXT,L'SSBHIADR,X'18000000'                 
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LOW CORE ADDRESS - R2=A(SCRLD OUTPUT AREA)                          *         
***********************************************************************         
LOCORTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Lowcore'                                            
         LA    R0,SSBLOADR         HIGH CORE                                    
         GOTO1 VHEXOUT,DMCB,(R0),SCRLTXT,L'SSBLOADR,X'18000000'                 
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* REGION USED - R2=A(SCRLD OUTPUT AREA)                               *         
***********************************************************************         
REGNRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Region  '                                           
         L     R0,SSBHIADR         REGION (K) = (HICORE-LOCORE)/1024            
         AHI   R0,1                                                             
         S     R0,SSBLOADR                                                      
         SRDL  R0,32                                                            
         D     R0,=F'1024'                                                      
         LTR   R0,R0               ROUND UP                                     
         JZ    *+8                                                              
         AHI   R1,1                                                             
*                                                                               
         LR    R0,R1                                                            
         EDIT  (R0),(7,SCRLTXT),ALIGN=LEFT                                      
         LA    R1,SCRLTXT                                                       
         AR    R1,R0                                                            
         MVI   0(R1),C'K'                                                       
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DD GETMAIN SIZE - R2=A(SCRLD OUTPUT AREA)                           *         
***********************************************************************         
DDSZRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'DDict Sz'                                           
         L     R0,SSBDICTG         REGION (K) = (HICORE-LOCORE)/1024            
         AHI   R0,1                                                             
         SRDL  R0,32                                                            
         D     R0,=F'1024'                                                      
         LTR   R0,R0               ROUND UP                                     
         JZ    *+8                                                              
         AHI   R1,1                                                             
*                                                                               
         LR    R0,R1                                                            
         EDIT  (R0),(7,SCRLTXT),ALIGN=LEFT                                      
         LA    R1,SCRLTXT                                                       
         AR    R1,R0                                                            
         MVI   0(R1),C'K'                                                       
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* XA HIGH CORE ADDRESS - R2=A(SCRLD OUTPUT AREA)                      *         
***********************************************************************         
XHCORTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'XA High '                                           
         LA    R0,SSBXAHI          HIGH CORE                                    
         GOTO1 VHEXOUT,DMCB,(R0),SCRLTXT,L'SSBXAHI,X'18000000'                  
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* XA LOW CORE ADDRESS - R2=A(SCRLD OUTPUT AREA)                       *         
***********************************************************************         
XLCORTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'XA Low '                                            
         LA    R0,SSBXALO          LOW CORE                                     
         GOTO1 VHEXOUT,DMCB,(R0),SCRLTXT,L'SSBXALO,X'18000000'                  
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* REGION USED - R2=A(SCRLD OUTPUT AREA)                               *         
***********************************************************************         
XRGNRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'XA Regn '                                           
         L     R0,SSBXAHI          REGION (K) = (HICORE-LOCORE)/1024            
         AHI   R0,1                                                             
         S     R0,SSBXALO                                                       
         SRDL  R0,32                                                            
         D     R0,=F'1024'                                                      
         LTR   R0,R0               ROUND UP                                     
         JZ    *+8                                                              
         AHI   R1,1                                                             
*                                                                               
         LR    R0,R1                                                            
         EDIT  (R0),(7,SCRLTXT),ALIGN=LEFT                                      
         LA    R1,SCRLTXT                                                       
         AR    R1,R0                                                            
         MVI   0(R1),C'K'                                                       
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIMER POP TIME - R2=A(SCRLD OUTPUT AREA)                       *         
***********************************************************************         
TPOPRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Last Pop'                                           
         MVC   FULL,SSBTPOPT                                                    
         MVC   WORK(10),=X'402020204B20204B2020'                                
         ED    WORK(10),FULL                                                    
         OC    WORK(10),=C'  00.00.00'                                          
         MVC   SCRLTXT(8),WORK+2                                                
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MAX UTLS - R2=A(SCRLD OUTPUT AREA)                                  *         
***********************************************************************         
MUTLRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Core Max'                                           
         SAM31                                                                  
         L     RE,AUTL             FOR VTAM IS AT AUTL-2(2)                     
         AHI   RE,-2                                                            
         LH    R0,0(RE)                                                         
         SAM24                                                                  
*                                                                               
         EDIT  (R0),(5,SCRLTXT),ALIGN=LEFT                                      
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CURRENT UTLS - R2=A(SCRLD OUTPUT AREA)                              *         
***********************************************************************         
CUTLRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Current '                                           
         EDIT  (2,SSBTRMS),(5,SCRLTXT),ALIGN=LEFT                               
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MAX TEMPSTR - R2=A(SCRLD OUTPUT AREA)                               *         
***********************************************************************         
TPSTRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Disk Max'                                           
         XC    IOA,IOA                                                          
         MVC   DMCB+8(4),=X'0000FFFF'                                           
         GOTO1 ADATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,IOA                        
         EDIT  (2,IOA+6),(5,SCRLTXT),ALIGN=LEFT                                 
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MAX SESSIONS - R2=A(SCRLD OUTPUT AREA)                              *         
***********************************************************************         
MXSSRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Sess L/P'                                           
         LH    R0,SSBSSMAX                                                      
         EDIT  (R0),(4,SCRLTXT),ALIGN=LEFT                                      
         LA    R3,SCRLTXT                                                       
         AR    R3,R0                                                            
         MVI   0(R3),C'/'                                                       
         LH    R0,SSBSSMXP                                                      
         EDIT  (R0),(4,1(R3)),ALIGN=LEFT                                        
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MAX TEMPEST - R2=A(SCRLD OUTPUT AREA)                               *         
***********************************************************************         
MXTMRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Maximum'                                            
         XC    IOA,IOA                                                          
         MVC   DMCB+8(4),=X'0000FFFF'                                           
         GOTO1 ADATAMGR,DMCB,DMREAD,TEMPEST,,IOA                                
X        USING TTSSHDRD,IOA                                                     
         EDIT  (B4,X.TTSMAXCI),(5,SCRLTXT),ALIGN=LEFT,ZERO=NOBLANK              
         J     EXITOK                                                           
         DROP  X                                                                
         EJECT                                                                  
***********************************************************************         
* CURR TEMPEST - R2=A(SCRLD OUTPUT AREA)                              *         
***********************************************************************         
CUTMRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Current'                                            
         XC    IOA,IOA                                                          
         MVC   DMCB+8(4),=X'0000FFFF'                                           
         GOTO1 ADATAMGR,DMCB,DMREAD,TEMPEST,,IOA                                
X        USING TTSSHDRD,IOA                                                     
         EDIT  (B4,X.TTSUSECI),(5,SCRLTXT),ALIGN=LEFT,ZERO=NOBLANK              
         J     EXITOK                                                           
         DROP  X                                                                
         EJECT                                                                  
***********************************************************************         
* HIGH TEMPEST - R2=A(SCRLD OUTPUT AREA)                              *         
***********************************************************************         
HITMRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Day High'                                           
         XC    IOA,IOA                                                          
         MVC   DMCB+8(4),=X'0000FFFF'                                           
         GOTO1 ADATAMGR,DMCB,DMREAD,TEMPEST,,IOA                                
X        USING TTSSHDRD,IOA                                                     
         EDIT  (B4,X.TTSHICI),(5,SCRLTXT),ALIGN=LEFT,ZERO=NOBLANK               
         J     EXITOK                                                           
         DROP  X                                                                
         EJECT                                                                  
***********************************************************************         
* TEMPSTR PAGES - R2=A(SCRLD OUTPUT AREA)                             *         
***********************************************************************         
PGTSRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Pg/Sess '                                           
         EDIT  (B2,SSBSSPGS),(5,SCRLTXT),ALIGN=LEFT                             
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* TEMPSTR PAGES PER TERMINAL - R2=A(SCRLD OUTPUT AREA)                *         
***********************************************************************         
PGTPRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Twa/Sess'                                           
         EDIT  (B2,SSBTWAS),(5,SCRLTXT),ALIGN=LEFT                              
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* TEMPSTR PAGE LENGTH - R2=A(SCRLD OUTPUT AREA)                       *         
***********************************************************************         
LETPRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Page Len'                                           
         EDIT  (B2,SSBTMSL),(5,SCRLTXT),ALIGN=LEFT                              
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* TEMPEST PAGES - R2=A(SCRLD OUTPUT AREA)                             *         
***********************************************************************         
PGTMRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Pg/Sess '                                           
         XC    IOA,IOA                                                          
         MVC   DMCB+8(4),=X'0000FFFF'                                           
         GOTO1 ADATAMGR,DMCB,DMREAD,TEMPEST,,IOA                                
X        USING TTSSHDRD,IOA                                                     
         EDIT  (B2,X.TTSMAX),(5,SCRLTXT),ALIGN=LEFT,ZERO=NOBLANK                
         J     EXITOK                                                           
         DROP  X                                                                
         EJECT                                                                  
***********************************************************************         
* TSAR BUFFERS - R2=A(SCRLD OUTPUT AREA)                              *         
***********************************************************************         
TBUFRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'TsarBuff'                                           
         LA    R3,SCRLTXT                                                       
         TM    SSBTSAR,X'80'       TEST 2 TSAR BUFFERS                          
         JZ    *+14                                                             
         MVC   0(2,R3),=C'2*'                                                   
         AHI   R3,2                                                             
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,SSBTSAR+1                                                   
         EDIT  (R0),(7,(R3)),ALIGN=LEFT                                         
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VTAM HANGS - R2=A(SCRLD OUTPUT AREA)                                *         
***********************************************************************         
VTHGRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'VTAM Hng'                                           
         MVC   SCRLTXT(4),=CL04'None'                                           
         ICM   R0,15,SSBVTHNG      TEST ANY HANGS                               
         JZ    EXITOK                                                           
         SRL   R0,4                                                             
         ST    R0,FULL                                                          
         MVC   WORK(10),=X'402020204B20204B2020'                                
         ED    WORK(10),FULL                                                    
         OC    WORK(10),=C'  00.00.00'                                          
         MVC   SCRLTXT(8),WORK+2                                                
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FACWRK UPDATIVE - R2=A(SCRLD OUTPUT AREA)                           *         
***********************************************************************         
FACWRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'FACWRK  '                                           
         MVC   SCRLTXT(9),=CL9'Running'                                         
         TM    SSBJFLAG,SSBJFNUP                                                
         JZ    *+10                                                             
         MVC   SCRLTXT(9),=CL9'Inhibited'                                       
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* STORAGE PROTECT - R2=A(SCRLD OUTPUT AREA)                           *         
***********************************************************************         
STPTRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Mem Prot'                                           
         MVC   SCRLTXT(8),=CL8'Disabled'                                        
         TM    SSBPROT,SSBPTSKQ                                                 
         JZ    *+8                                                              
         MVI   SCRLTXT+8,C'*'                                                   
         TM    SSBPROT,SSBPONQ                                                  
         JZ    EXITOK                                                           
         MVC   SCRLTXT(8),=CL8'Active'                                          
         TM    SSBPROT,SSBPROTQ                                                 
         JO    *+10                                                             
         MVC   SCRLTXT(8),=CL8'Debug '                                          
         TM    SSBPROT,SSBPROTO                                                 
         JZ    *+10                                                             
         MVC   SCRLTXT(8),=CL8'Inactive'                                        
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* PQ NEW REPORTS - R2=A(SCRLD OUTPUT AREA)                            *         
***********************************************************************         
PQNWRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'New Rpts'                                           
         MVC   SCRLTXT(4),=CL04'None'                                           
         ICM   RF,15,SSBPQXPE                                                   
         JZ    EXITOK                                                           
         ICM   R0,15,8(RF)         R0=NUM OF PART1 INDEX SEARCHES               
         JZ    EXITOK                                                           
*                                                                               
         MVC   SCRLTXT,SPACES                                                   
         EDIT  (R0),(5,SCRLTXT),ALIGN=LEFT,ZERO=NOBLANK                         
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* AVERAGE PQ I/O COUNTS - R2=A(SCRLD OUTPUT AREA)                     *         
***********************************************************************         
PQIORTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'IO/Rpt  '                                           
         MVC   SCRLTXT(4),=CL04'Zero'                                           
         ICM   RF,15,SSBPQXPE                                                   
         JZ    EXITOK                                                           
*                                                                               
         ICM   R0,15,12(RF)        R0=NUM OF PART1 INDEX I/OS                   
         SRDL  R0,32                                                            
         MHI   R1,200              R1=(NUM OF PART1 INDEX I/OS)*200             
*                                                                               
         ICM   RF,15,08(RF)        RF=NUM OF PART1 INDEX SEARCHES               
         JZ    EXITOK                                                           
*                                                                               
         MVC   SCRLTXT,SPACES                                                   
         DR    R0,RF               R1=(AVG NUM OF INDEX I/OS)*200               
         LTR   R1,R1                                                            
         JM    *+8                                                              
         AHI   R1,1                ROUND UP                                     
         SRA   R1,1                                                             
         LR    R0,R1                                                            
         EDIT  (R0),(5,SCRLTXT),2,ALIGN=LEFT                                    
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SHOW WHETHER THIS IS A TOR OR AN AOR - R2=A(SCRLD OUTPUT AREA)      *         
***********************************************************************         
TOAORTN  NTR1                                                                   
         MVC   SCRLHDR(03),=C'AOR' TEST AOR OR TOR                              
         TM    SSBSTAT4,SSBSAOR                                                 
         JNO   TOAO01                                                           
         LLC   R1,SSBSYSIX         SET AOR CHR                                  
         SRL   R1,4                                                             
         LA    R1,X'C0'(R1)                                                     
         STC   R1,SCRLHDR+3                                                     
         J     EXITOK                                                           
*                                                                               
TOAO01   MVC   SCRLHDR(03),=C'TOR' IF TOR SHOW ATTACHED AOR COUNT               
*                                                                               
         BRAS  RE,ARSOFF                                                        
         LAM   AR3,AR3,SSBALET                                                  
         ICM   R3,15,SSBATOR                                                    
         AHI   R3,TORFACLQ         INDEX TO FACPAK EXCHANGE GRID                
         SAC   512                                                              
         USING SBEXCHD,R3                                                       
         LH    R4,0(,R3)                                                        
         L     R5,2(,R3)                                                        
         LA    R3,6(R4,R3)         FIRST ENTRY IS ALWAYS TOR                    
         XR    R0,R0                                                            
*                                                                               
TOAO02   OC    SBSTOKEN,SBSTOKEN   AOR IN SLOT?                                 
         JZ    TOAO04              NO                                           
         CLI   SBAVLBL,SBYES       AOR WORKING?                                 
         JNE   TOAO04              NO                                           
         AHI   R0,1                INCREMENT NUMBER OF AORS RUNNING             
*                                                                               
TOAO04   JXLE  R3,R4,TOAO02                                                     
         BRAS  RE,ARSOFF                                                        
         DROP  R3                                                               
*                                                                               
         LTR   R0,R0                                                            
         JZ    EXITOK              NO AORS                                      
         EDIT  (R0),(2,SCRLTXT),ALIGN=LEFT                                      
*                                                                               
         LA    RE,SCRLTXT                                                       
         AR    RE,R0                                                            
         MVC   0(7,RE),=CL7'*AOR(s)'                                            
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SMFID - R2=A(SCRLD OUTPUT AREA)                                     *         
***********************************************************************         
SMFIRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'SMF Id'                                             
         L     R1,X'10'(,0)        POINT TO CPU ID                              
         L     R1,X'C4'(R1)                                                     
         LA    R1,X'10'(R1)        A(FOUR CHARACTER CPU ID)                     
         MVC   SCRLTXT(4),0(R1)                                                 
         MVC   SCRLTXT+5(3),=C'OFF'                                             
         TM    SSBSTAT5,SSBADRSM   TEST SMF ADRFILE RECORDS ACTIVE              
         JZ    EXITOK                                                           
         MVC   SCRLTXT+5(3),=C'SMF'                                             
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MED PAY - R2=A(SCRLD OUTPUT AREA)                                   *         
***********************************************************************         
MPAYRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Med Pay'                                            
         MVC   SCRLTXT(8),=CL8'Disabled'                                        
         TM    SSBSTAT3,SSBNOPAY                                                
         JZ    *+10                                                             
         MVC   SCRLTXT(8),=CL8'Active'                                          
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SRTIM STATUS - R2=A(SCRLD OUTPUT AREA)                              *         
***********************************************************************         
SRTMRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'$TIM   '                                            
         MVC   SCRLTXT(8),=CL8'Inactive'                                        
         TM    SSBSTAT3,SSBSRTIM                                                
         JZ    *+10                                                             
         MVC   SCRLTXT(8),=CL8'Active'                                          
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DUPLICATE DUMPS - R2=A(SCRLD OUTPUT AREA)                           *         
***********************************************************************         
DUPERTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'DupDumps'                                           
         MVC   SCRLTXT(8),=CL8'Written'                                         
         TM    SSBSTAT3,SSBDUPDP                                                
         JO    *+10                                                             
         MVC   SCRLTXT(8),=CL8'Ignored'                                         
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* PGMS FILE OWNERSHIP - R2=A(SCRLD OUTPUT AREA)                       *         
***********************************************************************         
PGMSRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'PgmsFile'                                           
         MVC   SCRLTXT(9),=CL9'Not Owner'                                       
         CLI   SSBPGMUP,YES                                                     
         JNE   *+10                                                             
         MVC   SCRLTXT(9),=CL9'Owner'                                           
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* POLLING FOR DDLINK - R2=A(SCRLD OUTPUT AREA)                        *         
***********************************************************************         
POLLRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Polling'                                            
         MVC   SCRLTXT(9),=CL9'Disabled'                                        
         CLI   SSBSTAT6,SSB6POLL                                                
         JNE   *+10                                                             
         MVC   SCRLTXT(9),=CL9'Enabled'                                         
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MAX IO COUNTS - R2=A(SCRLD OUTPUT AREA)                             *         
***********************************************************************         
MXIORTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Max I/O '                                           
         XR    R0,R0                                                            
         ICM   R0,3,SSBVARIO                                                    
         JNZ   *+8                                                              
         ICM   R0,3,SSBMAXIO                                                    
         EDIT  (R0),(5,SCRLTXT),ALIGN=LEFT                                      
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,SSBVARIO                                                    
         JZ    EXITOK                                                           
         LA    RE,SCRLTXT                                                       
         AR    RE,R0                                                            
         MVC   0(03,RE),=CL03'(v)'                                              
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FREE GFREC COUNT - R2=A(SCRLD OUTPUT AREA)                          *         
***********************************************************************         
GFRCRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'GFRECS  '                                           
         LA    R3,SCRLTXT                                                       
         LH    R0,SSBGFUSD         USED COUNT                                   
         EDIT  (R0),(4,0(R3)),ALIGN=LEFT                                        
         AR    R3,R0                                                            
         MVI   0(R3),C'/'                                                       
         LH    R0,SSBGFREQ         REQUESTED COUNT                              
         EDIT  (R0),(4,1(R3)),ALIGN=LEFT                                        
         J     EXITOK                                                           
***********************************************************************         
* WKFILE BUFFERING STATUS - R2=A(SCRLD OUTPUT AREA)                   *         
***********************************************************************         
WKFLRTN  NTR1                                                                   
         MVC   SCRLHDR(8),=CL8'WKBuff '                                         
         MVC   SCRLTXT(8),=CL8'Active '                                         
         BRAS  RE,ARSOFF                                                        
         XC    DMCB(4*6),DMCB      GET DATASPACE HEADER FROM TABS               
         MVC   DMCB(4),=AL4(DTWRKR)                                             
         MVI   DMCB,X'20'          SET ENQUIRE                                  
         GOTO1 ALOCKSPC,DMCB                                                    
*                                                                               
         L     RF,4(R1)            SAVE DSPACE HEADER                           
         USING DMSPACED,RF                                                      
         NC    DSPTFRST,=XL4'3FFFFFFF'                                          
         BRAS  RE,ARSOFF                                                        
         LAM   AR3,AR3,SSBTBLET                                                 
         ICM   R3,15,DSPTFRST                                                   
         SAC   512                                                              
         USING WKBUFFD,R3                                                       
         CLI   WKBUFFID,0          ADDRESS SPACE PRESENT?                       
         JNE   *+10                                                             
         MVC   SCRLTXT(8),=CL8'Disabled'                                        
         BRAS  RE,ARSOFF                                                        
         DROP  R3,RF                                                            
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CTFILE BUFFERING STATUS - R2=A(SCRLD OUTPUT AREA)                   *         
***********************************************************************         
CTFLRTN  NTR1                                                                   
         MVC   SCRLHDR(8),=CL8'CTB I/O'                                         
         MVC   SCRLTXT(8),=CL8'0      '                                         
         BRAS  RE,ARSOFF                                                        
         XC    DMCB(4*6),DMCB      GET DATASPACE HEADER FROM TABS               
         MVC   DMCB(4),=AL4(DTCTB)                                              
         MVI   DMCB,X'20'          SET ENQUIRE                                  
         GOTO1 ALOCKSPC,DMCB                                                    
*                                                                               
         L     RF,4(R1)            SAVE DSPACE HEADER                           
         USING DMSPACED,RF                                                      
         NC    DSPTFRST,=XL4'3FFFFFFF'                                          
         BRAS  RE,ARSOFF                                                        
         LAM   AR3,AR3,SSBTBLET                                                 
         ICM   R3,15,DSPTFRST                                                   
         SAC   512                                                              
         USING CTBUFFD,R3                                                       
         CLI   CTBUFFID,0          TEST CTBUFFER ACTIVE                         
         JE    CTFLX                                                            
         LG    GR0,CTBUFFIV        OUTPUT NUMBER OF HITS                        
         CVDG  GR0,GRUB                                                         
         OC    GRUB(8),GRUB        TEST FOR OVERFLOW                            
         JZ    *+8                                                              
         MVI   SCRLHDR+7,C'?'                                                   
         EDIT  (P8,GRUB+8),(9,SCRLTXT),ALIGN=LEFT,ZERO=NOBLANK                  
*                                                                               
CTFLX    BRAS  RE,ARSOFF                                                        
         DROP  R3,RF                                                            
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CTFILE BUFFERED RECORDS - R2=A(SCRLD OUTPUT AREA)                   *         
***********************************************************************         
CTBURTN  NTR1                                                                   
         MVC   SCRLHDR(8),=CL8'Tot Tran'                                        
         EDIT  (B4,CSSBTTR#),(9,SCRLTXT),ZERO=NOBLANK,ALIGN=LEFT                
*&&DO                                                                           
         MVC   SCRLHDR(8),=CL8'CTB CPU '                                        
         MVC   SCRLTXT(8),=CL8'0       '                                        
         BRAS  RE,ARSOFF                                                        
         XC    DMCB(4*6),DMCB      GET DATASPACE HEADER FROM TABS               
         MVC   DMCB(4),=AL4(DTCTB)                                              
         MVI   DMCB,X'20'          SET ENQUIRE                                  
         GOTO1 ALOCKSPC,DMCB                                                    
*                                                                               
         L     RF,4(R1)            SAVE DSPACE HEADER                           
         USING DMSPACED,RF                                                      
         NC    DSPTFRST,=XL4'3FFFFFFF'                                          
         BRAS  RE,ARSOFF                                                        
         LAM   AR3,AR3,SSBTBLET                                                 
         ICM   R3,15,DSPTFRST                                                   
         SAC   512                                                              
         USING CTBUFFD,R3                                                       
         CLI   CTBUFFID,0          TEST CTBUFFER ACTIVE                         
         JE    CTBUX                                                            
*                                                                               
CTBU01   LG    GR1,CTBUFFCP        CONVERT CPU MICROSECS TO MILLISECS           
         DSG   GR0,=FD'1000'                                                    
         CVDG  GR1,GRUB                                                         
         OC    GRUB(8),GRUB        TEST FOR OVERFLOW                            
         JZ    *+8                                                              
         MVI   SCRLHDR+7,C'?'                                                   
         EDIT  (P8,GRUB+8),(10,LEFT),ALIGN=LEFT                                 
         CHI   R0,9                                                             
         JH    *+14                                                             
         MVC   SCRLTXT,LEFT                                                     
         J     *+10                                                             
         MVC   SCRLTXT-1(10),LEFT                                               
*                                                                               
CTBU02   LA    R3,CTBUFFP          R3=A(LIST OF BUFFERED RECORD KEYS)           
         USING CTBUFFP,R3                                                       
         LA    R4,SCRLHDR                                                       
         LHI   R0,L'SCRLHDR                                                     
*                                                                               
CTBU03   CLI   CTBUFFPI,X'FF'                                                   
         JE    CTBU04                                                           
         CLI   CTBUFFPI,0                                                       
         JE    CTBUX                                                            
         MVC   0(1,R4),CTBUFFPI                                                 
         AHI   R4,1                                                             
         AHI   R3,CTBUFFPL                                                      
         JCT   R0,CTBU03                                                        
*NOP*    MVI   SCRLHDR+L'SCRLHDR-1,C'+'                                         
*                                                                               
CTBU04   LG    GR0,CTBUFFCT        OUTPUT NUMBER OF CTBUFFER MISSES             
         AG    GR0,CTBUFFOL                                                     
         LTGR  GR0,GR0                                                          
         JZ    CTBUX                                                            
         CVDG  GR0,GRUB                                                         
         EDIT  (P8,GRUB+8),(9,SCRLHDR),ALIGN=LEFT,ZERO=NOBLANK                  
         OC    GRUB(8),GRUB                                                     
         JZ    *+8                                                              
         MVI   SCRLHDR+8,C'?'      SET OVERFLOW                                 
*                                                                               
CTBUX    BRAS  RE,ARSOFF                                                        
         DROP  R3,RF                                                            
*&&                                                                             
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* RECOVERTY WRITE INFORMATION - R2=A(SCRLD OUTPUT AREA)               *         
***********************************************************************         
         USING FASSBG,R3                                                        
RECORTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Rcvr Wrt'                                           
         MVC   SCRLTXT(9),=C'at EOT   '                                         
         TM    SSGSTAT1,SSGSRCVW                                                
         JO    EXITOK                                                           
         MVC   SCRLTXT(9),=C'Always   '                                         
         J     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* CONTROL FILE ENQ INFORMATION - R2=A(SCRLD OUTPUT AREA)              *         
***********************************************************************         
CENQRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Control'                                            
         MVC   SCRLTXT(8),=CL8'Not ENQ'                                         
         OC    SSBCTTSK,SSBCTTSK   CONTROL SYSTEM ENQUEUE STATUS                
         JZ    EXITOK                                                           
         MVC   SCRLTXT,SPACES                                                   
         MVC   SCRLHDR(8),=CL8'CTRL#'                                           
         EDIT  (B1,SSBCTTSK),(2,SCRLHDR+5),ALIGN=LEFT,ZERO=NOBLANK              
*                                                                               
         OC    SSBCTTSK+1(3),SSBCTTSK+1                                         
         JZ    EXITOK                                                           
         XR    RF,RF                                                            
         ICM   RF,7,SSBCTTSK+1     SET HOLDING TASK ID                          
         MVC   SCRLTXT(L'TCBID),TCBID-TCBD(RF)                                  
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SERVICE FILE ENQ INFORMATION - R2=A(SCRLD OUTPUT AREA)              *         
***********************************************************************         
SENQRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'Service'                                            
         MVC   SCRLTXT(8),=CL8'Not ENQ'                                         
         OC    SSBSRTSK,SSBSRTSK   CONTROL SYSTEM ENQUEUE STATUS                
         JZ    EXITOK                                                           
         MVC   SCRLTXT,SPACES                                                   
         MVC   SCRLHDR(8),=CL8'SRVC#'                                           
         EDIT  (B1,SSBSRTSK),(2,SCRLHDR+5),ALIGN=LEFT,ZERO=NOBLANK              
*                                                                               
         OC    SSBSRTSK+1(3),SSBSRTSK+1                                         
         JZ    EXITOK                                                           
         XR    RF,RF                                                            
         ICM   RF,7,SSBSRTSK+1     SET HOLDING TASK ID                          
         MVC   SCRLTXT(L'TCBID),TCBID-TCBD(RF)                                  
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MEDZ FILE ENQ INFORMATION - R2=A(SCRLD OUTPUT AREA)                 *         
***********************************************************************         
MENQRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'MEDZ   '                                            
         MVC   SCRLTXT(8),=CL8'Not ENQ'                                         
         OC    SSBMZTSK,SSBMZTSK   MEDZ SYSTEM ENQUEUE STATUS                   
         JZ    EXITOK                                                           
         MVC   SCRLTXT,SPACES                                                   
         MVC   SCRLHDR(8),=CL8'MEDZ#'                                           
         EDIT  (B1,SSBMZTSK),(2,SCRLHDR+5),ALIGN=LEFT,ZERO=NOBLANK              
*                                                                               
         OC    SSBMZTSK+1(3),SSBMZTSK+1                                         
         JZ    EXITOK                                                           
         XR    RF,RF                                                            
         ICM   RF,7,SSBMZTSK+1     SET HOLDING TASK ID                          
         MVC   SCRLTXT(L'TCBID),TCBID-TCBD(RF)                                  
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* EAZI FILE ENQ INFORMATION - R2=A(SCRLD OUTPUT AREA)                 *         
***********************************************************************         
EENQRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'EASI   '                                            
         MVC   SCRLTXT(8),=CL8'Not ENQ'                                         
         OC    SSBEWTSK,SSBEWTSK   EIZI SYSTEM ENQUEUE STATUS                   
         JZ    EXITOK                                                           
         MVC   SCRLTXT,SPACES                                                   
         MVC   SCRLHDR(8),=CL8'EASI#'                                           
         EDIT  (B1,SSBEWTSK),(2,SCRLHDR+5),ALIGN=LEFT,ZERO=NOBLANK              
*                                                                               
         OC    SSBEWTSK+1(3),SSBEWTSK+1                                         
         JZ    EXITOK                                                           
         XR    RF,RF                                                            
         ICM   RF,7,SSBEWTSK+1     SET HOLDING TASK ID                          
         MVC   SCRLTXT(L'TCBID),TCBID-TCBD(RF)                                  
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* WKFILE ENQ INFORMATION - R2=A(SCRLD OUTPUT AREA)                    *         
***********************************************************************         
WENQRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'WKFILE '                                            
         MVC   SCRLTXT(8),=CL8'Not ENQ'                                         
         OC    SSBWKTSK,SSBWKTSK   WKFILE ENQUEUE STATUS                        
         JZ    EXITOK                                                           
         MVC   SCRLTXT,SPACES                                                   
         MVC   SCRLHDR(8),=C'WKFIL#'                                            
         EDIT  (B1,SSBWKTSK),(2,SCRLHDR+6),ALIGN=LEFT,ZERO=NOBLANK              
*                                                                               
         OC    SSBWKTSK+1(3),SSBWKTSK+1                                         
         JZ    EXITOK                                                           
         XR    RF,RF                                                            
         ICM   RF,7,SSBWKTSK+1     SET HOLDING TASK ID                          
         MVC   SCRLTXT(L'TCBID),TCBID-TCBD(RF)                                  
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FACWRK ENQ INFORMATION - R2=A(SCRLD OUTPUT AREA)                    *         
***********************************************************************         
FENQRTN  NTR1                                                                   
         MVC   SCRLHDR,=CL8'FACWRK '                                            
         MVC   SCRLTXT(8),=CL8'Not ENQ'                                         
         OC    SSBFWTSK,SSBFWTSK   FACWRK ENQUEUE STATUS                        
         JZ    EXITOK                                                           
         MVC   SCRLTXT,SPACES                                                   
         MVC   SCRLHDR(8),=CL8'FACWK#'                                          
         EDIT  (B1,SSBFWTSK),(2,SCRLHDR+6),ALIGN=LEFT,ZERO=NOBLANK              
*                                                                               
         OC    SSBFWTSK+1(3),SSBFWTSK+1                                         
         JZ    EXITOK                                                           
         XR    RF,RF                                                            
         ICM   RF,7,SSBFWTSK+1     SET HOLDING TASK ID                          
         MVC   SCRLTXT(L'TCBID),TCBID-TCBD(RF)                                  
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* PRINT QUEUE DISPLAY ROUTINE PRIMERS                                 *         
* *WARNING* DO NOT TOUCH R0 OR ANYTHING ELSE FROM DISPLAY ABOVE!!!!!  *         
***********************************************************************         
PQ1ERTN  LHI   RF,1                                                             
         J     DISPPQ                                                           
*                                                                               
PQ2ERTN  LHI   RF,2                                                             
         J     DISPPQ                                                           
*                                                                               
PQ3ERTN  LHI   RF,3                                                             
         J     DISPPQ                                                           
*                                                                               
PQ4ERTN  LHI   RF,4                                                             
         J     DISPPQ                                                           
*                                                                               
PQ5ERTN  LHI   RF,5                                                             
         J     DISPPQ                                                           
*                                                                               
PQ6ERTN  LHI   RF,6                                                             
         J     DISPPQ                                                           
*                                                                               
PQ7ERTN  LHI   RF,7                                                             
         J     DISPPQ                                                           
*                                                                               
PQ8ERTN  LHI   RF,8                                                             
         J     DISPPQ                                                           
*                                                                               
PQ9ERTN  LHI   RF,9                                                             
         J     DISPPQ                                                           
*                                                                               
PQAERTN  LHI   RF,10                                                            
         J     DISPPQ                                                           
*                                                                               
PQBERTN  LHI   RF,11                                                            
         J     DISPPQ                                                           
*                                                                               
PQCERTN  LHI   RF,12                                                            
         J     DISPPQ                                                           
*                                                                               
PQDERTN  LHI   RF,13                                                            
         J     DISPPQ                                                           
*                                                                               
PQEERTN  LHI   RF,14                                                            
         J     DISPPQ                                                           
*                                                                               
PQFERTN  LHI   RF,15                                                            
         J     DISPPQ                                                           
         EJECT                                                                  
***********************************************************************         
* PQ ENQ INFORMATION - R2=A(SCRLD OUTPUT AREA)                        *         
***********************************************************************         
         USING SCRLD,R2                                                         
DISPPQ   NTR1                                                                   
         ST    RF,FULL             SAVE INDEX NUMBER                            
*                                                                               
         LA    R5,PRTQDTFL         DTF ADDR TABLE                               
         L     R0,FULL                                                          
         BCTR  R0,0                ZERO BASE                                    
         MHI   R0,L'PRTQDTFL                                                    
         AR    R5,R0               INDEX IN                                     
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,7,1(R5)          GET FILE NAME FROM DTF                       
         MVC   SCRLHDR(L'DTFDD),DTFDD-DTFPHD(RF)                                
         MVC   SCRLTXT(8),=CL8'Not ENQ'                                         
*                                                                               
         LA    R4,SSBPQINF                                                      
         L     R0,FULL                                                          
         BCTR  R0,0                ZERO BASE                                    
         MHI   R0,L'SSBPQINF                                                    
         AR    R4,R0               INDEX IN                                     
*                                                                               
         OC    0(L'SSBPQINF,R4),0(R4)                                           
         JZ    EXITOK                                                           
         MVC   SCRLTXT,SPACES                                                   
         MVI   SCRLHDR+5,C'='       WATCH OUT IF PQ NAMES CHANGE                
         EDIT  (1,0(R4)),(2,SCRLHDR+6),ALIGN=LEFT,ZERO=NOBLANK                  
*                                                                               
         CLI   1(R4),0             CHECK IF TCB ID PRESENT                      
         JE    EXITOK              NO                                           
*                                                                               
         L     RF,ATCB             DUMMY UP TASK DISPLAY                        
         AHI   RF,6                                                             
         MVC   SCRLTXT(L'TCBID),TCBID-TCBD(RF)                                  
         MVC   SCRLTXT+6(1),1(R4)                                               
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* WRKF? QUEUE DISPLAY ROUTINE PRIMERS                                 *         
* *WARNING* DO NOT TOUCH R0 OR ANYTHING ELSE FROM DISPLAY ABOVE!!!!!  *         
***********************************************************************         
WF1ERTN  LHI   RF,1                                                             
         J     DISPWF                                                           
*                                                                               
WF2ERTN  LHI   RF,2                                                             
         J     DISPWF                                                           
*                                                                               
WF3ERTN  LHI   RF,3                                                             
         J     DISPWF                                                           
*                                                                               
WF4ERTN  LHI   RF,4                                                             
         J     DISPWF                                                           
*                                                                               
WF5ERTN  LHI   RF,5                                                             
         J     DISPWF                                                           
*                                                                               
WF6ERTN  LHI   RF,6                                                             
         J     DISPWF                                                           
*                                                                               
WF7ERTN  LHI   RF,7                                                             
         J     DISPWF                                                           
*                                                                               
WF8ERTN  LHI   RF,8                                                             
         J     DISPWF                                                           
*                                                                               
WF9ERTN  LHI   RF,9                                                             
         J     DISPWF                                                           
*                                                                               
WFAERTN  LHI   RF,10                                                            
         J     DISPWF                                                           
*                                                                               
WFBERTN  LHI   RF,11                                                            
         J     DISPWF                                                           
*                                                                               
WFCERTN  LHI   RF,12                                                            
         J     DISPWF                                                           
*                                                                               
WFDERTN  LHI   RF,13                                                            
         J     DISPWF                                                           
*                                                                               
WFEERTN  LHI   RF,14                                                            
         J     DISPWF                                                           
*                                                                               
WFFERTN  LHI   RF,15                                                            
         J     DISPWF                                                           
         EJECT                                                                  
***********************************************************************         
* WF ENQ INFORMATION - R2=A(SCRLD OUTPUT AREA)                        *         
***********************************************************************         
         USING SCRLD,R2                                                         
DISPWF   NTR1                                                                   
         ST    RF,FULL             SAVE INDEX NUMBER                            
*                                                                               
         LA    R5,WRKFDTFL         DTF ADDR TABLE                               
         L     R0,FULL                                                          
         BCTR  R0,0                ZERO BASE                                    
         MHI   R0,L'WRKFDTFL                                                    
         AR    R5,R0               INDEX IN                                     
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,7,1(R5)          GET FILE NAME FROM DTF                       
         MVC   SCRLHDR(L'DTFDD),DTFDD-DTFPHD(RF)                                
         MVC   SCRLTXT(8),=CL8'Not ENQ'                                         
*                                                                               
         LA    R4,SSBWFINF                                                      
         L     R0,FULL                                                          
         BCTR  R0,0                ZERO BASE                                    
         MHI   R0,L'SSBWFINF                                                    
         AR    R4,R0               INDEX IN                                     
*                                                                               
         OC    0(L'SSBWFINF,R4),0(R4)                                           
         JZ    EXITOK                                                           
         MVC   SCRLTXT,SPACES                                                   
         MVI   SCRLHDR+5,C'='       WATCH OUT IF WF NAMES CHANGE                
         EDIT  (1,0(R4)),(2,SCRLHDR+6),ALIGN=LEFT,ZERO=NOBLANK                  
*                                                                               
         CLI   1(R4),0             CHECK IF TCB ID PRESENT                      
         JE    EXITOK              NO                                           
*                                                                               
         L     RF,ATCB             DUMMY UP TASK DISPLAY                        
         AHI   RF,6                                                             
         MVC   SCRLTXT(L'TCBID),TCBID-TCBD(RF)                                  
         MVC   SCRLTXT+6(1),1(R4)                                               
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT INTO FIRST PARAMETER FIELD                           *         
***********************************************************************         
VALP1    NTR1                                                                   
         LA    R4,SRVP1H                                                        
         USING FHD,R4                                                           
         ST    R4,ACURSOR                                                       
         CLI   FHIL,0                                                           
         JE    EXITOK                                                           
         LA    RE,SRVP2H                                                        
         ST    RE,ACURSOR                                                       
*                                                                               
         CLI   GLOBAL,YES                                                       
         JE    VP200                                                            
*                                                                               
         CLC   FHDA(2),=CL4'GO  '  INHIBIT/ALLOW INPUT                          
         JNE   VP102                                                            
         NI    SSBSTAT1,255-SSBUII                                              
         MVC   OPCMND,=CL4'GO  '                                                
         J     P1VECHO                                                          
*                                                                               
VP102    CLC   FHDA(4),=CL4'STOP'                                               
         JNE   VP104                                                            
         OI    SSBSTAT1,SSBUII                                                  
         MVC   OPCMND,=CL4'STOP'                                                
         J     P1VECHO                                                          
*                                                                               
VP104    CLC   FHDA(10),=CL10'FACWRKUPD='                                       
         JNE   VP108                                                            
         CLC   FHDA+10(2),=C'ON'   ENABLE UPDATES FROM FACWRK FILES             
         JNE   VP106                                                            
         NI    SSBJFLAG,255-SSBJFNUP                                            
         MVC   OPCMND,=C'FWON'                                                  
         J     P1VECHO                                                          
*                                                                               
VP106    CLC   FHDA+10(3),=C'OFF'  DISABLE UPDATES FROM FACWRK FILES            
         JNE   P1VERR                                                           
         OI    SSBJFLAG,SSBJFNUP                                                
         MVC   OPCMND,=C'FWOF'                                                  
         J     P1VECHO                                                          
*                                                                               
VP108    CLC   FHDA(3),=CL03'POP'  FORCE GLOBAL TIMER POP                       
         JNE   VP110                                                            
         OI    SSBT2,X'80'                                                      
         MVC   OPCMND,=C'POP '                                                  
         J     P1VECHO                                                          
*                                                                               
VP110    CLC   FHDA(8),=CL8'LUNATIC='                                           
         JNE   VP114                                                            
         CLC   FHDA+8(2),=C'ON'    ENABLE LUNATIC TRANMISSIONS                  
         JNE   VP112                                                            
         NI    SSBJFLAG,255-SSBJFNLU                                            
         MVC   OPCMND,=C'LUON'                                                  
         J     P1VECHO                                                          
*                                                                               
VP112    CLC   FHDA+8(3),=C'OFF'   DISABLE LUNATIC TRANSMISSIONS                
         JNE   P1VERR                                                           
         OI    SSBJFLAG,SSBJFNLU                                                
         MVC   OPCMND,=C'LUOF'                                                  
         J     P1VECHO                                                          
*                                                                               
VP114    CLC   FHDA(5),=C'SOON='   SOON=ON/OFF                                  
         JNE   VP118                                                            
         CLC   FHDA+5(2),=C'ON'    ENABLE SOON SUBMISSIONS                      
         JNE   VP116                                                            
         NI    SSBJFLG2,255-SSBJFSN                                             
         MVC   OPCMND,=C'SNON'                                                  
         J     P1VECHO                                                          
*                                                                               
VP116    CLC   FHDA+5(3),=C'OFF'   DISABLE SOON SUBMISSIONS                     
         JNE   P1VERR                                                           
         OI    SSBJFLG2,SSBJFSN                                                 
         MVC   OPCMND,=C'SNOF'                                                  
         J     P1VECHO                                                          
*                                                                               
VP118    CLC   FHDA(8),=C'BACKSOON' RESTORE SOON SUBMISSION                     
         JNE   VP120                                                            
*                                                                               
         ICM   R2,15,SSBAATC       GET A(ATTACHED TASK CONTROL)                 
         JZ    EXITOK              IGNORE IF MONSOON IS IN CONTROL              
         USING ATCD,R2                                                          
         CLI   ATCSTAT1,ATCSATCH+ATCSSACT                                       
         JNE   EXITOK              MUST BE ATTACHED+SYNC ACTIVE                 
         OC    ATCATCB,ATCATCB                                                  
         JNZ   EXITOK              MUST HAVE NO OWNER TCB                       
         MVI   ATCSTAT1,ATCSATCH   OK RESET STAT1 TO ATTACHED (X'80')           
         DROP  R2                                                               
*                                                                               
VP120    CLC   FHDA(4),=C'PAY='    PAY=Y/N                                      
         JNE   VP124                                                            
         CLI   FHDA+4,YES                                                       
         JNE   VP122                                                            
         NI    SSBSTAT3,255-SSBNOPAY                                            
         MVC   OPCMND,=C'PAYY'                                                  
         J     P1VECHO                                                          
*                                                                               
VP122    CLI   FHDA+4,NO                                                        
         JNE   P1VERR                                                           
         OI    SSBSTAT3,SSBNOPAY                                                
         MVC   OPCMND,=C'PAYN'                                                  
         J     P1VECHO                                                          
*                                                                               
VP124    CLC   FHDA(9),=C'DUPDUMPS=' DUPDUMPS=Y/N                               
         JNE   VP128                                                            
         CLI   FHDA+9,YES                                                       
         JNE   VP126                                                            
         OI    SSBSTAT3,SSBDUPDP                                                
         MVC   OPCMND,=C'DUPY'                                                  
         J     P1VECHO                                                          
*                                                                               
VP126    CLI   FHDA+9,NO                                                        
         JNE   P1VERR                                                           
         NI    SSBSTAT3,255-SSBDUPDP                                            
         MVC   OPCMND,=C'DUPN'                                                  
         J     P1VECHO                                                          
*                                                                               
VP128    CLC   FHDA(7),=C'REBUILD' REBUILD CORE                                 
         JNE   VP130                                                            
         OI    SSBSTAT4,SSBNOGO                                                 
         MVC   OPCMND,=C'RBLD'                                                  
         J     P1VECHO                                                          
*                                                                               
VP130    CLC   FHDA(5),=CL05'FAMO='                                             
         JNE   VP134                                                            
         CLC   FHDA+5(2),=C'ON'    ENABLE LUNATIC TRANMISSIONS                  
         JNE   VP132                                                            
         OI    SSBSTAT5,SSB5FAMO                                                
         MVC   OPCMND,=C'FMON'                                                  
         J     P1VECHO                                                          
*                                                                               
VP132    CLC   FHDA+5(3),=C'OFF'   DISABLE LUNATIC TRANSMISSIONS                
         JNE   P1VERR                                                           
         NI    SSBSTAT5,255-SSB5FAMO                                            
         MVC   OPCMND,=C'FMOF'                                                  
         J     P1VECHO                                                          
*                                                                               
VP134    CLC   FHDA(3),=C'CTBUFFER'                                             
         JNE   VP136                                                            
         BRAS  RE,DISCTB                                                        
         J     XMOD                                                             
*                                                                               
VP136    CLC   FHDA(5),=C'ESPIE'                                                
         JNE   VP138                                                            
         OI    SSBSTAT5,SSB5ESPY                                                
         MVC   OPCMND,=C'ESPY'                                                  
         J     P1VECHO                                                          
*                                                                               
VP138    CLC   FHDA(8),=C'MEDDSPC=' MEDDSPC=DISABLE/ENABLE                      
         JNE   VP140                                                            
         CLI   FHDA+8,C'D'                                                      
         JNE   VP138B                                                           
VP138A   XC    SSBMEDTB,SSBMEDTB                                                
         MVC   OPCMND,=C'MOFF'                                                  
         J     P1VECHO                                                          
*                                                                               
VP138B   CLI   FHDA+8,C'E'                                                      
         JNE   VP140                                                            
         XC    IOA,IOA                                                          
         MVC   IOA(4),=C'GETA'                                                  
         MVC   IOA+4(4),SSBMEDTN                                                
         MVC   IOA+8(8),=CL8'MEDDSPC'                                           
*                                                                               
         CLI   SRVP2H+5,0          USE P2 IF IT EXISTS                          
         JE    VP139                                                            
         LA    RE,SRVP3H                                                        
         ST    RE,ACURSOR                                                       
         MVI   IOA+4,C' '          SHOULD BE A 4CHR TABLE NAME                  
         MVC   IOA+5(3),IOA+4                                                   
         LA    R1,SRVP2                                                         
         LLC   RF,SRVP2H+5                                                      
         BCTR  RF,0                                                             
         EXRL  RF,*+10             MOVE IN WHATEVER IS ENTERED                  
         J     *+10                                                             
         MVC   IOA+4(0),0(R1)                                                   
*                                                                               
VP139    LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,IOA                                                           
         SVC   247                                                              
         LTR   RF,RF               TEST OK RETURN                               
         JNZ   VP138A                                                           
         OC    IOA+24(4),IOA+24    TEST ALET FOUND                              
         JZ    VP138A                                                           
         MVC   SSBMEDTB,IOA+20     MOVE IN ORIGIN AND ALET                      
         MVC   SSBMEDTN,IOA+4      DITTO TABLE ID                               
         MVC   OPCMND,=C'MON '                                                  
         J     P1VECHO                                                          
*                                                                               
VP140    CLC   FHDA(4),=CL5'PROT ' TOGGLE PROTECTION MODE                       
         JNE   VP142                                                            
         CLC   FHDA(7),=CL7'PROTOFF'                                            
         JNE   VP141                                                            
         XI    SSBPROT,SSBPROTO                                                 
         MVC   OPCMND,=CL4'POFF'                                                
         J     P1VECHO                                                          
*                                                                               
VP141    XI    SSBPROT,SSBPROTQ                                                 
         MVC   OPCMND,=CL4'PROT'                                                
         TM    SSBPROT,SSBPROTQ                                                 
         JO    *+10                                                             
         MVC   OPCMND,=CL4'WARN'                                                
         J     P1VECHO                                                          
*                                                                               
VP142    CLC   FHDA(5),=CL5'PTASK' TOGGLE TASK PROTECTION MODE                  
         JNE   VP143                                                            
         XI    SSBPROT,SSBPTSKQ                                                 
         MVC   OPCMND,=CL4'PTSK'                                                
         J     P1VECHO                                                          
*                                                                               
VP143    CLC   FHDA(4),=CL4'POLL'  TOGGLE TASK DDLINK POLLING                   
         JNE   VP144                                                            
         XI    SSBSTAT6,SSB6POLL                                                
         MVC   OPCMND,=CL4'POLL'                                                
         J     P1VECHO                                                          
*                                                                               
VP144    CLC   FHDA(4),=CL5'BULK'  TOGGLE TASK BULK UPLOAD                      
         JNE   VP150                                                            
         XI    SSBSTAT5,SSB5BULK                                                
         MVC   OPCMND,=CL4'BULK'                                                
         J     P1VECHO                                                          
*                                                                               
VP150    CLC   FHDA(4),=CL5'HIDE ' TOGGLE HIDDEN CT RECORDS                     
         JNE   VP160                                                            
         XI    SSBSTAT5,SSBCTHID                                                
         MVC   OPCMND,=CL4'HIDE'                                                
         J     P1VECHO                                                          
*                                                                               
VP160    CLC   FHDA(3),=CL5'SMF  ' TOGGLE SMF RECORDS ACTIVE                    
         JNE   VP165                                                            
         XI    SSBSTAT5,SSBADRSM                                                
         MVC   OPCMND,=CL4'SMF '                                                
         J     P1VECHO                                                          
*                                                                               
VP165    CLC   FHDA(3),=CL5'CTIM ' TOGGLE CPUREP IN SRTIM00                     
         JNE   VP170                                                            
         MVI   TOGGLE,YES                                                       
         CLI   SSBCPU,YES                                                       
         JNE   *+8                                                              
         MVI   TOGGLE,NO                                                        
         MVC   SSBCPU,TOGGLE       TOGGLE SWITCH                                
         MVC   OPCMND,=CL4'CTIM'                                                
         J     P1VECHO                                                          
*                                                                               
VP170    CLC   FHDA(5),=C'ALLC='   ALLC=Y/N TO SET ALL CHARACTERS               
         JNE   VP174                                                            
         CLI   FHDA+5,YES                                                       
         JNE   VP172                                                            
         OI    SSBSTAT6,SSB6ALLC                                                
         MVC   OPCMND,=C'ALLY'                                                  
         J     P1VECHO                                                          
*                                                                               
VP172    CLI   FHDA+5,NO                                                        
         JNE   P1VERR                                                           
         NI    SSBSTAT6,255-SSB6ALLC                                            
         MVC   OPCMND,=C'ALLN'                                                  
         J     P1VECHO                                                          
*                                                                               
VP174    DS    0H                                                               
         CLC   FHDA(4),=C'VSAM'                                                 
         JNE   P1VERR                                                           
         BRAS  RE,DISVSAM                                                       
         J     XMOD                DON'T DISPLAY THE REGULAR SSB INFO!          
*                                                                               
VP200    LAM   AR3,AR3,SSBALET                                                  
         XR    R3,R3                                                            
         SAC   512                                                              
         L     R3,DHASSBG-DMDHDR(,R3)                                           
         USING FASSBG,R3                                                        
         CLC   FHDA(4),=C'RCVR'                                                 
         JNE   P1VERR                                                           
         XI    SSGSTAT1,SSGSRCVW                                                
         J     P1VECHO                                                          
*                                                                               
VP210    J     P1VERR                                                           
         DROP  R3                                                               
*                                                                               
P1VERR   SAC   0                                                                
         MVC   SRVMSG+19(36),=CL36'I do not understand the command'             
         J     EXITOK                                                           
*                                                                               
P1VECHO  SAC   0                                                                
         JAS   RE,WTO              CONSOLE COMMAND + ECHO TO SCREEN             
         MVC   SRVMSG+19(16),SRVP1                                              
         MVI   SRVMSG+35,C' '                                                   
         OC    SRVMSG+19(17),SPACES                                             
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* WRITE TO OPERATOR                                                   *         
***********************************************************************         
WTO      NTR1                                                                   
         GOTO1 ATICTOC,DUB,C'SSET' SUSPEND TIMERS                               
         GOTO1 ADMOD000,DMCB,AWCTYPE,OPMSG,OPMSGL,C'LVL1'                       
         GOTO1 ATICTOC,DUB,C'RSET' RESET TIMERS                                 
         MVC   OPMSG,SPACES                                                     
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         USING SRPARMD,R1          R1=A(PARMS)                                  
INIT     NTR1                                                                   
         XC    ACURSOR,ACURSOR                                                  
         L     RE,SRQACOMF         SAVE VARIOUS ENTRYS                          
         USING COMFACSD,RE                                                      
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VDATCON,CDATCON                                                  
         MVC   VGETDAY,CGETDAY                                                  
         MVC   VGETFACT,CGETFACT                                                
         L     RE,SRQASEL                                                       
         LAY   R0,-6(,RE)          IT'S A BXLE TABLE: BACK UP 6 BYTES           
         ST    R0,ASELIST          SAVE A(SELIST)                               
         USING SELISTD,RE                                                       
         L     RE,SEFILES                                                       
         MVC   VFILETAB,0(RE)                                                   
         DROP  RE,R1                                                            
*                                                                               
         MVC   OPMSG,SPACES                                                     
         MVC   OPFACID(8),=C'+FAC???+'                                          
         MVC   OPFACID+4(3),SSBSYSNA                                            
         MVC   OPSSB,=C'=SSB'                                                   
         L     RF,SSBTKADR                                                      
         L     RF,TCBUTL-TCBD(RF)                                               
         MVC   OPLUID,TLUID-UTLD(RF)                                            
*                                                                               
         MVC   SYSID,SSBSYSID       Save off value                              
         MVC   SRVMSG,SPACES                                                    
         MVC   SRVMSG(10),=CL10'XXXXX Fac#'                                     
         EDIT  SSBSYSID,(2,SRVMSG+10),ALIGN=LEFT                                
         LA    R1,SRVMSG+11                                                     
         CLI   0(R1),C' '                                                       
         JE    *+8                                                              
         LA    R1,1(R1)                                                         
*NOP*    MVC   1(3,R1),=C'SSB'                                                  
*                                                                               
         MVC   SRVMSG+0(4),SSBSYSN4                                             
         LLC   R0,SSBSYSIX         AORNUM/ADVNUM                                
         CLI   SRVMSG+3,C' '                                                    
         JNE   *+8                                                              
         MVI   SRVMSG+3,C'/'                                                    
         SRL   R0,4                                                             
         LTR   R0,R0                                                            
         JNZ   *+12                                                             
         LHI   R0,X'A3'            SET TOR LETTER T                             
         J     *+8                                                              
         AHI   R0,X'80'            SET AOR LETTER A-H                           
         STC   R0,SRVMSG+4                                                      
*                                                                               
         MVC   SRVMSG+19(8),=C'Time Now'                                        
         GOTO1 ATICTOC,DMCB,C'SGET'                                             
         MVC   WORK(10),=X'402020204B20204B2020'                                
         ED    WORK(10),DMCB                                                    
         OC    WORK(10),=C'  00.00.00'                                          
         MVC   SRVMSG+28(8),WORK+2                                              
*                                                                               
         MVC   SRVMSG+38(8),=C'SSB Date'                                        
         MVC   SRVMSG+47(8),SSBDATE                                             
         MVC   SRVMSG+57(3),=X'5B9FB1' 1140-US USD/EUR/UKP                      
*                                                                               
         ICM   RE,15,VFILETAB      POINT TO SYSTEM FILES IN DMGR                
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RE),X'01'         FIRST ENTRY MUST BE SERVICE SYSTEM           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LH    R0,2(RE)            SET NUMBER OF FILES IN LIST                  
         LA    RE,4(RE)            POINT TO FIRST FILE IN LIST                  
         LA    R2,PRTQDTFL                                                      
         LA    R3,WRKFDTFL                                                      
INIT02   TM    0(RE),X'10'         TEST FOR PRTQ FILE                           
         JZ    *+14                                                             
         MVC   0(4,R2),4(RE)       SAVE PRTQ FILE DTF ADR                       
         LA    R2,4(R2)                                                         
*                                                                               
         TM    1(RE),X'C0'         TEST FOR WRKF FILE                           
         JZ    *+14                                                             
         MVC   0(4,R3),4(RE)       SAVE WRKF FILE DTF ADR                       
         LA    R3,4(R3)                                                         
*                                                                               
         LA    RE,8(RE)            BUMP TO NEXT FILE IN LIST                    
         JCT   R0,INIT02                                                        
*                                                                               
         GOTO1 VGETFACT,DMCB,(X'80',FULL),F#SYSADS                              
         L     R1,FULL                                                          
         USING SYSADSD,R1                                                       
         MVC   VAORALL,AAORALL                                                  
         DROP  R1                                                               
*                                                                               
         GOTO1 VAORALL,DMCB                                                     
         L     R6,0(R1)            Save of table address                        
*                                                                               
         USING FACALETD,R6                                                      
         SAM31                                                                  
         LA    R0,64               Max table entries see FAAORALL               
INIT10   OC    FACALFAC,FACALFAC                                                
         JZ    *+2                                                              
         CLI   FACALSTD,NO         Is it started?                               
         JE    INIT12              No, so can't check it.                       
         LAM   AR2,AR2,FACALALE    First entry is TOR address space             
         SAC   512                                                              
         L     R2,FACALSSB         TOR SSB                                      
*                                                                               
TOR      USING SSBD,R2                                                          
         MVC   SVSSB,0(R2)                                                      
         CLC   TOR.SSBSYSID,SYSID    Do they match? (Should add to tab)         
         JNE   INIT12                                                           
         MVC   CSSBTTR#,TOR.SSBTTRN# Current transaction count                  
         J     INIT15                                                           
*                                                                               
INIT12   AHI   R6,FACALLEN           Next entry                                 
         JCT   R0,INIT10                                                        
*        DC    H'00'                 Just ignore this then                      
*                                                                               
INIT15   SAM24                                                                  
         BRAS  RE,ARSOFF                                                        
         J     EXITOK                                                           
         DROP  TOR                                                              
         EJECT                                                                  
***********************************************************************         
* CHECK SERVICE REQUEST FIELD FOR INPUT                               *         
***********************************************************************         
CHKSVC   NTR1                                                                   
         CLC   SRVSRV+4(2),=C',D'  =SSB,D TO RETURN THE DUMP SCREEN             
         JNE   CHKS02                                                           
         MVC   WORK(17),SRVSRV                                                  
         XC    DMCB(24),DMCB       LOAD VIRGIN ABEND SCREEN                     
         LA    RE,64(RA)                                                        
         ST    RE,DMCB                                                          
         MVI   DMCB+4,C'R'                                                      
         MVC   DMCB+5(3),$ABEND                                                 
         GOTO1 ACALLOV,DMCB                                                     
         MVC   SRVSRV,WORK                                                      
         J     XMOD                                                             
*                                                                               
CHKS02   DS    0H                                                               
         LARL  RF,MAPTAB           DEFAULT DISPLAY TABLE                        
         ST    RF,AMAPTAB                                                       
*                                                                               
         CLC   SRVSRV+4(2),=C',E'  =SSB,E TO RETURN THE ENQ SCREEN              
         JNE   CHKS03                                                           
*                                                                               
         LARL  RF,ENQTAB           ENQ DISPLAY TABLE                            
         ST    RF,AMAPTAB                                                       
         J     EXITOK                                                           
*                                                                               
CHKS03   CLC   SRVSRV+4(2),=C',G'  =SSB,G TO RETURN THE GLOBAL SSB              
         JNE   CHKS04                                                           
*                                                                               
         LARL  RF,GLOBTAB          GLOBAL SSB TABLE                             
         ST    RF,AMAPTAB                                                       
         MVI   GLOBAL,YES                                                       
         J     EXITOK                                                           
*                                                                               
CHKS04   CLC   SRVSRV+4(4),=C',POP' =SSB,POP TO SET TIMER POP                   
         JNE   CHKS06                                                           
*                                                                               
         LA    R4,SRVP1H           SPOOF P1 FIELD FOR VALP1 ROUTINE             
         USING FHD,R4                                                           
         MVI   FHIL,3                                                           
         MVI   FHOL,3                                                           
         MVC   FHDA(3),=C'POP'                                                  
         OI    FHOI,FHOITR                                                      
         J     EXITOK                                                           
         DROP  R4                                                               
*                                                                               
CHKS06   CLC   SRVSRV+4(8),=C',REBUILD'                                         
         JNE   EXITOK                                                           
*                                                                               
         LA    R4,SRVP1H           SPOOF P1 FIELD FOR VALP1 ROUTINE             
         USING FHD,R4                                                           
         MVI   FHIL,7                                                           
         MVI   FHOL,7                                                           
         MVC   FHDA(7),=C'REBUILD'                                              
         OI    FHOI,FHOITR                                                      
         J     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY CTBUFF STATUS                                               *         
***********************************************************************         
         PUSH  USING                                                            
DISCTB   NTR1                                                                   
*                                                                               
         BRAS  RE,CLRTWA           FIRST CLEAR OUT THE SCREEN                   
*                                                                               
         BRAS  RE,ARSOFF                                                        
         XC    DMCB(4*6),DMCB      GET DATASPACE HEADER FROM TABS               
         MVC   DMCB(4),=AL4(DTCTB)                                              
         MVI   DMCB,X'20'          SET ENQUIRE                                  
         GOTO1 ALOCKSPC,DMCB                                                    
*                                                                               
         L     RF,4(R1)            SAVE DSPACE HEADER                           
         USING DMSPACED,RF                                                      
         NC    DSPTFRST,=XL4'3FFFFFFF'                                          
         BRAS  RE,ARSOFF                                                        
         LAM   AR3,AR3,SSBTBLET                                                 
         ICM   R3,15,DSPTFRST                                                   
         SAC   512                                                              
         USING CTBUFFD,R3                                                       
*                                                                               
         LA    R2,SRVL3H                                                        
         USING FHD,R2                                                           
         USING CTBTABD,FHDA                                                     
         MVC   CTBTYPE,=CL15'Status'                                            
         MVC   CTBONL,=CL15'Active'                                             
         CLI   CTBUFFID,0                                                       
         JNE   *+14                                                             
         MVC   CTBONL,=CL15'Not Active'                                         
         J     DISCTBX                                                          
*                                                                               
         LLC   RF,FHLN                                                          
         LA    R2,0(RF,R2)                                                      
         MVC   CTBTYPE,=CL15'Last Action'                                       
         MVC   CTBONL(L'CTBUFFLV),CTBUFFLV                                      
         MVC   CTBONL+09(02),=C'at'                                             
         MVC   CTBONL+12(08),CTBUFFTV HH:MM:SS                                  
         MVI   CTBONL+14,C'.'                                                   
         MVI   CTBONL+17,C'.'                                                   
         MVC   CTBONL+22(02),=C'on'                                             
         MVC   CTBONL+26(10),CTBUFFDV YYYY-MM-DD                                
*                                                                               
         LLC   RF,FHLN                                                          
         LA    R2,0(RF,R2)                                                      
         MVC   CTBTYPE,=CL15'Total hits'                                        
         LG    GR0,CTBUFFIV                                                     
         CVDG  GR0,GRUB                                                         
         OC    GRUB(8),GRUB        TEST FOR OVERFLOW                            
         JZ    *+8                                                              
         MVI   CTBTYPE+10,C'?'                                                  
         EDIT  (P8,GRUB+8),(15,CTBONL),0,ZERO=NOBLANK,ALIGN=LEFT                
*                                                                               
         LLC   RF,FHLN                                                          
         LA    R2,0(RF,R2)                                                      
         MVC   CTBTYPE,=CL15'Total misses'                                      
         MVI   CTBONL,C'0'                                                      
         LA    R0,CTBTYPE                                                       
         ST    R0,ATXTMISS         SAVE A(TOTAL MISSES TEXT FIELD)              
         LA    R0,CTBONL                                                        
         ST    R0,ATOTMISS         SAVE A(TOTAL MISSES FIELD)                   
*                                                                               
         LLC   RF,FHLN                                                          
         LA    R2,0(RF,R2)                                                      
         MVC   CTBTYPE,=CL15'CPU Millisecs'                                     
         LG    GR1,CTBUFFCP                                                     
         DSG   GR0,=FD'1000'       CONVERT MICROSECS TO MILLISECS               
         CVDG  GR1,GRUB                                                         
         OC    GRUB(8),GRUB        TEST FOR OVERFLOW                            
         JZ    *+8                                                              
         MVI   CTBTYPE+13,C'?'                                                  
         EDIT  (P8,GRUB+8),(12,CTBONL),ALIGN=LEFT                               
*                                                                               
         LLC   RF,FHLN                                                          
         LA    R2,0(RF,R2)                                                      
         LA    R2,0(RF,R2)                                                      
         MVC   CTBTYPE(CTBTABL),CTBHLINE                                        
*                                                                               
         XC    ONLHITS,ONLHITS                                                  
         XC    OFFHITS,OFFHITS                                                  
         LLC   RF,FHLN                                                          
         LA    R2,0(RF,R2)                                                      
         XC    HALF,HALF                                                        
*                                                                               
         LA    R3,CTBUFFP                                                       
         USING CTBUFFP,R3                                                       
DCTB06   CLI   0(R3),X'FF'                                                      
         JE    DCTB10                                                           
         MVC   DUB(1),CTBUFFPI                                                  
         GOTO1 VHEXOUT,DMCB,DUB,CTBTYPE,1,X'18000000'                           
         LAY   RF,CTRTTAB                                                       
*                                                                               
DCTB07   CLI   0(RF),X'FF'         SEARCH CTFILE RECORD TYPE TABLE              
         JE    DCTB08                                                           
         CLC   0(1,RF),CTBUFFPI                                                 
         JNE   *+14                                                             
         MVC   CTBTYPE+3(12),1(RF)                                              
         J     DCTB08                                                           
         LA    RF,L'CTRTTAB(RF)                                                 
         J     DCTB07                                                           
*                                                                               
DCTB08   LG    GR0,CTBUFFCT        OUTPUT ONLINE NUMBER OF HITS                 
         LG    GR1,ONLHITS                                                      
         AGR   GR1,GR0                                                          
         STG   GR1,ONLHITS                                                      
         CVDG  GR0,GRUB                                                         
         EDIT  (P8,GRUB+8),(15,CTBONL),0,ZERO=NOBLANK                           
*                                                                               
         LG    GR0,CTBUFFOL        OUTPUT OFFLINE NUMBER OF HITS                
         LG    GR1,OFFHITS                                                      
         AGR   GR1,GR0                                                          
         STG   GR1,OFFHITS                                                      
         CVDG  GR0,GRUB                                                         
         EDIT  (P8,GRUB+8),(15,CTBOFFL),0,ZERO=NOBLANK                          
*                                                                               
         LT    R0,CTBUFFMX         OUTPUT MAXIMUM BUFFER SIZE                   
         JZ    DCTB09                                                           
         EDIT  (R0),(10,CTBMAX),0,ZERO=NOBLANK                                  
*                                                                               
         L     R0,CTBUFFCU         OUTPUT CURRENT USED BYTES                    
         EDIT  (R0),(10,CTBUSD),0,ZERO=NOBLANK                                  
*                                                                               
         SR    R0,R0               OUTPUT PECENTAGE FREE                        
         L     R1,CTBUFFMX                                                      
         S     R1,CTBUFFCU                                                      
         M     R0,=F'100'                                                       
         D     R0,CTBUFFMX                                                      
         EDIT  (R1),(3,CTBPCT),0,ZERO=NOBLANK                                   
         MVI   CTBPCT+3,C'%'                                                    
*                                                                               
DCTB09   LLC   RF,FHLN             BUMP TO NEXT SCREEN LINE                     
         LA    R2,0(RF,R2)                                                      
         AHI   R3,CTBUFFPL                                                      
         LH    RF,HALF             BUMP RECORD TYPE LINES                       
         AHI   RF,1                                                             
         STH   RF,HALF                                                          
         CHI   RF,13               MAXIMUM IS 13                                
         JH    DCTB10                                                           
         J     DCTB06                                                           
*                                                                               
DCTB10   MVC   CTBTYPE,=CL15'Total hits'                                        
         LG    GR0,ONLHITS         OUTPUT ONLINE NUMBER OF HITS                 
         CVDG  GR0,GRUB                                                         
         EDIT  (P8,GRUB+8),(15,CTBONL),0,ZERO=NOBLANK                           
         LG    GR0,OFFHITS         OUTPUT OFFLINE NUMBER OF HITS                
         CVDG  GR0,GRUB                                                         
         EDIT  (P8,GRUB+8),(15,CTBOFFL),0,ZERO=NOBLANK                          
*                                                                               
DCTB11   LLC   RF,FHLN                                                          
         LA    R2,0(RF,R2)                                                      
         MVC   CTBTYPE,=CL15'Total misses'                                      
         LG    GR0,CTBUFFCT        OUTPUT ONLINE NUMBER OF MISSES               
         CVDG  GR0,GRUB                                                         
         EDIT  (P8,GRUB+8),(15,CTBONL),0,ZERO=NOBLANK                           
         LG    GR0,CTBUFFOL        OUTPUT OFFLINE NUMBER OF MISSES              
         CVDG  GR0,GRUB                                                         
         EDIT  (P8,GRUB+8),(15,CTBOFFL),0,ZERO=NOBLANK                          
*                                                                               
DCTB12   LG    GR0,CTBUFFCT        OUTPUT TOTAL MISSES                          
         AG    GR0,CTBUFFOL                                                     
         CVDG  GR0,GRUB                                                         
         OC    GRUB(8),GRUB        TEST FOR OVERFLOW                            
         JZ    *+12                                                             
         L     R5,ATXTMISS                                                      
         MVI   12(R5),C'?'                                                      
         L     R5,ATOTMISS                                                      
         EDIT  (P8,GRUB+8),(15,(R5)),0,ZERO=NOBLANK,ALIGN=LEFT                  
*                                                                               
         J     DISCTBX             NOP ONLINE MISS SPECIALS                     
*                                                                               
DCTB13   LTG   GR0,CTBUFFPP+0      OUTPUT X'01' MISSES                          
         JZ    DCTB14                                                           
         LLC   RF,FHLN                                                          
         LA    R2,0(RF,R2)                                                      
         MVC   CTBTYPE(9),=C'01 misses'                                         
         CVDG  GR0,GRUB                                                         
         EDIT  (P8,GRUB+8),(15,CTBONL),0,ZERO=NOBLANK                           
*                                                                               
DCTB14   LTG   GR0,CTBUFFPP+8      OUTPUT X'99' MISSES                          
         JZ    DCTB15                                                           
         LLC   RF,FHLN                                                          
         LA    R2,0(RF,R2)                                                      
         MVC   CTBTYPE(9),=C'99 misses'                                         
         CVDG  GR0,GRUB                                                         
         EDIT  (P8,GRUB+8),(15,CTBONL),0,ZERO=NOBLANK                           
*                                                                               
DCTB15   LTG   GR0,CTBUFFPP+16     OUTPUT X'9B' MISSES                          
         JZ    DCTB16                                                           
         LLC   RF,FHLN                                                          
         LA    R2,0(RF,R2)                                                      
         MVC   CTBTYPE(9),=C'9B misses'                                         
         CVDG  GR0,GRUB                                                         
         EDIT  (P8,GRUB+8),(15,CTBONL),0,ZERO=NOBLANK                           
*                                                                               
DCTB16   LTG   GR0,CTBUFFPP+24     OUTPUT X'F3' MISSES                          
         JZ    DCTB17                                                           
         LLC   RF,FHLN                                                          
         LA    R2,0(RF,R2)                                                      
         MVC   CTBTYPE(9),=C'F3 misses'                                         
         CVDG  GR0,GRUB                                                         
         EDIT  (P8,GRUB+8),(15,CTBONL),0,ZERO=NOBLANK                           
*                                                                               
DCTB17   EQU   *                                                                
*                                                                               
DISCTBX  BRAS  RE,ARSOFF                                                        
         J     EXITOK                                                           
         DROP  R2,R3                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY VSAM STATUS                                                 *         
* ENTER "VSAM" IN P1 TO DISPLAY VSAM INFORMATION.                     *         
* IF P2 IS BLANK, THE VSAM VERSION CONTROL TABLE IS DISPLAYED.        *         
* IF P2 = "STATS", THEN THE TABLE OF VSAM COUNTERS IS DISPLAYED.      *         
* P2 MAY ALSO BE A VERSION CONTROL DISPLAY FILTER:                    *         
*     AGY=     (E.G., AGY=SJ)                                         *         
*     FILE=    (E.G., FILE=N)                                         *         
*     FAC=     (E.G., FAC=ADV1)                                       *         
*     TYPE=    (E.G., TYPE=F)                                         *         
*     SYS=     (E.G., SYS=SPOT)                                       *         
* P3 MAY BE FILLED IN AUTOMATICALLY BY THIS PROGRAM. IT IS USED TO    *         
*    SUPPORT SCROLLING IN THE EVENT THAT THERE ARE TOO MANY ENTRIES   *         
*    TO FIT ON ONE SCREEN.                                            *         
***********************************************************************         
DISVSAM  NTR1                                                                   
*                                                                               
     BRAS RE,CLRTWA                CLEAR DOWN THE TWA                           
                                                                                
* IF P2 IS ABSENT, THEN DIPLAY THE VERSION CONTROL TABLE WITH NO                
* FILTERING. IF P2 IS PRESENT, THEN WE WILL EITHER FILTER THE TABLE,            
* OR DISPLAY THE VSAM STATS (DEPENDING ON THE P2 VALUE).                        
     IF (CLI,SRVP2H+5,EQ,0)    SECOND PARAMETER PRESENT?                        
       JAS RE,DVVRTAB          NO: DISPLAY ENTIRE VERSION CONTROL TABLE         
     ELSE                                                                       
                                                                                
       LA    RE,SRVP3H                                                          
       ST    RE,ACURSOR                                                         
                                                                                
       IF (CLC,=C'STATS',EQ,SRVP2) DISPLAY VSAM STATS?                          
         GOTO1 VGETFACT,DMCB,(X'80',FULL),F#VSAM#S                              
         ICM   R4,15,FULL          A(VSAM COUNTER TABLE)                        
         IF (NZ)                   MAKE SURE WE HAVE A(TABLE)                   
           JAS RE,DVSTATS          DISPLAY THE STATS                            
         ENDIF                                                                  
                                                                                
       ELSEIF (CLC,=C'AGY=',EQ,SRVP2)                                           
         MVC VSAGYFLT,SRVP2+4    SAVE AGENCY FILTER                             
         JAS RE,DVVRTAB                                                         
                                                                                
       ELSEIF (CLC,=C'FILE=',EQ,SRVP2)  FILE FILTER (A/N/R/NTI/PAV)             
         IF (CLI,SRVP2+5,EQ,C'A')                                               
             MVI VSFILFLT,DMVSVDAQ                                              
         ELSEIF (CLI,SRVP2+5,EQ,C'N')                                           
             MVI VSFILFLT,DMVSVDNQ                                              
         ELSEIF (CLI,SRVP2+5,EQ,C'R')                                           
             MVI VSFILFLT,DMVSVDRQ                                              
         ELSEIF (CLC,=C'NTI',EQ,SRVP2+5)                                        
             MVI VSFILFLT,DMVSVNTQ                                              
         ELSEIF (CLC,=C'PAV',EQ,SRVP2+5)                                        
             MVI VSFILFLT,DMVSVPVQ                                              
         ELSE                                                                   
             J   P1VERR          INVALID FILTER: *** ERROR EXIT                 
         ENDIF                                                                  
         JAS RE,DVVRTAB                                                         
                                                                                
       ELSEIF (CLC,=C'FAC=',EQ,SRVP2)                                           
         L    RE,SSBAFID         A(FACIDTAB)                                    
         ICM  R0,15,SRVP2+4      ENTERED FACPAK NAME (E.G., TST)                
         OILF GR0,C'    '        BLANK-PAD FOR COMPARE                          
         DO UNTIL=(CLI,0(RE),EQ,X'FF')  STOP LOOKING IF WE HIT EOT              
           IF (C,R0,EQ,0(,RE))                                                  
             MVC VSFACFLT,4(RE)      SAVE FACPAK # FILTER                       
             ASMLEAVE                                                           
           ENDIF                                                                
           LA    RE,8(,RE)       NEXT TABLE ENTRY                               
         ENDDO                                                                  
         IF (CLI,0(RE),EQ,X'FF') IF WE DIDN'T FIND A TABLE ENTRY...             
           J   P1VERR            INVALID FILTER: *** ERROR EXIT                 
         ENDIF                                                                  
         JAS RE,DVVRTAB                                                         
                                                                                
       ELSEIF (CLC,=C'TYPE=',EQ,SRVP2)                                          
         MVC VSTYPFLT,SRVP2+5    SAVE TYPE FILTER (JUST FIRST CHAR.)            
         JAS RE,DVVRTAB                                                         
                                                                                
       ELSEIF (CLC,=C'SYS=',EQ,SRVP2)                                           
         SELECT CLI,SRVP2+4,EQ   SAVE SYSTEM FILTER                             
           WHEN (C'S')                                                          
             MVI VSSYSFLT,X'02'    SPOT                                         
           WHEN (C'N')                                                          
             MVI VSSYSFLT,X'03'    NET                                          
           WHEN (C'R')                                                          
             MVI VSSYSFLT,X'08'    REP                                          
           OTHRWISE                                                             
             J   P1VERR          INVALID FILTER: *** ERROR EXIT                 
         ENDSEL ,                                                               
         JAS RE,DVVRTAB                                                         
                                                                                
       ELSE                                                                     
         J   P1VERR              INVALID PARAMETER: *** ERROR EXIT              
       ENDIF ,                                                                  
                                                                                
     ENDIF ,                                                                    
                                                                                
     J     EXITOK                                                               
*                                                                               
***********************************************************************         
DVVRTAB NTR1                                                                    
*                                                                               
* WE WILL DISPLAY THE VERSION CONTROL TABLE.                                    
                                                                                
     MVC   SRVMSG+19(36),=CL36'Type STATS in P2 for VSAM counters'              
     BRAS  RE,ARSOFF                                                            
     LAM   AR3,AR3,SSBALET                                                      
     SR    R3,R3               SET R3 TO SSBG                                   
*                                                                               
     SAC   512                 A/R MODE *ON*                                    
*                                                                               
     L     R3,DHASSBG-DMDHDR(,R3)                                               
                                                                                
* START BY DISPLAYING THE DEFAULT SETTINGS FOR EACH FILE.                       
     LA    R3,SSGDMSTA-FASSBG(,R3) VSAM/DANDX FLAG ARRAY                        
     LAY   R4,DISVSNAM         DEMDIR NAME TABLE                                
                                                                                
     LA    R5,SRVL3H           FIRST DISPLAY LINE                               
     USING FHD,R5                                                               
                                                                                
     DO FROM=(R0,=A(DISVSNM#)) PROCESS ALL DEMO FILE SETS                       
       MVC   FHDA(7),0(R4)       DIRECTORY NAME                                 
       MVC   FHDA+10(8),=C'DEFAULT='                                            
       SELECT CLI,0(R3),EQ       EXAMINE SSBG FLAG FOR THIS DEMDIR              
         WHEN (C'D')                                                            
           MVC FHDA+18(5),=CL5'DANDX'                                           
         WHEN (C'V')                                                            
           MVC FHDA+18(5),=CL5'VSAM'                                            
           NEXTWHEN ,                                                           
         WHEN (C'X')                                                            
           MVC FHDA+18(5),=CL5'XVSAM'                                           
           NEXTWHEN ,                                                           
         WHEN (C'V',C'X')                                                       
           OI  FHOI,FHOIHI       HIGHLIGHT NON-DANDX CASES                      
         OTHRWISE                                                               
           MVC FHDA+18(5),=CL5'N/A'                                             
       ENDSEL ,                                                                 
                                                                                
       OI    FHOI,FHOITR         XMIT FIELD                                     
                                                                                
       LLC   RF,FHLN             BUMP TO NEXT SCREEN LINE                       
       AR    R5,RF                                                              
       LA    R4,L'DISVSNAM(,R4)  BUMP TO NEXT DEMDIR TABLE ENTRY                
       LA    R3,1(,R3)           BUMP TO NEXT FLAG                              
     ENDDO ,                                                                    
                                                                                
     LLC   RF,FHLN             BUMP TO NEXT SCREEN LINE                         
     AR    R5,RF                                                                
     MVC   FHDA(46),=C'AGY  FILE(S)        TYPE    FAC   SYS  PGM/SVR'          
     LLC   RF,FHLN             BUMP TO NEXT SCREEN LINE                         
     AR    R5,RF               R5 = A(CURRENT TWA FIELD)                        
                                                                                
* POINT TO THE GLOBAL SSB IN THE DATASPACE.                                     
     SR    R3,R3               SET R3 TO SSBG                                   
     L     R3,DHASSBG-DMDHDR(,R3)                                               
     USING FASSBG,R3                                                            
                                                                                
     MVC   SRVL3+60(17),=C'SMF logging = xxx'                                   
     SELECT CLI,SSGDMSMF,EQ    DISPLAY DEMO DIR. SMF LOGGING STATUS             
       WHEN (YES)                                                               
         MVC  SRVL3+74(3),=CL5'ON '                                             
       OTHRWISE                                                                 
         MVC  SRVL3+74(3),=CL5'OFF'                                             
     ENDSEL ,                                                                   
                                                                                
* NOW POINT TO THE VERSION CONTROL TABLE IN THE DATASPACE.                      
     LA    R3,SSGDMVST         VSAM VERSION CONTROL TABLE                       
     DROP  R3                                                                   
                                                                                
     SR    R0,R0                                                                
     ICM   R0,3,0(R3)          # OF ENTRIES IN TABLE                            
     IF    (NZ)                MAKE SURE THERE ARE SOME TO PROCESS              
                                                                                
       LA  R3,2(,R3)              R3 = A(FIRST TABLE ENTRY)                     
       LHI R2,1                   ASSUME WE'RE STARTING WITH 1ST ENTRY          
       IF (CLI,SRVP3H+5,NE,0),AND,   WAS THE LAST DISPLAY INCOMPLETE?           
          (CLC,=C'NEXT=',EQ,SRVP3)                                              
         PACK DUB,SRVP3+5(3)         YES: PICK UP THE STARTING ENTRY #          
         CVB  R0,DUB                                                            
         LR   R2,R0                  R2 = NEXT ENTRY # TO DISPLAY               
         BCTR R0,0                                                              
         DO  FROM=(R0)                                                          
           LA  R3,DMVSVTLQ(,R3)      SKIP ENTRIES WE'VE ALREADY SHOWN           
         ENDDO                                                                  
         XC  SRVP3,SRVP3             CLEAR P3 AND RETRANSMIT                    
         OI  SRVP3H+(FHOI-FHD),FHOITR                                           
       ENDIF                                                                    
                                                                                
* PROCESS AS MUCH OF THE VERSION CONTROL TABLE AS WILL FIT IN TWA.              
                                                                                
       DO WHILE=(CLI,0(R3),NE,X'FF') STOP AT EOT                                
                                                                                
*        PROCESS ONE TABLE ENTRY. IF THE ENTRY IS NOT FILTERED OUT,             
*        DISPLAY IT, AND BUMP R5 (THE TWA FIELD POINTER).                       
         JAS   RE,PROCESS_ONE_VERSION_CONTROL_ENTRY  ** R5 MAY CHANGE           
         LA    R3,DMVSVTLQ(,R3)    BUMP TO NEXT TABLE ENTRY                     
         AHI   R2,1                INCREMENT ENTRY DISPLAY COUNTER              
                                                                                
         IF    (CLI,0(R5),EQ,0)    END OF TWA?                                  
           DOEXIT (CLI,0(R3),EQ,X'FF') YES: STOP AT EOT                         
           CVD  R2,DUB                 CONTINUE ON NEXT HIT OF ENTER            
           OI   DUB+7,X'0F'                                                     
           MVC  SRVP3(5),=C'NEXT='     P3 = WHERE TO START NEXT SCREEN          
           UNPK SRVP3+5(3),DUB                                                  
           OI   SRVP3H+(FHOI-FHD),FHOITR   XMIT P3                              
           LA   R0,SRVP4H                                                       
           ST   R0,ACURSOR                                                      
           ASMLEAVE ,              STOP DISPLAYING!                             
         ENDIF ,                                                                
       ENDDO                                                                    
     ENDIF ,                       (NOTHING TO PROCESS)                         
                                                                                
     SAC   0                       A/R MODE *OFF*                               
     BRAS  RE,ARSOFF                                                            
*                                                                               
     J     EXITOK                                                               
*                                                                               
*======================================================================         
PROCESS_ONE_VERSION_CONTROL_ENTRY NTR1 ,                                        
                                                                                
     DO                                                                         
       MVC   WORK(DMVSVTLQ),0(R3)  COPY ENTRY TO LOCAL W/S                      
WK     USING DMVSVSNT,WORK                                                      
       LA    R2,FHDA                                                            
       USING VSAMDISD,R2                                                        
                                                                                
* IF A FILTER IS PRESENT IN P2, HONOR IT.                                       
       DOEXIT (CLC,VSAGYFLT,NE,=X'0000'),AND,    AGENCY FILTER                  
              (CLC,WK.DMVSVTAL,NE,=X'0000'),AND,                                
              (CLC,WK.DMVSVTAL,NE,VSAGYFLT)                                     
       MVC BYTE,VSFILFLT                         FILE FILTER                    
       NC  BYTE,WK.DMVSVTFL                      ISOLATE FILE BIT               
       DOEXIT (CLI,VSFILFLT,NE,0),AND,                                          
              (CLI,BYTE,EQ,0)                                                   
       DOEXIT (CLI,VSTYPFLT,NE,0),AND,           TYPE FILTER                    
              (CLI,WK.DMVSVTTY,NE,0),AND,                                       
              (CLC,WK.DMVSVTTY,NE,VSTYPFLT)                                     
       DOEXIT (CLI,VSFACFLT,NE,0),AND,           FACPAK FILTER                  
              (CLI,WK.DMVSVTFI,NE,0),AND,                                       
              (CLC,WK.DMVSVTFI,NE,VSFACFLT)                                     
       DOEXIT (CLI,VSSYSFLT,NE,0),AND,           SYSTEM FILTER                  
              (CLI,WK.DMVSVTSY,NE,0),AND,                                       
              (CLC,WK.DMVSVTSY,NE,VSSYSFLT)                                     
                                                                                
* FORMAT THE DISPLAY LINE.                                                      
       IF (CLC,WK.DMVSVTAL,EQ,=X'0000')  DISPLAY AGENCY ALPHA                   
         MVC  VSAMVTAL(3),=C'ALL'                                               
       ELSE                                                                     
         MVC  VSAMVTAL,WK.DMVSVTAL                                              
       ENDIF ,                                                                  
                                                                                
       JAS RE,DISPLAY_VSAM_FILE_LIST   DISPLAY VSAM FILE LIST                   
*                                                                               
* THE FIELDS HAVE DIFFERENT MEANINGS DEPENDING UPON THE "TYPE" FIELD            
* (FACPAK, RUNNER, OR BATCH).                                                   
       SELECT CLI,WK.DMVSVTTY,EQ       ENTRY TYPE                               
                                                                                
         WHEN (0)                      "ALL" TYPES?                             
           MVC VSAMVTTY,=CL6'ALL'      YES                                      
                                                                                
         WHEN (DMVSVTFQ)      *FACPAK* TYPE:                                    
           MVC VSAMVTTY,=CL6'FACPAK'                                            
           IF (CLI,WK.DMVSVTFI,EQ,0)   ALL FACPAKS?                             
             MVC  VSAMVTFI,=CL4'ALL'   YES                                      
           ELSE                                                                 
             L     RE,SSBAFID          A(FACIDTAB)                              
             LLC   RF,WK.DMVSVTFI      FACPAK ID#                               
             MHI   RF,8                L'FACIDTAB ENTRY                         
             AR    RE,RF               INDEX INTO FACIDTAB                      
             MVC   VSAMVTFI,0(RE)      FACPAK NAME (E.G., ADV1)                 
           ENDIF ,                                                              
                                                                                
           SELECT CLI,WK.DMVSVTSY,EQ   CHECK SYSTEM                             
             WHEN (0)                                                           
               MVC VSAMVTSY,=C'ALL'                                             
             WHEN (2)                                                           
               MVC VSAMVTSY,=C'SPT'                                             
             WHEN (3)                                                           
               MVC VSAMVTSY,=C'NET'                                             
             WHEN (8)                                                           
               MVC VSAMVTSY,=C'REP'                                             
             OTHRWISE                                                           
               MVC VSAMVTSY,=C'???'                                             
           ENDSEL ,                                                             
                                                                                
           IF (CLI,WK.DMVSVTPR,EQ,0)   CHECK PROGRAM                            
             MVC VSAMVTPR,=CL7'ALL'                                             
           ELSEIF (CLC,VSAMVTSY,NE,=C'???')  WAS THE SYSTEM VALID?              
             L     R1,ASELIST          YES: GET A(SELIST)                       
             LH    RE,0(R1)            L'TABLE ENTRY (FOR BXLE)                 
             L     RF,2(R1)            A(LAST BYTE OF TABLE)                    
             LA    R1,6(R1)            A(FIRST TABLE ENTRY)                     
             USING SELISTD,R1                                                   
             CLC   SEOVSYS,WK.DMVSVTSY FIND FIRST PGMLST FOR THIS SYS           
             JE    *+12                                                         
             JXLE  R1,RE,*-10                                                   
             J     *+2                 WHERE IS THE PROGRAM LIST ?!?            
             L     R1,SEPGMS           R1 = A(PROGRAM LIST FOR SYST.)           
             DROP  R1                                                           
                                                                                
             LH    RE,0(R1)            L'TABLE ENTRY (FOR BXLE)                 
             L     RF,2(R1)            A(LAST BYTE OF TABLE)                    
             LA    R1,6(R1)            A(FIRST TABLE ENTRY)                     
             USING PGMLSTD,R1                                                   
             CLC   PGMNUM,WK.DMVSVTPR  LOOK FOR PROGRAM # IN LIST               
             JE    *+14                GOT IT                                   
             JXLE  R1,RE,*-10                                                   
             MVC   VSAMVTPR,=C'???????'  PROGRAM NOT FOUND                      
                                                                                
             IF    (CLC,VSAMVTPR,NE,=C'???????')                                
               MVC   VSAMVTPR,PGMNAME    DISPLAY THE PROGRAM NAME               
             ENDIF ,                                                            
             DROP  R1                                                           
                                                                                
           ENDIF ,                                                              
                                                                                
         WHEN (DMVSVTBQ)      *BATCH* TYPE:                                     
           MVC VSAMVTTY,=CL6'BATCH'                                             
           IF (CLC,WK.DMVSVTRP,EQ,=X'0000')   ALL PROGRAMS?                     
             MVC  VSAMVTRP(3),=C'ALL'                                           
           ELSE                                                                 
             MVC  VSAMVTRP,WK.DMVSVTRP REPORT PROG. CODE                        
           ENDIF ,                                                              
                                                                                
           SELECT CLI,WK.DMVSVTSY,EQ   CHECK SYSTEM                             
             WHEN (0)                                                           
               MVC VSAMVTSY,=C'ALL'                                             
             WHEN (2)                                                           
               MVC VSAMVTSY,=C'SPT'                                             
             WHEN (3)                                                           
               MVC VSAMVTSY,=C'NET'                                             
             WHEN (8)                                                           
               MVC VSAMVTSY,=C'REP'                                             
           ENDSEL ,                                                             
                                                                                
         WHEN (DMVSVTRQ)       *RUNNER* TYPE:                                   
           MVC VSAMVTTY,=CL6'RUNNER'                                            
           IF (CLI,WK.DMVSVTFI,EQ,0)   ANY FACPAK SPECIFIED IN ENTRY?           
             MVC  VSAMVTFI,=CL4'ALL'   NO                                       
           ELSE                                                                 
             L     RE,SSBAFID          A(FACIDTAB)                              
             LLC   RF,WK.DMVSVTFI      FACPAK ID#                               
             MHI   RF,8                L'FACIDTAB ENTRY                         
             AR    RE,RF               INDEX INTO FACIDTAB                      
             MVC   VSAMVTFI,0(RE)      FACPAK NAME (E.G., ADV1)                 
           ENDIF ,                                                              
           IF (CLI,WK.DMVSVTSV,EQ,0)   ALL SERVER TYPES?                        
             MVC VSAMVTSV(3),=C'ALL'   YES                                      
           ELSE                                                                 
             MVC VSAMVTSV,WK.DMVSVTSV  SPECIFIC RUNNER SERVER TYPE              
           ENDIF ,                                                              
                                                                                
       ENDSEL ,                                                                 
                                                                                
       LLC   RF,FHLN             BUMP TO NEXT SCREEN LINE                       
       AR    R5,RF                                                              
                                                                                
     ENDDO                                                                      
                                                                                
     XIT1 REGS=(R5)              RETURN: R5 = A(CURRENT TWA FIELD)              
                                                                                
*======================================================================         
                                                                                
DISPLAY_VSAM_FILE_LIST NTR1 ,                                                   
                                                                                
     DO                                                                         
       LA  RF,VSAMVTFL           DISPLAY FILE LIST                              
       IF (CLI,WK.DMVSVTFL,EQ,0)                                                
         MVC  0(6,RF),=C'*NONE*' NO FILES (NEGATIVE FILTER)                     
         ASMLEAVE ,                                                             
       ENDIF ,                                                                  
       IF (TM,WK.DMVSVTFL,DMVSVDAQ,O)                                           
         MVC  0(2,RF),=C'A/'     DEMDIRA                                        
         LA   RF,2(,RF)                                                         
       ENDIF ,                                                                  
       IF (TM,WK.DMVSVTFL,DMVSVDNQ,O)                                           
         MVC  0(2,RF),=C'N/'     DEMDIRN                                        
         LA   RF,2(,RF)                                                         
       ENDIF ,                                                                  
       IF (TM,WK.DMVSVTFL,DMVSVDRQ,O)                                           
         MVC  0(2,RF),=C'R/'     DEMDIRR                                        
         LA   RF,2(,RF)                                                         
       ENDIF ,                                                                  
       IF (TM,WK.DMVSVTFL,DMVSVNTQ,O)                                           
         MVC  0(4,RF),=C'NTI/'   NTIDIR                                         
         LA   RF,4(,RF)                                                         
       ENDIF ,                                                                  
       IF (TM,WK.DMVSVTFL,DMVSVPVQ,O)                                           
         MVC  0(4,RF),=C'PAV/'   PAVDIR                                         
         LA   RF,4(,RF)                                                         
       ENDIF ,                                                                  
       MVIY  -1(RF),C' '         OVERWRITE LAST SLASH WITH BLANK                
     ENDDO ,                                                                    
*                                                                               
     J   EXITOK                  RETURN                                         
                                                                                
     DROP  WK                                                                   
     DROP  R2                                                                   
     DROP  R5                                                                   
                                                                                
***********************************************************************         
*                                                                               
DVSTATS  NTR1 ,                    DISPLAY VSAM STATS                           
*                                                                               
* UPON ENTRY: R4 = A(VSAM COUNTER TABLE)                                        
*                                                                               
     USING DMVSDEMD,R4                                                          
*                                                                               
VD   USING VSAMDISD,SRVL3                                                       
     EDIT  CTRDREAD,VD.VSAMCTRL,ZERO=NOBLANK                                    
     MVC   VD.VSAMDSCL,=CL25'DIR DMREAD'                                        
     EDIT  CTRFRDHI,VD.VSAMCTRR,ZERO=NOBLANK                                    
     MVC   VD.VSAMDSCR,=CL25'FIL DMRDHI'                                        
                                                                                
VD   USING VSAMDISD,SRVL4                                                       
     EDIT  CTRDRDHI,VD.VSAMCTRL,ZERO=NOBLANK                                    
     MVC   VD.VSAMDSCL,=CL25'DIR DMRDHI'                                        
     EDIT  CTRFRSEQ,VD.VSAMCTRR,ZERO=NOBLANK                                    
     MVC   VD.VSAMDSCR,=CL25'FIL DMRSEQ'                                        
                                                                                
VD   USING VSAMDISD,SRVL5                                                       
     EDIT  CTRDRSEQ,VD.VSAMCTRL,ZERO=NOBLANK                                    
     MVC   VD.VSAMDSCL,=CL25'DIR DMRSEQ'                                        
     EDIT  CTRFRSHI,VD.VSAMCTRR,ZERO=NOBLANK                                    
     MVC   VD.VSAMDSCR,=CL25'FIL DMRSEQ had to READ HI'                         
                                                                                
VD   USING VSAMDISD,SRVL6                                                       
     EDIT  CTRDRSHI,VD.VSAMCTRL,ZERO=NOBLANK                                    
     MVC   VD.VSAMDSCL,=CL25'DIR DMRSEQ RDHI nxt major'                         
     EDIT  CTRFALRD,VD.VSAMCTRR,ZERO=NOBLANK                                    
     MVC   VD.VSAMDSCR,=CL25'FIL rec already in ioarea'                         
                                                                                
VD   USING VSAMDISD,SRVL7                                                       
     EDIT  CTRDNRF,VD.VSAMCTRL,ZERO=NOBLANK                                     
     MVC   VD.VSAMDSCL,=CL25'DIR no rec found'                                  
     EDIT  CTRFEOF,VD.VSAMCTRR,ZERO=NOBLANK                                     
     MVC   VD.VSAMDSCR,=CL25'FIL EOF set'                                       
                                                                                
VD   USING VSAMDISD,SRVL8                                                       
     EDIT  CTRFPAS,VD.VSAMCTRR,ZERO=NOBLANK                                     
     MVC   VD.VSAMDSCR,=CL25'FIL passive reads'                                 
                                                                                
VD   USING VSAMDISD,SRVL9                                                       
     EDIT  CTRDDEL,VD.VSAMCTRL,ZERO=NOBLANK                                     
     MVC   VD.VSAMDSCL,=CL25'DIR deleted recs'                                  
                                                                                
VD   USING VSAMDISD,SRVL10                                                      
     EDIT  CTRDDSK,VD.VSAMCTRL,ZERO=NOBLANK                                     
     MVC   VD.VSAMDSCL,=CL25'DIR skipped deletes'                               
     EDIT  CTRFDSK,VD.VSAMCTRR,ZERO=NOBLANK                                     
     MVC   VD.VSAMDSCR,=CL25'FIL skipped deletes'                               
                                                                                
VD   USING VSAMDISD,SRVL11                                                      
     EDIT  CTRDPAOP,VD.VSAMCTRL,ZERO=NOBLANK                                    
     MVC   VD.VSAMDSCL,=CL25'DIR non-xtnd pasvs (PROD)'                         
     EDIT  CTRFOUTP,VD.VSAMCTRR,ZERO=NOBLANK                                    
     MVC   VD.VSAMDSCR,=CL25'FIL recs. returned (PROD)'                         
                                                                                
VD   USING VSAMDISD,SRVL12                                                      
     EDIT  CTRDPAOT,VD.VSAMCTRL,ZERO=NOBLANK                                    
     MVC   VD.VSAMDSCL,=CL25'DIR non-xtnd pasvs (TEST)'                         
     EDIT  CTRFOUTT,VD.VSAMCTRR,ZERO=NOBLANK                                    
     MVC   VD.VSAMDSCR,=CL25'FIL recs. returned (TEST)'                         
                                                                                
VD   USING VSAMDISD,SRVL13                                                      
     EDIT  CTRDPAEP,VD.VSAMCTRL,ZERO=NOBLANK                                    
     MVC   VD.VSAMDSCL,=CL25'DIR extended pasvs (PROD)'                         
     EDIT  CTRVSHI,VD.VSAMCTRR,ZERO=NOBLANK                                     
     MVC   VD.VSAMDSCR,=CL25'VSM READ HI GETs issued'                           
                                                                                
VD   USING VSAMDISD,SRVL14                                                      
     EDIT  CTRDPAET,VD.VSAMCTRL,ZERO=NOBLANK                                    
     MVC   VD.VSAMDSCL,=CL25'DIR extended pasvs (TEST)'                         
     EDIT  CTRVSSK,VD.VSAMCTRR,ZERO=NOBLANK                                     
     MVC   VD.VSAMDSCR,=CL25'VSM READ SKP GETs issued'                          
                                                                                
VD   USING VSAMDISD,SRVL15                                                      
     EDIT  CTRDPTRP,VD.VSAMCTRL,ZERO=NOBLANK                                    
     MVC   VD.VSAMDSCL,=CL25'DIR file pointers  (PROD)'                         
     EDIT  CTRVSSQ,VD.VSAMCTRR,ZERO=NOBLANK                                     
     MVC   VD.VSAMDSCR,=CL25'VSM READ SEQ GETs issued'                          
                                                                                
VD   USING VSAMDISD,SRVL16                                                      
     EDIT  CTRDPTRT,VD.VSAMCTRL,ZERO=NOBLANK                                    
     MVC   VD.VSAMDSCL,=CL25'DIR file pointers  (TEST)'                         
     EDIT  CTRVSWT,VD.VSAMCTRR,ZERO=NOBLANK                                     
     MVC   VD.VSAMDSCR,=CL25'VSM READs needed a wait'                           
                                                                                
VD   USING VSAMDISD,SRVL17                                                      
     EDIT  CTRVERR,VD.VSAMCTRL,ZERO=NOBLANK                                     
     MVC   VD.VSAMDSCL,=CL25'VSAM errors'                                       
     IF  (CP,CTRVERR,NE,=P'0')                                                  
       OI    SRVL17H+6,FHOIHI    HIGHLIGHT IF NON-ZERO                          
       MVC   VD.VSAMDSCL+17(7),=C'LATEST:'                                      
       LLC   R1,VSERRLST         GET FILE SET #                                 
       BCTR  R1,0                INDEX INTO TABLE                               
       MHI   R1,L'DISVSNAM                                                      
       LAY   R1,DISVSNAM(R1)                                                    
       MVC   VD.VSAMDSCL+25(4),=C'FIL='                                         
       MVC   VD.VSAMDSCL+29(3),7(R1)  DISPLAY FILE SET SHORT NAME               
       MVC   VD.VSAMDSCL+32(5),=C',TYP='                                        
       MVC   VD.VSAMDSCL+37(3),=C'???'  IN CASE FIELD IS INVALID                
       SELECT CLI,VSERRLST+1,EQ  FILE TYPE                                      
         WHEN (C'D')                  DIRECTORY?                                
           MVC  VD.VSAMDSCL+37(3),=C'DIR'                                       
         WHEN (C'F')                  "FILE"?                                   
           MVC  VD.VSAMDSCL+37(3),=C'FIL'                                       
       ENDSEL ,                                                                 
       LLC   R1,VSERRLST+2       DATAMGR ACTION #                               
       MHI   R1,L'DISVSACT       INDEX INTO TABLE                               
       LAY   R1,DISVSACT(R1)                                                    
       MVC   VD.VSAMDSCL+40(6),=C',ACTN='                                       
       MVC   VD.VSAMDSCL+46(4),0(R1)  DATAMGR ACTION                            
       MVC   VD.VSAMDSCL+50(7),=C',FDBK=x'                                      
       GOTO1 VHEXOUT,DMCB,VSERRLST+4,VD.VSAMDSCL+57,4,X'18000000'               
     ENDIF ,                                                                    
                                                                                
VD   USING VSAMDISD,SRVL18                                                      
     EDIT  CTRVRAC,VD.VSAMCTRL,ZERO=NOBLANK                                     
     MVC   VD.VSAMDSCL,=CL25'VSGET: RPL still active'                           
     IF (CP,CTRVRAC,NE,=P'0')                                                   
       OI  SRVL18H+6,FHOIHI    HIGHLIGHT IF NON-ZERO                            
     ENDIF ,                                                                    
     IF  (CP,CTRVERR,NE,=P'0')                                                  
       MVC   VD.VSAMDSCL+17(7),=C'FIRST :'                                      
       LLC   R1,VSERR1ST         GET FILE SET #                                 
       BCTR  R1,0                INDEX INTO TABLE                               
       MHI   R1,L'DISVSNAM                                                      
       LAY   R1,DISVSNAM(R1)                                                    
       MVC   VD.VSAMDSCL+25(4),=C'FIL='                                         
       MVC   VD.VSAMDSCL+29(3),7(R1)  DISPLAY FILE SET SHORT NAME               
       MVC   VD.VSAMDSCL+32(5),=C',TYP='                                        
       MVC   VD.VSAMDSCL+37(3),=C'DIR'                                          
       MVC   VD.VSAMDSCL+37(3),=C'???'  IN CASE FIELD IS INVALID                
       SELECT CLI,VSERR1ST+1,EQ       FILE TYPE                                 
         WHEN (C'D')                    DIRECTORY?                              
           MVC VD.VSAMDSCL+37(3),=C'DIR'                                        
         WHEN (C'F')                    "FILE"?                                 
           MVC VD.VSAMDSCL+37(3),=C'FIL'                                        
       ENDSEL ,                                                                 
       LLC   R1,VSERR1ST+2       DATAMGR ACTION #                               
       MHI   R1,L'DISVSACT       INDEX INTO TABLE                               
       LAY   R1,DISVSACT(R1)                                                    
       MVC   VD.VSAMDSCL+40(6),=C',ACTN='                                       
       MVC   VD.VSAMDSCL+46(4),0(R1)  DATAMGR ACTION                            
       MVC   VD.VSAMDSCL+50(7),=C',FDBK=x'                                      
       GOTO1 VHEXOUT,DMCB,VSERR1ST+4,VD.VSAMDSCL+57,4,X'18000000'               
     ENDIF ,                                                                    
                                                                                
     DROP  R4,VD                                                                
                                                                                
     J    EXITOK                                                                
*                                                                               
DISVSNAM DS    0CL10               ONE-BASED                                    
         DC    CL7'DEMDIRA',CL3'A'                                              
         DC    CL7'DEMDIRN',CL3'N'                                              
         DC    CL7'DEMDIRR',CL3'R'                                              
         DC    CL7'NTIDIR',CL3'NTI'                                             
         DC    CL7'PAVDIR',CL3'PAV'                                             
DISVSNM# EQU   (*-DISVSNAM)/L'DISVSNAM                                          
*                                                                               
DISVSACT DS    0CL4                ZERO-BASED                                   
         DC    CL4'????'                                                        
         DC    CL4'????'                                                        
         DC    CL4'DMRD'           DMREAD                                       
         DC    CL4'RSEQ'           DMRSEQ                                       
         DC    CL4'RDHI'           DMRDHI                                       
*                                                                               
VSAMDISD DSECT                                                                  
*                                                                               
* VSAM COUNTER DISPLAY                                                          
VSAMCTRL DS    CL11                LEFT COLUMN COUNTER VALUE                    
         DS    C                                                                
VSAMDSCL DS    CL25                LEFT COLUMN COUNTER DESCRIPTION              
         DS    CL2                                                              
VSAMCTRR DS    CL11                RIGHT COLUMN COUNTER VALUE                   
         DS    C                                                                
VSAMDSCR DS    CL25                RIGHT COLUMN COUNTER DESCRIPTION             
*                                                                               
* VERSION CONTROL DISPLAY                                                       
         ORG   VSAMCTRL                                                         
VSAMVTAL DS    CL2                 AGENCY ALPHA (E.G. SJ) OR "ALL"              
         DS    C                                                                
         DS    CL2                                                              
VSAMVTFL DS    CL13                FILE LIST (E.G., R/N)                        
         DS    CL2                                                              
VSAMVTTY DS    CL6                 FACPAK/BATCH/RUNNER OR "ALL"                 
         DS    CL2                                                              
VSAMVTFI DS    CL4                 FACPAK ID (E.G. ADV1) OR "ALL"               
         DS    CL2                                                              
VSAMVTSY DS    CL3                 SYSTEM (E.G., SPT) OR "ALL"                  
         DS    CL2                                                              
VSAMVTPR DS    0CL7                PROGRAM (E.G. BUY) OR "ALL"                  
VSAMVTSV DS    0CL1                RUNNER SERVER (E.G. T) OR "ALL"              
VSAMVTRP DS    0CL2                BATCH REPORT PROGRAM (OR "ALL")              
         DS    CL(L'VSAMVTPR)                                                   
         ORG                                                                    
         EJECT                                                                  
SSB      RSECT                                                                  
*                                                                               
***********************************************************************         
* USEFUL ROUTINES                                                     *         
***********************************************************************         
ARSOFF   SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         BR    RE                                                               
*                                                                               
EXITOK   CR    RB,RB                                                            
         J     EXIT                                                             
*                                                                               
EXITL    CLIY  *,255               SET CC NE                                    
         J     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
XMOD     LT    R4,ACURSOR                                                       
         JZ    *+8                                                              
         OI    6(R4),FHOICU                                                     
         L     RD,SAVERD                                                        
         J     EXIT                                                             
*                                                                               
*======================================================================         
CLRTWA   NTR1 ,                    CLEAR DOWN TWA FIELDS                        
*                                                                               
* CLEAR DOWN THE SCREEN                                                         
     LA    R5,SRVL3H                                                            
     USING FHD,R5                                                               
     DO UNTIL=(CLI,0(R5),EQ,0)  FOR EACH TWA FIELD                              
       LLC   RF,FHLN            RF = TWA FIELD LENGTH                           
       LR    R0,RF              SAVE FIELD LENGTH                               
       LHI   R1,FHDAD+1         L'HEADER (PLUS ONE FOR UPCOMING EX)             
       IF (TM,FHAT,FHATXH,O)                                                    
         AHI   R1,FHDAD         ADD L'EXTENSION IF PRESENT                      
       ENDIF ,                                                                  
       SR    RF,R1              RF = L'DATA (PLUS ONE FOR EX)                   
       MVI   FHDA,C' '                                                          
       BCTR  RF,0               MINUS 1 FOR FOR BLANK PROPAGATION               
       EXRL  RF,*+10                                                            
       J     *+10                                                               
       MVC   FHDA+1(0),FHDA     PROPAGATE THE BLANK                             
       OI    FHOI,FHOITR        XMIT THE FIELD                                  
       AR    R5,R0              BUMP TO NEXT TWA FIELD                          
     ENDDO ,                                                                    
     DROP  R5                                                                   
                                                                                
     J     EXITOK                                                               
*                                                                               
         EJECT                                                                  
LITERALS DS    0D                                                               
***********************************************************************         
* LITERAL POOL AND CONSTANTS                                          *         
***********************************************************************         
         LTORG                                                                  
*                                                                               
ARZERO   DC    16F'0'                                                           
SPACES   DC    CL80' '                                                          
$ABEND   DC    X'01FFFF'                                                        
DMREAD   DC    CL8'DMREAD  '                                                    
TEMPSTR  DC    CL8'TEMPSTR '                                                    
TEMPEST  DC    CL8'TEMPEST '                                                    
*                                                                               
CTBHLINE DC    CL48'XX Record Type      Hits-Online    Hits-Offline '           
         DC    CL26' Max-Bytes Used-Bytes Free'                                 
         EJECT                                                                  
***********************************************************************         
* SCREEN MAPPING DISPLAY TABLES                                       *         
***********************************************************************         
MAPTAB   DC    A(SECVRTN,STTMRTN,RSTMRTN,DWTMRTN)   L3                          
         DC    A(USERRTN,CPUURTN,CPUTRTN,TOAORTN)   L4                          
         DC    A(SMFIRTN,DSINRTN,TSKSRTN,SRTMRTN)   L5                          
         DC    A(LOOPRTN,GLOBRTN,TPOPRTN,TSKARTN)   L6                          
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L7                          
         DC    A(LOCORTN,HICORTN,REGNRTN,STPTRTN)   L8                          
         DC    A(XLCORTN,XHCORTN,XRGNRTN,DDSZRTN)   L9                          
         DC    A(DMPNRTN,DMPTRTN,DTIMRTN,DUPERTN)   L10                         
         DC    A(MONSRTN,SOONRTN,FACWRTN,MPAYRTN)   L11                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L12                         
         DC    A(TMPSHDR,MUTLRTN,TPSTRTN,CUTLRTN)   L13                         
         DC    A(DUMYRTN,PGTSRTN,PGTPRTN,MXSSRTN)   L14                         
         DC    A(TPSTHDR,MXTMRTN,CUTMRTN,HITMRTN)   L15                         
         DC    A(DUMYRTN,PGTMRTN,LETPRTN,TBUFRTN)   L16                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L17                         
         DC    A(PQNWRTN,PQIORTN,VTHGRTN,MQIOREQ)   L18                         
         DC    A(MXIORTN,GFRCRTN,WKFLRTN,DAYNRTN)   L19                         
         DC    A(CTFLRTN,CTBURTN,LOCKRTN,ZIPARTN)   L20                         
         DC    A(SMTPRTN,VTAMRTN,PCNTRTN,FAMORTN)   L21                         
         DC    A(MQIORTN,MXMQRTN,CUMQRTN,TDMQRTN)   L22                         
*&&US*&& DC    A(DADARTN,DAPQRTN,EADARTN,ERDARTN)   L23                         
*&&UK*&& DC    A(MEDDRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L23                         
         DC    A(BSAMRTN,HIDERTN,PGMSRTN,POLLRTN)   L24                         
         DC    X'FF'                                                            
*                                                                               
ENQTAB   DC    A(CENQRTN,WENQRTN,FENQRTN,EENQRTN)   L3                          
         DC    A(SENQRTN,MENQRTN,DUMYRTN,DUMYRTN)   L4                          
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L5                          
         DC    A(PQ1ERTN,PQ2ERTN,PQ3ERTN,PQ4ERTN)   L6                          
         DC    A(PQ5ERTN,PQ6ERTN,PQ7ERTN,PQ8ERTN)   L7                          
         DC    A(PQ9ERTN,PQAERTN,PQBERTN,PQCERTN)   L8                          
         DC    A(PQDERTN,PQEERTN,PQFERTN,DUMYRTN)   L9                          
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L10                         
         DC    A(WF1ERTN,WF2ERTN,WF3ERTN,WF4ERTN)   L11                         
         DC    A(WF5ERTN,WF6ERTN,WF7ERTN,WF8ERTN)   L12                         
         DC    A(WF9ERTN,WFAERTN,WFBERTN,WFCERTN)   L13                         
         DC    A(WFDERTN,WFEERTN,WFFERTN,DUMYRTN)   L14                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L15                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L16                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L17                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L18                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L19                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L20                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L21                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L22                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L23                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L24                         
         DC    X'FF'                                                            
*                                                                               
GLOBTAB  DC    A(RECORTN,DUMYRTN,DUMYRTN,DUMYRTN)   L3                          
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L4                          
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L5                          
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L6                          
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L7                          
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L8                          
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L9                          
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L10                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L11                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L12                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L13                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L14                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L15                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L16                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L17                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L18                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L19                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L20                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L21                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L22                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L23                         
         DC    A(DUMYRTN,DUMYRTN,DUMYRTN,DUMYRTN)   L24                         
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* CTFILE RECORD TYPES FOR CTBUFF                                      *         
***********************************************************************         
CTRTTAB  DS    0CL13                                                            
         DC    X'01',CL12'Prog Save   '                                         
         DC    X'99',CL12'Radio Upload'                                         
         DC    X'9B',CL12'Agy/Userid  '                                         
         DC    X'C6',CL12'Security    '                                         
         DC    X'C9',CL12'Userid      '                                         
         DC    X'D7',CL12'Profile Prg '                                         
         DC    X'E3',CL12'Terminal    '                                         
         DC    X'E4',CL12'Profile User'                                         
         DC    X'E6',CL12'Sys list    '                                         
         DC    X'F0',CL12'Security    '                                         
         DC    X'F3',CL12'Fac/IPsubnet'                                         
         DC    X'F5',CL12'Sys access  '                                         
         DC    X'FF'                                                            
*                                                                               
         DROP  R8                                                               
         EJECT                                                                  
***********************************************************************         
* Common equates                                                      *         
***********************************************************************         
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
K        EQU   1024                                                             
***********************************************************************         
* W/S DSECT                                                           *         
***********************************************************************         
WORKD    DSECT                                                                  
GRUB     DS    L                                                                
ONLHITS  DS    D                                                                
OFFHITS  DS    D                                                                
DUB      DS    D                                                                
RELO     DS    F                                                                
FULL     DS    F                                                                
SAVERD   DS    F                                                                
DMCB     DS    6F                                                               
AMAPTAB  DS    A                                                                
VHEXOUT  DS    A                                                                
VDATCON  DS    A                                                                
VGETDAY  DS    A                                                                
VGETFACT DS    A                Get facts                                       
VAORALL  DS    A                Get AOR info                                    
VFILETAB DS    A                                                                
ASELIST  DS    A                                                                
ACURSOR  DS    A                                                                
ATXTMISS DS    A                                                                
ATOTMISS DS    A                                                                
*                                                                               
PRTQDTFL DS    16F                                                              
PRTQDTFX EQU   *                                                                
*                                                                               
WRKFDTFL DS    17F                                                              
WRKFDTFX EQU   *                                                                
*                                                                               
ASSB     DS    A                                                                
AUTL     DS    A                                                                
ATCB     DS    A                                                                
ATICTOC  DS    A                                                                
ADATAMGR DS    A                                                                
ACALLOV  DS    A                                                                
ALOCKSPC DS    A                                                                
AWCTYPE  DS    A                                                                
ADMOD000 DS    A                                                                
*                                                                               
CSSBTTR# DS    F                   Current number of transactions               
SYSID    DS    X                   Facpak id (TOR value)                        
*                                                                               
HALF     DS    H                                                                
BYTE     DS    X                                                                
*                                                                               
TOGGLE   DS    C                   YES/NO TOGGLE                                
GLOBAL   DS    C                                                                
*                                                                               
LEFT     DS    CL10                                                             
WORK     DS    CL20                                                             
IOA      DS    CL80                                                             
*                                                                               
OPMSG    DS    0CL27               +FACPAK+ =SSB STOP                           
OPFACID  DS    CL8                                                              
         DS    CL1                                                              
OPSSB    DS    CL4                                                              
         DS    CL1                                                              
OPCMND   DS    CL4                                                              
         DS    CL1                                                              
OPLUID   DS    CL8                                                              
OPMSGL   EQU   *-OPMSG                                                          
*                                                                               
VSAGYFLT DS    CL2                 VSAM AGENCY FILTER                           
VSFILFLT DS    X                   VSAM FILE FILTER                             
VSFACFLT DS    AL1                 VSAM FACPAK FILTER                           
VSTYPFLT DS    C                   VSAM TYPE FILTER (F/B/R)                     
VSSYSFLT DS    X                   VSAM SYSTEM FILTER (SPOT/NET/REP)            
*                                                                               
SVSSB    DS    CL256                                                            
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* COLUMN ON LINE DSECT                                                *         
***********************************************************************         
SCRLD    DSECT                                                                  
SCRLHDR  DS    XL8                                                              
         DS    X                                                                
SCRLTXT  DS    XL9                                                              
SCRLGAP  DS    X                   INTER COLUMN GAP IMPLICIT WITH THIS          
SCRLDLQ  EQU   *-SCRLD                                                          
         EJECT                                                                  
***********************************************************************         
* CTBUFFER DISPLAY DSECT                                              *         
***********************************************************************         
CTBTABD  DSECT                                                                  
CTBTYPE  DS    CL15                                                             
         DS    C                                                                
CTBONL   DS    CL15                                                             
         DS    C                                                                
CTBOFFL  DS    CL15                                                             
         DS    C                                                                
CTBMAX   DS    CL10                                                             
         DS    C                                                                
CTBUSD   DS    CL10                                                             
         DS    C                                                                
CTBPCT   DS    CL4                                                              
CTBTABX  DS    0C                                                               
CTBTABL  EQU   *-CTBTABD                                                        
         EJECT                                                                  
***********************************************************************         
* SCREEN DSECT                                                        *         
***********************************************************************         
SRSSBFFD DSECT                                                                  
         DS    CL64                                                             
* SRSSBFFD                                                                      
       ++INCLUDE SRSSBFFD                                                       
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
* FATABSTMS                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSTMS                                                      
         PRINT ON                                                               
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
* DMDSHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDSHDR                                                        
         PRINT ON                                                               
* WKBUFFERD                                                                     
         PRINT OFF                                                              
       ++INCLUDE WKBUFFERD                                                      
         PRINT ON                                                               
* FATABSD                                                                       
         PRINT OFF                                                              
       ++INCLUDE FATABSD                                                        
         PRINT ON                                                               
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
* FATABSDMP                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSDMP                                                      
         PRINT ON                                                               
* CTBUFFERD                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTBUFFERD                                                      
         PRINT ON                                                               
* FAPIGFACD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAPIGFACD                                                      
         PRINT ON                                                               
* DMDTFPH                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
* FATABSDEQU                                                                    
         PRINT OFF                                                              
       ++INCLUDE FATABSDEQU                                                     
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* DMVSDEMCTD                                                                    
         PRINT OFF                                                              
DMVSDEMD DSECT                                                                  
       ++INCLUDE DMVSDEMCTD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015SRSSB00   02/25/20'                                      
         END                                                                    
