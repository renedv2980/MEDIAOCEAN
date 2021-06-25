*          DATA SET PPLNK12    AT LEVEL 054 AS OF 11/06/18                      
*PHASE T41412A                                                                  
PPLNK12  TITLE '- PRINT SYSTEM INITIAL DOWNLOAD'                                
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,REQUEST=*,CODE=CODE,SYSTEM=PRTSYSQ,              +        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED,B#DCMREC,DCM_OUTD,  +        
               B#SDRREC,SDRRECD,B#ACH,ACHTABD,B#CCO,CCOTABD)                    
                                                                                
CODE     DS    0D                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**PL12**,RR=RE                                                 
         USING LP_D,R1                                                          
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
                                                                                
         L     R9,LP_ABLK1         ROOT PASSES A(WORKD) IN BLOCK1               
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     R8,LP_ABLK2         ROOT PASSES A(SAVED) IN BLOCK2               
         USING SAVED,R8            R8=A(SAVED W/S)                              
         L     RA,ATWA             RA=A(TWA)                                    
         USING TWAD,RA                                                          
         USING SECD,SECBLOCK       SECRET BLOCK                                 
                                                                                
         ST    R1,ALP              SAVE A(DDLINK PARAMETER LIST)                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
                                                                                
***********************************************************************         
* INITIALIZE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNPMODE,RRUNSTRQ   TEST FIRST FOR RUN                           
         BNE   RUNREQ                                                           
         MVC   LP_BLKS+((B#ACH-1)*L'LP_BLKS),AIO6                               
         MVC   LP_BLKS+((B#SDRREC-1)*L'LP_BLKS),AIO5                            
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* RUN A DOWNLOAD REQUEST                                              *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         JNE   EXITY                                                            
         MVC   AGY,LP_AGY          SET AGENCY CODE                              
         MVI   NULLBYTE,0                                                       
         MVI   NATNALTY,NAT_USAQ   Default to US                                
         CLC   LP_QMAPN,CFMMED#    TEST CFM MEDIA DOWNLOAD                      
         BE    RUNREQ10                                                         
                                                                                
         XC    SECVALS(SECVALSL),SECVALS                                        
         XC    SECD(256),SECD                                                   
         XC    DMCB(6*4),DMCB                                                   
         GOTOR VSECRET,DMCB,('SECPINIT',SECD),SECLENQ                           
                                                                                
         LA    R2,SECFLDS          R2=A(FIELD SECURITY DISPLACEMENTS)           
         LHI   R0,SECFLDSN         R0=N'SECURITY FIELDS                         
         XC    DMCB(6*4),DMCB                                                   
RUNREQ02 GOTOR VSECRET,DMCB,('SECPFLDP',SECD),1(R2)                             
         BE    RUNREQ04                                                         
         LA    RF,C'Y'             C'Y'=READ ONLY                               
         BH    *+8                                                              
         LA    RF,C'N'             C'N'=NO ACCESS                               
         SR    RE,RE                                                            
         IC    RE,0(R2)            RE=DISPLACEMENT TO SECURITY VALUE            
         LA    RE,SECVALS(RE)                                                   
         STC   RF,0(RE)            YES - DISPLAY AND CHANGE                     
RUNREQ04 AHI   R2,L'SECFLDS        BUMP TO NEXT DISPLACEMENT                    
         BCT   R0,RUNREQ02         DO FOR NUMBER OF SECURITY FIELDS             
*                                                                               
* IF NOT AB 2.0.0.0 THEN DEFAULT TO AB 1.0.0.0 SECURITY                         
*                                                                               
         L     R2,ALP                                                           
         CLI   (LP_VRSN1-LP_D)(R2),X'01'                                        
         BH    *+16                                                             
         LA    R2,SECACT1          USE AB 1.0.0.0 SECURITY                      
         LHI   R0,SECACT1N         R0=N'SECURITY ACTIONS                        
         B     RUNREQ06                                                         
*                                                                               
         LA    R2,SECACTS          USE AB 2.0.0.0 SECURITY                      
         LHI   R0,SECACTSN         R0=N'SECURITY ACTIONS                        
         XC    DMCB(6*4),DMCB                                                   
*                                                                               
RUNREQ06 GOTOR VSECRET,DMCB,('SECPRACT',SECD),(1(R2),2(R2))                     
         BNE   RUNREQ08                                                         
         SR    RE,RE                                                            
         IC    RE,0(R2)            RE=DISPLACEMENT TO SECURITY VALUE            
         LA    RE,SECVALS(RE)                                                   
         MVI   0(RE),C'Y'          SET ACTION IS VALID                          
RUNREQ08 AHI   R2,L'SECACTS        BUMP TO NEXT DISPLACEMENT                    
         BCT   R0,RUNREQ06         DO FOR NUMBER OF SECURITY ACTIONS            
         B     RUNREQ18                                                         
                                                                                
RUNREQ10 MVC   LIMACCS,LP_ACCS                                                  
         LA    R4,C'A'                                                          
         LA    R3,MEDTAB                                                        
         USING MEDTAB,R3           R3=A(MEDIA TABLE)                            
         LHI   R0,MEDTABMX                                                      
                                                                                
RUNREQ12 XC    MEDTAB(MEDTABL),MEDTAB                                           
         LA    R2,IOKEY                                                         
         USING PAGYRECD,R2         R2=A(MEDIA RECORD KEY)                       
         XC    PAGYKEY,PAGYKEY                                                  
         MVC   PAGYKAGY,AGY                                                     
         STC   R4,PAGYKMED                                                      
         MVI   PAGYKRCD,PAGYKIDQ                                                
                                                                                
         LHI   R1,IOHI+IOPRTDIR+IO1                                             
         GOTOR (#IOEXEC,AIOEXEC)                                                
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   PAGYKAGY,AGY                                                     
         BNE   RUNREQ18                                                         
         CLI   PAGYKRCD,PAGYKIDQ                                                
         BE    RUNREQ14                                                         
         IC    R4,PAGYKMED                                                      
         BL    RUNREQ12                                                         
         B     RUNREQ16                                                         
                                                                                
RUNREQ14 LHI   R1,IOGET+IOPRTFIL+IO1                                            
         GOTOR (#IOEXEC,AIOEXEC)                                                
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO1                                                          
         MVC   MEDTCODE,PAGYKMED                                                
         MVC   MEDTNAME,PAGYMED                                                 
         AHI   R3,MEDTABL                                                       
         IC    R4,PAGYKMED                                                      
                                                                                
RUNREQ16 AHI   R4,1                                                             
         BCT   R0,RUNREQ12                                                      
         DC    H'0'                                                             
         DROP  R3                                                               
                                                                                
RUNREQ18 BRAS  RE,GETSTTNG         GET ESIO, ESR SETTING                        
         BRAS  RE,GETOPTCN         GET OPTION CONTROL VALUES                    
         BRAS  RE,GETACTSE         GET ACTION SECURITY                          
         BRAS  RE,GETFCONV         GET FIELD CONTROL VALUES                     
                                                                                
         L     R1,ALP                                                           
         GOTOR LP_APUTO                                                         
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
                                                                                
CFMMED#  DC    AL2(M#CFMMED)       CFM MEDIA DOWNLOAD                           
                                                                                
SECFLDS  DS    0XL2                ** DISPS. TO SECURITY VALUES **              
         DC    AL1(SECRATE-SECVALS,01)                                          
         DC    AL1(SECPREM-SECVALS,02)                                          
         DC    AL1(SECGRSOR-SECVALS,03)                                         
         DC    AL1(SECNETOR-SECVALS,04)                                         
         DC    AL1(SECGLCDO-SECVALS,05)                                         
         DC    AL1(SECNLCDO-SECVALS,06)                                         
         DC    AL1(SECAGCDP-SECVALS,07)                                         
         DC    AL1(SECAGCDA-SECVALS,08)                                         
         DC    AL1(SECCDPCT-SECVALS,09)                                         
         DC    AL1(SECCDAM-SECVALS,10)                                          
         DC    AL1(SECTAXP-SECVALS,11)                                          
         DC    AL1(SECTAXA-SECVALS,12)                                          
         DC    AL1(SECSREP-SECVALS,13)                                          
         DC    AL1(SECBLDDT-SECVALS,20)                                         
         DC    AL1(SECGRSBD-SECVALS,21)                                         
         DC    AL1(SECNETBD-SECVALS,22)                                         
         DC    AL1(SECGLCDB-SECVALS,23)                                         
         DC    AL1(SECNLCDB-SECVALS,24)                                         
         DC    AL1(SECINVNO-SECVALS,25)                                         
         DC    AL1(SECPAYDT-SECVALS,26)                                         
         DC    AL1(SECGRSPD-SECVALS,27)                                         
         DC    AL1(SECNETPD-SECVALS,28)                                         
         DC    AL1(SECGLCDP-SECVALS,29)                                         
         DC    AL1(SECNLCDP-SECVALS,30)                                         
         DC    AL1(SECPAYEE-SECVALS,31)                                         
         DC    AL1(SECCHKNO-SECVALS,32)                                         
         DC    AL1(SECCHKDT-SECVALS,33)                                         
         DC    AL1(SECADNO-SECVALS,40)                                          
         DC    AL1(SECINSTA-SECVALS,41)                                         
         DC    AL1(SECSCLDT-SECVALS,42)                                         
         DC    AL1(SECONSDT-SECVALS,43)                                         
         DC    AL1(SECBBLDT-SECVALS,44)                                         
         DC    AL1(SECPBLDT-SECVALS,45)                                         
         DC    AL1(SECMCLDT-SECVALS,46)                                         
         DC    AL1(SECMCLXD-SECVALS,47)                                         
         DC    AL1(SECRCOMS-SECVALS,50)                                         
         DC    AL1(SECICOMS-SECVALS,51)                                         
         DC    AL1(SECPINSS-SECVALS,52)                                         
         DC    AL1(SECGRSAC-SECVALS,60)                                         
         DC    AL1(SECNETAC-SECVALS,61)                                         
         DC    AL1(SECGLCDA-SECVALS,62)                                         
         DC    AL1(SECNLCDA-SECVALS,63)                                         
         DC    AL1(SECTSTAT-SECVALS,70)                                         
         DC    AL1(SECPAGNT-SECVALS,71)                                         
         DC    AL1(SECTCOMS-SECVALS,72)                                         
         DC    AL1(SECDECIR-SECVALS,80)                                         
         DC    AL1(SECRPNTS-SECVALS,81)                                         
         DC    AL1(SECCUV-SECVALS,82)                                           
         DC    AL1(SECCLE-SECVALS,83)                                           
         DC    AL1(SECREFNO-SECVALS,90)                                         
         DC    AL1(SECPLCST-SECVALS,91)                                         
         DC    AL1(SECEIMPS-SECVALS,92)                                         
         DC    AL1(SECAIMPS-SECVALS,93)                                         
         DC    AL1(SECCLICK-SECVALS,94)                                         
         DC    AL1(SECPVIEW-SECVALS,95)                                         
*                                                                               
         DC    AL1(SECINSCS-SECVALS,96)                                         
         DC    AL1(SECINSDS-SECVALS,97)                                         
         DC    AL1(SECINSMS-SECVALS,98)                                         
         DC    AL1(SECTSREC-SECVALS,99)                                         
         DC    AL1(SECINVDT-SECVALS,100)                                        
         DC    AL1(SECINVHC-SECVALS,101)                                        
         DC    AL1(SECINVNM-SECVALS,102)                                        
         DC    AL1(SECINVSP-SECVALS,103)                                        
         DC    AL1(SECINVSR-SECVALS,104)                                        
         DC    AL1(SECINVST-SECVALS,105)                                        
         DC    AL1(SECINVTT-SECVALS,106)                                        
         DC    AL1(SECINVTP-SECVALS,107)                                        
         DC    AL1(SECINVAD-SECVALS,108)                                        
         DC    AL1(SECINVGA-SECVALS,109)                                        
         DC    AL1(SECINVID-SECVALS,110)                                        
         DC    AL1(SECINVIC-SECVALS,111)                                        
         DC    AL1(SECINVNA-SECVALS,112)                                        
         DC    AL1(SECINVNL-SECVALS,113)                                        
         DC    AL1(SECINVPR-SECVALS,114)                                        
         DC    AL1(SECINVRT-SECVALS,115)                                        
         DC    AL1(SECINVSD-SECVALS,116)                                        
         DC    AL1(SECINSNP-SECVALS,117)                                        
         DC    AL1(SECINSGP-SECVALS,118)                                        
         DC    AL1(SECINSSD-SECVALS,119)                                        
*                                                                               
         DC    AL1(SECACPM-SECVALS,120)                                         
         DC    AL1(SECINSDT-SECVALS,121)                                        
         DC    AL1(SECIVCLT-SECVALS,122)                                        
         DC    AL1(SECIVPRD-SECVALS,123)                                        
         DC    AL1(SECIVPUB-SECVALS,124)                                        
         DC    AL1(SECISSNM-SECVALS,125)                                        
         DC    AL1(SEC#IPAN-SECVALS,126)                                        
         DC    AL1(SEC#RPAN-SECVALS,127)                                        
         DC    AL1(SECSFH-SECVALS,128)                                          
         DC    AL1(SECSHPDT-SECVALS,129)                                        
         DC    AL1(SECSHWGS-SECVALS,130)                                        
         DC    AL1(SECSITEL-SECVALS,131)                                        
         DC    AL1(SECTSAPP-SECVALS,132)                                        
         DC    AL1(SECTSSTA-SECVALS,133)                                        
         DC    AL1(SECTREPO-SECVALS,136)                                        
         DC    AL1(SECGST-SECVALS,139)                                          
         DC    AL1(SECPST-SECVALS,140)                                          
         DC    AL1(SEC2NDID-SECVALS,141)                                        
         DC    AL1(SECC2FAC-SECVALS,142)                                        
         DC    AL1(SECC2NET-SECVALS,143)                                        
         DC    AL1(SECC2GRS-SECVALS,144)                                        
         DC    AL1(SECPURO#-SECVALS,145)                                        
         DC    AL1(SECPCNET-SECVALS,146)                                        
         DC    AL1(SECPCGRO-SECVALS,147)                                        
         DC    AL1(SECCCGR1-SECVALS,148)                                        
         DC    AL1(SECCCGR2-SECVALS,149)                                        
         DC    AL1(SECCCGR3-SECVALS,150)                                        
         DC    AL1(SECCCGR4-SECVALS,151)                                        
         DC    AL1(SECCCGR5-SECVALS,152)                                        
         DC    AL1(SECCCGR6-SECVALS,153)                                        
         DC    AL1(SECCCGR7-SECVALS,154)                                        
         DC    AL1(SECCCGR8-SECVALS,155)                                        
         DC    AL1(SEC_ECPM-SECVALS,156)                                        
         DC    AL1(SECIVDCM-SECVALS,157)                                        
         DC    AL1(SECSRCOM-SECVALS,158)                                        
         DC    AL1(SECPVIV#-SECVALS,159)                                        
         DC    AL1(SECC2RAT-SECVALS,160)                                        
*                                                                               
SECFLDSN EQU   (*-SECFLDS)/L'SECFLDS                                            
*                                                                               
SECACT1  DS    0XL3                          SECURITY FOR AB 1.X.X.X.X          
         DC    AL1(SECAADD-SECVALS),AL1(1,1)                                    
         DC    AL1(SECACHA-SECVALS),AL1(1,2)                                    
         DC    AL1(SECADEL-SECVALS),AL1(1,3)                                    
         DC    AL1(SECAULD-SECVALS),AL1(1,6)                                    
         DC    AL1(SECADLD-SECVALS),AL1(1,7)                                    
         DC    AL1(SECAIOR-SECVALS),AL1(1,10)                                   
SECACT1N EQU   (*-SECACT1)/L'SECACT1                                            
*                                                                               
SECACTS  DS    0XL3                                                             
         DC    AL1(SECAADD-SECVALS),AL1(1,1)                                    
         DC    AL1(SECACHA-SECVALS),AL1(1,2)                                    
         DC    AL1(SECADEL-SECVALS),AL1(1,3)                                    
         DC    AL1(SECAIOR-SECVALS),AL1(1,10)                                   
         DC    AL1(SECBESR-SECVALS),AL1(1,14)                                   
         DC    AL1(SECBDUP-SECVALS),AL1(1,15)                                   
         DC    AL1(SECBQIV-SECVALS),AL1(1,16)                                   
         DC    AL1(SECBRIV-SECVALS),AL1(1,17)                                   
         DC    AL1(SECAADI-SECVALS),AL1(2,1)                                    
         DC    AL1(SECACHI-SECVALS),AL1(2,2)                                    
         DC    AL1(SECADLI-SECVALS),AL1(2,3)                                    
         DC    AL1(SECAPAY-SECVALS),AL1(2,11)                                   
         DC    AL1(SECAOVR-SECVALS),AL1(2,12)                                   
         DC    AL1(SECAULD-SECVALS),AL1(3,6)                                    
         DC    AL1(SECADLD-SECVALS),AL1(3,7)                                    
         DC    AL1(SECADRR-SECVALS),AL1(4,13)                                   
SECACTSN EQU   (*-SECACTS)/L'SECACTS                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* READ MEDIA RECORDS AND GET ADDITIONAL CHARGE RATES *                          
***********************************************************************         
                                                                                
GETMED   J     *+12                                                             
         DC    C'*GETMED*'                                                      
         LR    RB,RF                                                            
         USING GETMED,RB                                                        
                                                                                
         LA    R2,IOKEY                                                         
         USING PAGYRECD,R2                                                      
         LA    R0,X'41'                                                         
         CLI   LP_RMODE,LP_RFRST                                                
         BE    GETMED02                                                         
         IC    R0,PAGYKMED                                                      
         AHI   R0,1                                                             
                                                                                
GETMED02 XC    PAGYKEY,PAGYKEY                                                  
         MVC   PAGYKAGY,AGY                                                     
         STC   R0,PAGYKMED                                                      
         MVI   PAGYKRCD,PAGYKIDQ                                                
                                                                                
         LHI   R1,IOHI+IOPRTDIR+IO1                                             
         GOTOR (#IOEXEC,AIOEXEC)                                                
         BNE   GETMEDN                                                          
                                                                                
         CLC   PAGYKAGY,AGY                                                     
         BNE   GETMEDN                                                          
         CLI   PAGYKRCD,PAGYKIDQ                                                
         BE    GETMED04                                                         
         IC    R0,PAGYKMED                                                      
         BL    *+8                                                              
         AHI   R0,1                                                             
         B     GETMED02                                                         
                                                                                
GETMED04 LHI   R1,IOGET+IOPRTFIL+IO1                                            
         GOTOR (#IOEXEC,AIOEXEC)                                                
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO1                                                          
         XC    MEDVALS(MEDVALSL),MEDVALS                                        
         MVC   MEDCODE,PAGYKMED                                                 
                                                                                
         CLI   PAGYNAT,0           Default USA?                                 
         JE    *+18                                                             
         CLI   NATNALTY,NAT_CANQ   Already set to Canada?                       
         JE    *+10                                                             
         MVC   NATNALTY,PAGYNAT    Save nationality                             
                                                                                
         MVC   MEDNAME,SPACES                                                   
         MVC   MEDNAME(L'PAGYMED),PAGYMED                                       
         CLC   =C'INTERACTIV',PAGYMED                                           
         BE    GETMED06                                                         
         CLC   =C'INTERACTV',PAGYMED                                            
         BE    GETMED06                                                         
         CLC   =C'INTRAC',PAGYMED                                               
         BE    GETMED06                                                         
         CLC   =C'OUT OF H',PAGYMED    CHECK FOR VARIOUS OUT OF HOME            
         BE    GETMED08                ENTRIES                                  
         CLC   =C'OUT-OF-H',PAGYMED                                             
         BE    GETMED08                                                         
         CLC   =C'O-O-H',PAGYMED                                                
         BE    GETMED08                                                         
         CLC   =C'OUT O HOME',PAGYMED                                           
         BE    GETMED08                                                         
         CLC   =C'OOH       ',PAGYMED                                           
         BE    GETMED08                                                         
*                                                                               
         B     GETMED10                                                         
*                                                                               
         CLI   MEDCODE,C'V'        National video?                              
         BE    GETMED_V                                                         
         CLI   MEDCODE,C'W'        Local video?                                 
         BE    GETMED_W                                                         
                                                                                
         B     GETMED10                                                         
                                                                                
GETMED06 MVC   MEDNAME(11),=C'INTERACTIVE'                                      
         B     GETMED10                                                         
                                                                                
GETMED_V MVC   MEDNAME(14),=C'NATIONAL VIDEO'                                   
         J     GETMED10                                                         
                                                                                
GETMED_W MVC   MEDNAME(11),=C'LOCAL VIDEO'                                      
         J     GETMED10                                                         
                                                                                
GETMED08 MVC   MEDNAME(11),=C'OUT-OF-HOME'                                      
                                                                                
***********************************************************************         
* BUILD TABLE OF ADDITIONAL CHARGE CODES                              *         
***********************************************************************         
                                                                                
GETMED10 MVC   WORK(L'IOKEY),IOKEY SAVE KEY OF MEDIA RECORD                     
                                                                                
         L     R3,AIO6                                                          
         USING ACHTABD,R3          R3=A(ADDITIONAL CHARGES TABLE)               
         LA    R2,IOKEY                                                         
         USING PSPLRECD,R2                                                      
         SR    R4,R4               R4=N'ADDITIONAL CHARGES                      
                                                                                
         XC    PSPLKEY,PSPLKEY     BUILD KEY OF RECORD                          
         MVC   PSPLKAGY,AGY                                                     
         MVC   PSPLKMED,MEDCODE                                                 
         MVI   PSPLKRCD,PSPLKIDQ                                                
                                                                                
         LHI   R1,IORD+IOPRTDIR+IO1                                             
         GOTOR (#IOEXEC,AIOEXEC)                                                
         BNE   GETMED16                                                         
                                                                                
         LHI   R1,IOGET+IOPRTFIL+IO1                                            
         GOTOR (#IOEXEC,AIOEXEC)                                                
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R1,AIO1                                                          
         LA    R1,PSPLELEM-PSPLRECD(R1)                                         
         USING PSPLELEM,R1                                                      
         SR    RE,RE                                                            
GETMED12 CLI   PSPLELEM,0          TEST END OF RECORD                           
         BE    GETMED16                                                         
         CLI   PSPLELEM,PSPELQ     TEST ADDITIONAL CHARGE ELEMENT               
         BNE   GETMED14                                                         
                                                                                
         MVC   ACHCODE,PSPLCODE                                                 
         MVC   ACHTYPE,PSPLTYPE                                                 
         MVC   ACHDESC,PSPLDESC                                                 
         AHI   R3,ACHTABL          BUMP TO NEXT TABLE ENTRY                     
         AHI   R4,1                INCREMENT N'TABLE ENTRIES                    
                                                                                
GETMED14 IC    RE,PSPLELEM+1       BUMP TO NEXT ELEMENT                         
         AR    R1,RE                                                            
         B     GETMED12                                                         
         DROP  R1                                                               
                                                                                
GETMED16 MVC   ACHCODE,=C'FX'      RESERVED FOREIGN EXCHANGE CHARGE             
         MVI   ACHTYPE,C'M'        DEFAULT TO MEDIA RELATED                     
         MVC   ACHDESC(20),=C'USD TO CAD EXCHG AMT'                             
         AHI   R3,ACHTABL          BUMP TO NEXT TABLE ENTRY                     
         AHI   R4,1                INCREMENT N'TABLE ENTRIES                    
                                                                                
         STCM  R4,3,ACHTABN        SET N'ADDITIONAL CHARGES                     
         DROP  R2,R3                                                            
                                                                                
         MVC   IOKEY,WORK          RESTORE SAVED MEDIA KEY                      
                                                                                
GETMEDY  L     R1,ALP                                                           
         USING LP_D,R1                                                          
         LA    R0,WORKD            RETURN A(SAVED) AS A(ARRAY)                  
         STCM  R0,15,LP_ADATA                                                   
         J     EXITY                                                            
                                                                                
GETMEDN  L     R1,ALP                                                           
         MVI   LP_RMODE,LP_RLAST                                                
         J     EXITY                                                            
         DROP  RB,R1                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* GET EIO, ESR SETUP RECORD VALUE                                     *         
***********************************************************************         
*                                                                               
GETSTTNG NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,STGVALS                                                       
         LHI   RF,STGVALSL                                                      
         XCEFL                                                                  
*                                                                               
         L     R2,ALP                                                           
         CLC   (LP_VRSN1-LP_D)(4,R2),=AL1(03,02,00,12)                          
         JE    EXIT                                                             
         CLC   (LP_VRSN1-LP_D)(4,R2),=AL1(03,02,00,19)                          
         JE    EXIT                                                             
*                                                                               
         MVC   PRVEPEND(L'PRVWEPE1),PRVWEPE1                                    
         MVC   SOAEPEND(L'SOAPEPE1),SOAPEPE1                                    
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,(CXTRAINF-COMFACSD)(RF)                                       
         USING XTRAINFD,RF         Establish extra info                         
         TM    XIAGCOPT,XIAGUAT    UAT agency?                                  
         JNZ   EXIT                                                             
         DROP  RF                                                               
*                                                                               
         CLC   (LP_VRSN1-LP_D)(4,R2),=AL1(03,02,00,17)                          
         BNL   GTSTG40                                                          
*                                                                               
         CLC   TWAUSRID,=H'17'     SJR?                                         
         BE    GTSTG20                                                          
         CLC   TWAUSRID,=H'7389'   PHNY?                                        
         BE    GTSTG20                                                          
         CLC   TWAUSRID,=H'540'    MCNYC?                                       
         BE    GTSTG20                                                          
         CLC   TWAUSRID,=H'12128'  EDITEST?                                     
         BE    GTSTG20                                                          
         J     EXIT                                                             
*                                                                               
GTSTG20  MVI   STGEIO,C'Y'                                                      
         L     R2,ALP                                                           
         CLC   (LP_VRSN1-LP_D)(4,R2),=AL1(03,03,00,02)                          
         JL    EXIT                                                             
         B     GTSTG62                                                          
*                                                                               
GTSTG40  LA    R2,IOKEY                                                         
         LA    R3,MEDCDTB                                                       
         USING SCH1KEYD,R2                                                      
GTSTG44  XC    IOKEY,IOKEY                                                      
         MVC   SCH1AGY,AGY                                                      
         MVC   SCH1MED,0(R3)                                                    
         MVC   SCH1RCD,=AL2(SCH1RCDQ)                                           
         LHI   R1,IOHI+IOPRTDIR+IO1                                             
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLC   IOKEY(SCH1CLT-SCH1KEY),IOKEYSAV                                  
         BE    GTSTG50                                                          
         CLI   0(R3),X'FF'         END OF MEDIA CODE TABLE?                     
         BE    GTSTG60                                                          
         LA    R3,1(R3)                                                         
         B     GTSTG44                                                          
*                                                                               
GTSTG50  GOTOR VGETFACT,DMCB,0                                                  
         L     R2,0(R1)                                                         
         USING FACTSD,R2                                                        
         XC    SOAPENDP,SOAPENDP                                                
         XC    EMALENDP,EMALENDP                                                
         MVC   SOAPENDP(L'TSTSOAPE),TSTSOAPE                                    
         MVC   EMALENDP(L'TSTEMALE),TSTEMALE                                    
*                                                                               
         CLI   FASYSID,001         TST?                                         
         BE    GTSTG54                                                          
         CLI   FASYSID,006         MEL?                                         
         BE    GTSTG54                                                          
         CLI   FASYSID,015         FQA?                                         
         BE    GTSTG54                                                          
         CLI   FASYSID,011         CSC?                                         
         BNE   *+20                                                             
         XC    SOAPENDP,SOAPENDP                                                
         XC    EMALENDP,EMALENDP                                                
         B     GTSTG52H                                                         
*                                                                               
         XC    SOAPENDP,SOAPENDP                                                
         XC    EMALENDP,EMALENDP                                                
         CLC   TWAAGY,=C'U#'                                                    
         BNE   *+14                                                             
         MVC   SOAPENDP(L'ADVU#SOP),ADVU#SOP                                    
         B     GTSTG52M                                                         
         CLC   TWAAGY,=C'WI'                                                    
         BNE   *+14                                                             
         MVC   SOAPENDP(L'ADVWISOP),ADVWISOP                                    
         B     GTSTG52M                                                         
         CLC   TWAAGY,=C'M1'                                                    
         BNE   *+14                                                             
         MVC   SOAPENDP(L'ADVWISOP),ADVWISOP                                    
         B     GTSTG52M                                                         
         CLC   TWAAGY,=C'WT'                                                    
         BNE   *+14                                                             
         MVC   SOAPENDP(L'ADVWISOP),ADVWISOP                                    
         B     GTSTG52M                                                         
         CLC   TWAAGY,=C'WJ'                                                    
         BNE   *+14                                                             
         MVC   SOAPENDP(L'ADVWISOP),ADVWISOP                                    
         B     GTSTG52M                                                         
         CLC   TWAAGY,=C'MC'                                                    
         BNE   *+14                                                             
         MVC   SOAPENDP(L'ADVMCSOP),ADVMCSOP                                    
         B     GTSTG52M                                                         
*                                                                               
GTSTG52H MVC   SOAPENDP(L'ADVSOAPE),ADVSOAPE                                    
*                                                                               
GTSTG52M MVC   EMALENDP(L'ADVEMALE),ADVEMALE                                    
*                                                                               
GTSTG54  B     GTSTG20                                                          
*                                                                               
GTSTG60  L     R2,ALP                                                           
         CLC   (LP_VRSN1-LP_D)(4,R2),=AL1(03,03,00,02)                          
         JL    EXIT                                                             
*                                                                               
GTSTG62  LA    R2,IOKEY                                                         
         LA    R3,MEDCDTB                                                       
         USING SCH2KEYD,R2                                                      
GTSTG64  XC    IOKEY,IOKEY                                                      
         MVC   SCH2AGY,AGY                                                      
         MVC   SCH2MED,0(R3)                                                    
         MVC   SCH2RCD,=AL2(SCH2RCDQ)                                           
         LHI   R1,IOHI+IOPRTDIR+IO1                                             
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLC   IOKEY(SCH2CLT-SCH2KEY),IOKEYSAV                                  
         BE    GTSTG66                                                          
         CLI   0(R3),X'FF'         END OF MEDIA CODE TABLE?                     
         BE    GTSTG70                                                          
         LA    R3,1(R3)                                                         
         B     GTSTG64                                                          
*                                                                               
GTSTG66  GOTOR VGETFACT,DMCB,0                                                  
         L     R2,0(R1)                                                         
         USING FACTSD,R2                                                        
         XC    ESRENDP1,ESRENDP1                                                
         XC    ESRENDP2,ESRENDP2                                                
         MVC   ESRENDP1(L'TSTESRE1),TSTESRE1                                    
         MVC   ESRENDP2(L'TSTESRE2),TSTESRE2                                    
*                                                                               
         CLI   FASYSID,001         TST?                                         
         BE    GTSTG68                                                          
         CLI   FASYSID,006         MEL?                                         
         BE    GTSTG68                                                          
         CLI   FASYSID,015         FQA?                                         
         BE    GTSTG68                                                          
         CLI   FASYSID,011         CSC?                                         
         BNE   *+20                                                             
         XC    ESRENDP1,ESRENDP1                                                
         XC    ESRENDP2,ESRENDP2                                                
         B     GTSTG67H                                                         
*                                                                               
         XC    ESRENDP1,ESRENDP1                                                
         XC    ESRENDP2,ESRENDP2                                                
         CLC   TWAAGY,=C'U#'                                                    
         BNE   *+14                                                             
         MVC   ESRENDP1(L'ADVU#SR1),ADVU#SR1                                    
         B     GTSTG67M                                                         
         CLC   TWAAGY,=C'WI'                                                    
         BNE   *+14                                                             
         MVC   ESRENDP1(L'ADVWISR1),ADVWISR1                                    
         B     GTSTG67M                                                         
         CLC   TWAAGY,=C'M1'                                                    
         BNE   *+14                                                             
         MVC   ESRENDP1(L'ADVWISR1),ADVWISR1                                    
         B     GTSTG67M                                                         
         CLC   TWAAGY,=C'WT'                                                    
         BNE   *+14                                                             
         MVC   ESRENDP1(L'ADVWISR1),ADVWISR1                                    
         B     GTSTG67M                                                         
         CLC   TWAAGY,=C'WJ'                                                    
         BNE   *+14                                                             
         MVC   ESRENDP1(L'ADVWISR1),ADVWISR1                                    
         B     GTSTG67M                                                         
         CLC   TWAAGY,=C'MC'                                                    
         BNE   *+14                                                             
         MVC   ESRENDP1(L'ADVMCSR1),ADVMCSR1                                    
         B     GTSTG67M                                                         
*                                                                               
GTSTG67H MVC   ESRENDP1(L'ADVESRE1),ADVESRE1                                    
*                                                                               
GTSTG67M MVC   ESRENDP2(L'ADVESRE2),ADVESRE2                                    
*                                                                               
GTSTG68  MVI   STGESR,C'Y'                                                      
         MVC   WSTIMOUT,=AL2(600)  WS TIME-OUT, DEFAULT 60 SECONDS              
* * * *  MVC   EMTIMOUT,=AL2(300)  E-MAIL TIME-OUT, DEFAULT 30 SECONDS          
*                                                                               
         CLI   WSTIMOUT,60         DEFAULT 60 SECONDS?                          
         BNE   *+8                                                              
         MVI   WSTIMOUT,0          DON'T SEND IF DEFAULT                        
         CLI   EMTIMOUT,30         DEFAULT 30 SECONDS?                          
         BNE   *+8                                                              
         MVI   EMTIMOUT,0          DON'T SEND IF DEFAULT                        
*                                                                               
GTSTG70  XC    TEMP(12),TEMP                                                    
         MVI   TEMP,C'P'                                                        
         MVC   TEMP+01(3),=C'AB2'                                               
         NI    TEMP,X'BF'          MAKE SYSTEM LOWER CASE                       
         MVC   TEMP+04(L'AGY),AGY                                               
         GOTOR VGETPROF,DMCB,(X'E0',TEMP),TEMP2,VDATAMGR,,WORK2                 
         MVC   EIO_LOGO,TEMP2+0                                                 
         CLI   EIO_LOGO,C'Y'                                                    
         BNE   *+18                                                             
         CLI   WORK2,C'A'                                                       
         BL    *+10                                                             
         MVC   EIOALPHA,WORK2                                                   
         MVC   ESR_LOGO,TEMP2+1                                                 
         CLI   ESR_LOGO,C'Y'                                                    
         BNE   *+18                                                             
         CLI   WORK2,C'A'                                                       
         BL    *+10                                                             
         MVC   ESRALPHA,WORK2                                                   
*                                                                               
         J     EXIT                                                             
*                                                                               
MEDCDTB  DC    C'IMNSTO',X'FF'                                                  
*                                                                               
TSTSOAPE DC    C'http://webio-tst-svc.ny.dds.net:80/WebIOServcieTst'            
TSTEMALE DC    C'http://webio-tst.ny.dds.net:80/WebIOTst/InsertionOrder+        
               .jsp?KID='                                                       
*                                                                               
ADVSOAPE DC    C'http://webiosvc.dds.net:80/WebIOServciePrd'                    
ADVU#SOP DC    C'http://webiosvc-m2.dds.net:80/WebIOServciePrd'                 
ADVWISOP DC    C'http://webiosvc-ipgchi.dds.net:80/WebIOServciePrd'             
ADVMCSOP DC    C'http://webiosvc-mcny.dds.net:80/WebIOServciePrd'               
ADVEMALE DC    C'http://webio.dds.net/WebIOPrd/InsertionOrder.jsp?KID='         
*                                                                               
TSTESRE1 DC    C'http://webio-tst-svc.ny.dds.net:80/WebIOServcieTst'            
TSTESRE2 DC    C'http://webio-tst.ny.dds.net:80/WebIOTst/SpaceReservati+        
               on.jsp?KID='                                                     
*                                                                               
ADVESRE1 DC    C'http://webiosvc.dds.net:80/WebIOServciePrd'                    
ADVU#SR1 DC    C'http://webiosvc-m2.dds.net:80/WebIOServciePrd'                 
ADVWISR1 DC    C'http://webiosvc-ipgchi.dds.net:80/WebIOServciePrd'             
ADVMCSR1 DC    C'http://webiosvc-mcny.dds.net:80/WebIOServciePrd'               
ADVESRE2 DC    C'http://webio.dds.net/WebIOPrd/SpaceReservation.jsp?KID+        
               ='                                                               
*                                                                               
SOAPEPE1 DC    C'servlet/rpcrouter'                                             
PRVWEPE1 DC    C'PDF?KID='                                                      
*                                                                               
         LTORG                                                                  
         DROP  RB,R2                                                            
*                                                                               
***********************************************************************         
* GET OPTION CONTROL VALUES                                           *         
***********************************************************************         
*                                                                               
GETOPTCN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    OPTVALS(OPTVALSL),OPTVALS                                        
*                                                                               
         L     R3,ALP                                                           
         CLC   (LP_VRSN1-LP_D)(4,R3),=AL1(3,4,0,1)                              
         BL    GTOPT_X                                                          
*                                                                               
         LA    R2,OPTFLDS          R2=A(OPTION CONTROL DISPLACEMENTS)           
         LHI   R0,OPTFLDSN         R0=N'OPTION CONTROL FIELDS                   
         XC    DMCB(6*4),DMCB                                                   
GTOPT20  GOTOR VSECRET,DMCB,('SECPOPTP',SECD),1(R2)                             
         BNE   GTOPT40                                                          
         SR    RE,RE                                                            
         IC    RE,0(R2)            RE=DISPLACEMENT TO SECURITY VALUE            
         LA    RE,OPTVALS(RE)                                                   
         MVI   0(RE),C'Y'          SET OPTION TO 'YES'                          
GTOPT40  AHI   R2,L'OPTFLDS        BUMP TO NEXT DISPLACEMENT                    
         BCT   R0,GTOPT20          DO FOR NUMBER OF OPTION CONTROLS             
*                                                                               
         GOTOR VGETFACT,DMCB,0                                                  
         L     R2,0(R1)                                                         
         USING FACTSD,R2                                                        
         CLI   FASYSID,011         CSC?                                         
         BNE   GTOPT50                                                          
         MVI   OPTSDEIO,C'Y'       ALWAYS eio VIEW-ONLY ON CSC                  
         MVI   OPTSDESR,C'Y'       ALWAYS ESR VIEW-ONLY ON CSC                  
         DROP  R2                                                               
*                                                                               
GTOPT50  DS    0H                                                               
*                                                                               
GTOPT_X  J     EXIT                                                             
*                                                                               
OPTFLDS  DS    0XL2                ** DISPS. TO OPTION CNTRL VALUES **          
         DC    AL1(OPTSDEIO-OPTVALS,001)                                        
         DC    AL1(OPTSDESR-OPTVALS,002)                                        
         DC    AL1(OPTIMPOR-OPTVALS,003)                                        
OPTFLDSN EQU   (*-OPTFLDS)/L'OPTFLDS                                            
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
GETACTSE NTR1  BASE=*,LABEL=*      GET ACTION SECURITY                          
*                                                                               
         XC    SESTACTS(SESTACLQ),SESTACTS                                      
         XC    SADFACTS(SADFACLQ),SADFACTS                                      
         L     R3,ALP                                                           
         CLC   (LP_VRSN1-LP_D)(4,R3),=AL1(3,4,0,31)                             
         BL    GTEAC20                                                          
         MVC   FULL1(2),=X'041C'   PRINT SFM                                    
         LA    R2,ESTACTBS         R2=A(EST ACTION SEC DISPLACEMENTS)           
         LHI   R0,ESTACTBN         R0=N'ACTIONS                                 
         LHI   R3,L'ESTACTBS       R3=LENGTH OF TABLE ENTRY                     
         BRAS  RE,GET_ACTV                                                      
*                                                                               
GTEAC20  L     R3,ALP                                                           
         CLC   (LP_VRSN1-LP_D)(4,R3),=AL1(3,5,0,41)                             
         BL    GTEAC30                                                          
         MVI   SADF_Y_N,C'N'       SET TO NO FOR AD FILE ACCESS                 
*                                                                               
         XC    HALF1,HALF1                                                      
         GOTOR VGETFACT,DMCB,(2,0),0,0                                          
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'       CHECK IF SECRET CODE IS THERE                
         BZ    *+10                                                             
         MVC   HALF1,FAPASSWD      SAVE PASSWORD ID NUMBER                      
         DROP  R1                                                               
         OC    HALF1,HALF1         HAVE PID?                                    
         BZ    GTEAC30                                                          
*                                                                               
         L     RE,AIO1                                                          
         LHI   RF,4*ONEK                                                        
         XCEFL                                                                  
         L     R2,AIO1                                                          
         USING SA0REC,R2                                                        
         MVI   SA0KTYP,SA0KTYPQ    RECORD TYPE                                  
         MVC   SA0KAGY,SECAGY      Security agency alpha code                   
         MVC   SA0KNUM,HALF1       PERSON AUTH NUMBER (X'FFFF'-X'0000')         
         GOTOR VDATAMGR,DMCB,=C'DMREAD',=C'CTFILE',(R2),(R2)                    
         CLI   8(R1),0                                                          
         BNE   GTEAC30                                                          
         LA    R1,SA0DATA                                                       
         USING SASYSD,R1                                                        
GTEAC22  CLI   SASYSEL,0           END OF RECORD?                               
         BE    GTEAC30                                                          
         CLI   SASYSEL,SASYSELQ                                                 
         BNE   GTEAC24                                                          
         CLI   SASYSNUM,X'04'      PRINT SYSTEM?                                
         BNE   GTEAC24                                                          
         MVC   HALF1,SASYSALL                                                   
         NC    HALF1,=X'000F'                                                   
         CLC   HALF1,=X'000F'      DEFAULT VALUE IS YES?                        
         BNE   *+8                                                              
         MVI   SADF_Y_N,C'Y'       YES, ACCESS TO PROGRAM ALLOWED               
         SR    RF,RF                                                            
         IC    RF,SASYSLN                                                       
         BCTR  RF,0                                                             
         AR    RF,R1               POINT TO END OF PRINT AUTH ELEM              
         LA    RE,SASYSPGM                                                      
GTEAC22H CR    RE,RF               ALL PROGRAM AUTH ENTRIES PROCESSED?          
         BH    GTEAC30                                                          
         CLI   0(RE),P#ADFILE      PROGRAM IS ADFILE?                           
         BNE   GTEAC22M                                                         
         MVI   SADF_Y_N,C'N'       SET TO NO FOR AD FILE ACCESS                 
         MVC   HALF1,1(RE)                                                      
         NC    HALF1,=X'000F'                                                   
         CLC   HALF1,=X'000F'      ACCESS TO PROGRAM ALLOWED?                   
         BNE   GTEAC22M                                                         
         MVI   SADF_Y_N,C'Y'       YES, ACCESS TO PROGRAM ALLOWED               
GTEAC22M LA    RE,L'SASYSPGM(RE)   NEXT PROGRAM AUTH ENTRY                      
         B     GTEAC22H                                                         
GTEAC24  SR    RE,RE                                                            
         IC    RE,SASYSLN                                                       
         AR    R1,RE               POINT TO NEXT ELEM                           
         B     GTEAC22                                                          
*                                                                               
GTEAC30  DS    0H                                                               
*                                                                               
GTEAC_X  J     EXIT                                                             
*                                                                               
GET_ACTV ST    RE,FULL2                                                         
         XC    SECD(256),SECD                                                   
         XC    FULL1+2(2),FULL1+2                                               
         XC    DMCB(6*4),DMCB                                                   
         GOTOR VSECRET,DMCB,('SECPINIT+SECPOSP+SECPOLP',SECD),         *        
               SECLENQ,(0,FULL1)                                                
         BNL   *+6                                                              
         DC    H'0'                CAN'T INITIALIZE SECRET                      
         TM    SECINDS,SECIOLD     OLD SECURITY BEING USED?                     
         BNZ   GACTV_X                                                          
         XC    DMCB(6*4),DMCB                                                   
GACTV10  GOTOR VSECRET,DMCB,('SECPRACT',SECD),(1(R2),2(R2))                     
         BNE   GACTV12                                                          
         SR    RE,RE                                                            
         IC    RE,0(R2)            RE=DISPLACEMENT TO SECURITY VALUE            
         LA    RE,SESTACTS(RE)                                                  
         MVI   0(RE),C'Y'          SET OPTION TO 'YES'                          
GACTV12  AR    R2,R3               BUMP TO NEXT DISPLACEMENT                    
         BCT   R0,GACTV10          DO FOR NUMBER OF ACTIONS                     
GACTV_X  L     RE,FULL2                                                         
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
ESTACTBS DS    0XL3                ** DISPS. TO ACTION SEC VALUES **            
         DC    AL1(SEST_ADD-SESTACTS),AL1(057,001)                              
         DC    AL1(SEST_CHG-SESTACTS),AL1(057,002)                              
         DC    AL1(SEST_DIS-SESTACTS),AL1(057,003)                              
         DC    AL1(SEST_REP-SESTACTS),AL1(057,012)                              
         DC    AL1(SEST_CPY-SESTACTS),AL1(057,013)                              
ESTACTBN EQU   (*-ESTACTBS)/L'ESTACTBS                                          
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
***********************************************************************         
*                                                                               
GETFCONV NTR1  BASE=*,LABEL=*      GET FIELD CONTROL VALUES                     
*                                                                               
         XC    SFMFCONS(SFMFCLNQ),SFMFCONS                                      
         L     R3,ALP                                                           
         CLC   (LP_VRSN1-LP_D)(4,R3),=AL1(3,4,0,31)                             
         BL    GTFCN_X                                                          
*                                                                               
         LA    R2,SESTACTS         POINT TO SFM ACTION SECURITY VALUES          
         LHI   R0,SESTACLQ         R0=N'ACTIONS                                 
GTFCN05  CLI   0(R2),C'Y'          HAVE ACCESS?                                 
         BE    GTFCN10                                                          
         LA    R2,L'SESTACTS(R2)   POINT TO NEXT ACTION SEC VALUE               
         BCT   R0,GTFCN05          DO FOR NUMBER OF ACTIONS                     
         LA    R2,SFMFCONS         POINT TO SFM FIELD CONTROL VALUES            
         LHI   R0,SFMFCLNQ         R0=N'FIELDS                                  
GTFCN07  MVI   0(R2),C'N'          SET TO NO ACCESS                             
         LA    R2,L'SFMFCONS(R2)   POINT TO NEXT FCON SECURITY VALUE            
         BCT   R0,GTFCN07                                                       
         B     GTFCN_X                                                          
*                                                                               
GTFCN10  XC    SECD(256),SECD                                                   
         XC    FULL1,FULL1         GET SFM'S SECURITY VALUES                    
         MVI   FULL1+0,X'04'                                                    
         MVI   FULL1+1,X'1C'                                                    
         XC    DMCB(6*4),DMCB                                                   
         GOTOR VSECRET,DMCB,('SECPINIT+SECPOSP+SECPOLP',SECD),         *        
               SECLENQ,('SECPIFLD+SECPIOPT',FULL1)                              
         BNL   *+6                                                              
         DC    H'0'                CAN'T INITIALIZE SECRET                      
*                                                                               
         LA    R2,FCONTABS         R2=A(EST FCON DISPLACEMENTS)                 
         LHI   R0,FCONTABN         R0=N'FIELDS                                  
GTFCN20  GOTOR VSECRET,DMCB,('SECPFLDP',SECD),1(R2)                             
         BE    GTFCN40                                                          
         LA    RF,C'Y'             C'Y'=READ ONLY                               
         BH    *+8                                                              
         LA    RF,C'N'             C'N'=NO ACCESS                               
         SR    RE,RE                                                            
         IC    RE,0(R2)            RE=DISPLACEMENT TO SECURITY VALUE            
         LA    RE,SFMFCONS(RE)                                                  
         STC   RF,0(RE)                                                         
GTFCN40  AHI   R2,L'FCONTABS       BUMP TO NEXT DISPLACEMENT                    
         BCT   R0,GTFCN20          DO FOR NUMBER OF ACTIONS                     
*                                                                               
GTFCN_X  J     EXIT                                                             
*                                                                               
FCONTABS DS    0XL2                ** DISPS. TO FIELD CONTRL VALUES **          
         DC    AL1(SACTULZE-SFMFCONS,018)                                       
FCONTABN EQU   (*-FCONTABS)/L'FCONTABS                                          
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
***********************************************************************         
* BUILD TABLE OF CUSTOM COLUMNS (USES AIO3 AIO4 AIO5)                 *         
***********************************************************************         
*                                                                               
NXTCCO   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R0,AIO5                                                          
         LHI   R1,CCOTABL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               Clear output block                           
                                                                                
         L     R4,ALP                                                           
         USING LP_D,R4                                                          
         L     R3,AIO5                                                          
         STCM  R3,15,LP_ADATA                                                   
         USING CCOTABD,R3                                                       
         L     R2,IOADDR                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         BNE   NXTCC18                                                          
         XC    IOKEY,IOKEY                                                      
         XC    IOKEYSAV,IOKEYSAV                                                
         XC    TEMP,TEMP           For Standard Column GENDIR key               
         XC    TEMP2,TEMP2                                                      
         LA    RE,IOKEY                                                         
         USING PCOLRECD,RE                                                      
         MVC   PCOLKAGY,AGY                                                     
         MVI   PCOLKMED,C'A'       CUSTOM COL RECS HAVE MED CODE A              
         MVI   PCOLKRCD,PCOLKRCQ                                                
         DROP  RE                                                               
         LHI   R1,IOHI+IOPRTDIR+IO1                                             
         B     NXTCC20                                                          
*                                                                               
NXTCC18  OC    TEMP,TEMP           Have GENDIR key?                             
         JNZ   NXTCC42                                                          
         LHI   R1,IOSQ+IOPRTDIR+IO1                                             
*                                                                               
NXTCC20  GOTOR (#IOEXEC,AIOEXEC)                                                
         BNE   NXTCC40                                                          
         CLC   IOKEY(PCOLKCOD-PCOLKEY),IOKEYSAV                                 
         BNE   NXTCC40                                                          
         LHI   R1,IOGET+IOPRTFIL+IO1                                            
         GOTOR (#IOEXEC,AIOEXEC)                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         LA    R2,(PCOLFRST-PCOLKEY)(R2)                                        
         USING PCOLELEM,R2                                                      
*                                                                               
* For certain conditions, CC data is not replied for AB 1.X.X.X                 
*                                                                               
         CLI   LP_VRSN1,X'01'                                                   
         BH    NXTCC30                                                          
         CLI   PCOLTYP,C'%'        Data type is percent?                        
         BE    NXTCC18             Do seq, ab 1.x.x.x cannot handle it          
         CLI   PCOLTYP,C'P'        Data type is period (date range)?            
         BE    NXTCC18             Do seq, ab 1.x.x.x cannot handle it          
*                                                                               
NXTCC30  BRAS  RE,SETCCDAT         Set Custom Column data                       
         MVC   CCOCODE,IOKEY+(PCOLKCOD-PCOLKEY)                                 
         B     NXTCC_Y             Send formatted Custom Column data            
*                                                                               
NXTCC40  LA    RE,TEMP                                                          
         USING GCOLKEY,RE                                                       
         MVC   GCOLKRID,=AL3(GCOLKRIQ)                                          
         MVI   GCOLKMED,C'A'                                                    
         MVI   GCOLKRCD,PCOLKRCQ                                                
         DROP  RE                                                               
         MVC   TEMP2,TEMP                                                       
         GOTOR VDATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',TEMP,TEMP                    
         B     NXTCC46                                                          
*                                                                               
NXTCC42  MVC   TEMP2,TEMP                                                       
         GOTOR VDATAMGR,DMCB,=C'DMRSEQ',=C'GENDIR',TEMP,TEMP                    
*                                                                               
NXTCC46  CLC   TEMP(GCOLKCOD-GCOLKEY),TEMP2                                     
         BNE   NXTCC_X                                                          
         GOTOR VDATAMGR,DMCB,=C'GETREC',=C'GENFIL',TEMP+36,AIO1,WORK            
         CLI   DMCB+8,0                                                         
         BNE   NXTCC42                                                          
         L     R2,AIO1                                                          
         LA    R2,(GCOLFRST-GCOLKEY)(R2)                                        
         USING PCOLELEM,R2                                                      
*                                                                               
* For certain conditions, CC data is not replied for AB 1.X.X.X                 
*                                                                               
         CLI   LP_VRSN1,X'01'                                                   
         BH    NXTCC48                                                          
         CLI   PCOLTYP,C'%'        Data type is percent?                        
         BE    NXTCC42             Do seq, ab 1.x.x.x cannot handle it          
         CLI   PCOLTYP,C'P'        Data type is period (date range)?            
         BE    NXTCC42             Do seq, ab 1.x.x.x cannot handle it          
*                                                                               
NXTCC48  J     NXTCC50                                                          
*                                                                               
* Table is deactivated, feature is now available for all agencies               
*                                                                               
         CLC   PCOLSQN,=X'2011'    Column is lower than !DESKINVSTAT?           
         JL    NXTCC50                                                          
         CLC   PCOLSQN,=X'2016'    Column is higher than !ABINV$?               
         JH    NXTCC50                                                          
         LAY   RE,CCAGYTAB                                                      
NXTCC48H CLI   0(RE),X'FF'         End of table?                                
         JE    NXTCC42             Do seq, Columns not allowed                  
         CLC   AGY,0(RE)                                                        
         JE    NXTCC50                                                          
         LA    RE,2(RE)            Next agency alpha in table                   
         J     NXTCC48H                                                         
*                                                                               
NXTCC50  BRAS  RE,SETCCDAT         Set Standard Column data                     
*                                                                               
         MVC   CCOCODE,TEMP+(GCOLKCOD-GCOLKEY)                                  
         J     NXTCC_Y                                                          
                                                                                
NXTCC_X  MVI   LP_RMODE,LP_RLAST   NO MORE                                      
                                                                                
NXTCC_Y  MVC   LP_ADATA,AIO5                                                    
         J     EXITY                                                            
                                                                                
CCAGYTAB DC    C'AF'                                                            
         DC    C'ED'                                                            
         DC    C'FG'                                                            
         DC    C'FM'                                                            
         DC    C'FR'                                                            
         DC    C'H0'                                                            
         DC    C'H7'                                                            
         DC    C'HD'                                                            
         DC    C'HY'                                                            
         DC    C'M2'                                                            
         DC    C'PH'                                                            
         DC    C'PM'                                                            
         DC    C'RP'                                                            
         DC    C'SJ'                                                            
         DC    C'TB'                                                            
         DC    C'TD'               Time & Space                                 
         DC    C'U#'                                                            
         DC    C'UB'                                                            
         DC    C'YN'                                                            
         DC    X'FF'               End of Agency Column table                   
                                                                                
         USING PCOLELEM,R2                                                      
SETCCDAT MVC   CCONAME,PCOLDESC    Custom Column name/description               
         MVC   CCODTYPE,PCOLTYP    Custom Column type                           
         MVC   CCOTOTAL,PCOLTOT    Custom Column how to total                   
         MVC   CCOHEAD1,PCOLHDR1   Custom Column heading one                    
         MVC   CCOHEAD2,PCOLHDR2   Custom Column heading two                    
         MVC   CCOSEQ#,PCOLSQN     Custom Column sequence number                
         MVC   CCOLENG,PCOLMLEN    Custom Column max length                     
         MVC   CCODEC#,PCOLDECS    Custom Column # of decimals                  
         MVC   CCGRP_#,PCOLFCON    Custom Column fcon group number              
         MVC   CCRONAB,PCOLRDWR    Custom Column Read Only in AdBuyer           
         XC    CCOMEDS,CCOMEDS     Media codes                                  
         LA    R1,CCOMEDS                                                       
         TM    PCOLMED2,PCOLM_BQ                                                
         BNO   *+12                                                             
         MVI   0(R1),C'B'                                                       
         LA    R1,1(R1)            Move to right                                
         TM    PCOLMED,PCOLM_IQ                                                 
         JNO   *+12                                                             
         MVI   0(R1),C'I'                                                       
         LA    R1,1(R1)            Move to right                                
         TM    PCOLMED,PCOLM_LQ                                                 
         JNO   *+12                                                             
         MVI   0(R1),C'L'                                                       
         LA    R1,1(R1)            Move to right                                
         TM    PCOLMED,PCOLM_MQ                                                 
         JNO   *+12                                                             
         MVI   0(R1),C'M'                                                       
         LA    R1,1(R1)            Move to right                                
         TM    PCOLMED,PCOLM_NQ                                                 
         JNO   *+12                                                             
         MVI   0(R1),C'N'                                                       
         LA    R1,1(R1)            Move to right                                
         TM    PCOLMED,PCOLM_OQ                                                 
         JNO   *+12                                                             
         MVI   0(R1),C'O'                                                       
         LA    R1,1(R1)            Move to right                                
         TM    PCOLMED,PCOLM_SQ                                                 
         JNO   *+12                                                             
         MVI   0(R1),C'S'                                                       
         LA    R1,1(R1)            Move to right                                
         TM    PCOLMED,PCOLM_TQ                                                 
         BNO   *+12                                                             
         MVI   0(R1),C'T'                                                       
         LA    R1,1(R1)            Move to right                                
         TM    PCOLMED2,PCOLM_VQ                                                
         BNO   *+12                                                             
         MVI   0(R1),C'V'                                                       
         LA    R1,1(R1)            Move to right                                
         TM    PCOLMED2,PCOLM_WQ                                                
         BNO   *+12                                                             
         MVI   0(R1),C'W'                                                       
         LA    R1,1(R1)            Move to right                                
         TM    PCOLMED2,PCOLM_DQ                                                
         BNO   *+12                                                             
         MVI   0(R1),C'D'                                                       
         LA    R1,1(R1)            Move to right                                
*                                                                               
         CLI   PCOLMED,PCOLM_AQ    All media before indicator 2?                
         JL    *+16                                                             
         XC    CCOMEDS,CCOMEDS     Retro "ALL" media code adjustment            
         MVC   CCOMEDS,=C'BDILMNOSTVW'                                          
*                                                                               
         MVI   QCCDLIS,0           Init Column drop down list flag              
         CLC   CCOSEQ#,=X'2013'    Column is !ABINVSTAT?                        
         JNE   SETCCD30                                                         
         MVI   QCCDLIS,YESQ        Set to reply drop down list                  
         MVC   CCDLS_01(11),=C'P;Pending;A'                                     
         MVC   CCDLS_02(14),=C'D;Discrepant;A'                                  
         MVC   CCDLS_03(11),=C'M;Matched;A'                                     
         MVC   CCDLS_04(14),=C'N;No Invoice;A'                                  
         MVC   CCDLS_05(14),=C'A;Ad Serving;A'                                  
         MVC   CCDLS_06(07),=C'D;DNR;A'                                         
         MVC   CCDLS_07(10),=C'T;No T/S;A'                                      
         MVC   CCDLS_08(10),=C'R;Rebate;A'                                      
         MVC   CCDLS_09(14),=C'S;Short Rate;A'                                  
         MVC   CCDLS_10(18),=C'F;Circ Shortfall;A'                              
         MVC   CCDLS_11(20),=C'I;Need Revised Inv;A'                            
         MVC   CCDLS_12(11),=C'C;Caption;A'                                     
         MVI   CCDLEND,X'FF'       End of drop down list marker                 
*                                                                               
SETCCD30 CLC   =C'SJ',AGY                                                       
         JNE   SETCCD32                                                         
         CLC   CCOSEQ#,=X'049A'    Column is METDIS?                            
         JE    SETCCD36                                                         
         J     SETCCD40                                                         
SETCCD32 CLC   =C'FM',AGY                                                       
         JNE   SETCCD40                                                         
         CLC   CCOSEQ#,=X'03EA'    Column is METDIS?                            
         JNE   SETCCD40                                                         
SETCCD36 MVI   QCCDLIS,YESQ        Set to reply drop down list                  
         MVC   CCDLS_01(L'METDIS01),METDIS01                                    
         MVC   CCDLS_02(L'METDIS02),METDIS02                                    
         MVC   CCDLS_03(L'METDIS03),METDIS03                                    
         MVC   CCDLS_04(L'METDIS04),METDIS04                                    
         MVC   CCDLS_05(L'METDIS05),METDIS05                                    
         MVC   CCDLS_06(L'METDIS06),METDIS06                                    
         MVC   CCDLS_07(L'METDIS07),METDIS07                                    
         MVC   CCDLS_08(L'METDIS08),METDIS08                                    
         MVC   CCDLS_09(L'METDIS09),METDIS09                                    
         MVC   CCDLS_10(L'METDIS10),METDIS10                                    
         MVC   CCDLS_11(L'METDIS11),METDIS11                                    
         MVC   CCDLS_12(L'METDIS12),METDIS12                                    
         MVI   CCDLEND,X'FF'       End of drop down list marker                 
*                                                                               
SETCCD40 DS    0H                                                               
*                                                                               
SETCCD_X BR    RE                                                               
                                                                                
         LTORG                                                                  
METDIS01 DC    C'01;01 Missing vendor invoice;A'                                
METDIS02 DC    C'02;02 Waiting on client to resolve;A'                          
METDIS03 DC    C'03;03 Waiting on revised invoice from vendor;A'                
METDIS04 DC    C'04;04 Waiting for media team action;A'                         
METDIS05 DC    C'05;05 MC needs to adjust to correct;A'                         
METDIS06 DC    C'06;06 Waiting for vendor to respond;A'                         
METDIS07 DC    C'07;07 Waiting on MC team;A'                                    
METDIS08 DC    C'08;08 Listed as uncleared in error;A'                          
METDIS09 DC    C'09;09 Missing tearsheet;A'                                     
METDIS10 DC    C'10;10 Media Ocean ticket open on issue;A'                      
METDIS11 DC    C'11;11 Held in IPS queue;A'                                     
METDIS12 DC    C'12;12 Specialty communication issues;A'                        
*                                                                               
         DROP  RB,R2,R3,R4                                                      
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
EXITN    LA    RE,1                                                             
         J     EXITCC                                                           
EXITY    SR    RE,RE                                                            
EXITCC   LTR   RE,RE                                                            
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST MAPS FOR INITIAL DOWNLOAD                                   *         
***********************************************************************         
                                                                                
REQINI   LKREQ *,M#DLINI,OUTINI                                                 
                                                                                
REQCFM   LKREQ *,M#CFMMED,OUTCFM                                                
                                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* OUTPUT MAPS FOR INITIAL DOWNLOAD                                    *         
***********************************************************************         
                                                                                
OUTINI   LKOUT H                                                                
                                                                                
BUYS     LKOUT R,E#SEC                                                          
Rate     LKOUT C,D#UNTRAT,(D,B#SAVED,SECRATE),CHAR,ND=Y                         
Prem     LKOUT C,D#PREMUM,(D,B#SAVED,SECPREM),CHAR,ND=Y                         
AdCod    LKOUT C,D#ADCODE,(D,B#SAVED,SECADNO),CHAR,ND=Y                         
IStat    LKOUT C,D#INSSTA,(D,B#SAVED,SECINSTA),CHAR,ND=Y                        
SClDt    LKOUT C,D#SPCDAT,(D,B#SAVED,SECSCLDT),CHAR,ND=Y                        
OnSDt    LKOUT C,D#ONSDAT,(D,B#SAVED,SECONSDT),CHAR,ND=Y                        
BblDt    LKOUT C,D#BBLDAT,(D,B#SAVED,SECBBLDT),CHAR,ND=Y                        
PblDt    LKOUT C,D#PBLDAT,(D,B#SAVED,SECPBLDT),CHAR,ND=Y                        
MClDt    LKOUT C,D#MCLDAT,(D,B#SAVED,SECMCLDT),CHAR,ND=Y                        
MClXD    LKOUT C,D#MCLXDT,(D,B#SAVED,SECMCLXD),CHAR,ND=Y                        
RCom1    LKOUT C,D#REGCO1,(D,B#SAVED,SECRCOMS),CHAR,ND=Y                        
RCom2    LKOUT C,D#REGCO2,(D,B#SAVED,SECRCOMS),CHAR,ND=Y                        
RCom3    LKOUT C,D#REGCO3,(D,B#SAVED,SECRCOMS),CHAR,ND=Y                        
RCom4    LKOUT C,D#REGCO4,(D,B#SAVED,SECRCOMS),CHAR,ND=Y                        
RCom5    LKOUT C,D#REGCO5,(D,B#SAVED,SECRCOMS),CHAR,ND=Y                        
ICom1    LKOUT C,D#INSCO1,(D,B#SAVED,SECICOMS),CHAR,ND=Y                        
ICom2    LKOUT C,D#INSCO2,(D,B#SAVED,SECICOMS),CHAR,ND=Y                        
ICom3    LKOUT C,D#INSCO3,(D,B#SAVED,SECICOMS),CHAR,ND=Y                        
ICom4    LKOUT C,D#INSCO4,(D,B#SAVED,SECICOMS),CHAR,ND=Y                        
ICom5    LKOUT C,D#INSCO5,(D,B#SAVED,SECICOMS),CHAR,ND=Y                        
PIns1    LKOUT C,D#POSIN1,(D,B#SAVED,SECPINSS),CHAR,ND=Y                        
PIns2    LKOUT C,D#POSIN2,(D,B#SAVED,SECPINSS),CHAR,ND=Y                        
PIns3    LKOUT C,D#POSIN3,(D,B#SAVED,SECPINSS),CHAR,ND=Y                        
PIns4    LKOUT C,D#POSIN4,(D,B#SAVED,SECPINSS),CHAR,ND=Y                        
PIns5    LKOUT C,D#POSIN5,(D,B#SAVED,SECPINSS),CHAR,ND=Y                        
GrsOr    LKOUT C,D#GRSORD,(D,B#SAVED,SECGRSOR),CHAR,ND=Y                        
NetOr    LKOUT C,D#NETORD,(D,B#SAVED,SECNETOR),CHAR,ND=Y                        
GLCDO    LKOUT C,D#GLCDO,(D,B#SAVED,SECGLCDO),CHAR,ND=Y                         
NLCDO    LKOUT C,D#NLCDO,(D,B#SAVED,SECNLCDO),CHAR,ND=Y                         
AgCDP    LKOUT C,D#COMPCT,(D,B#SAVED,SECAGCDP),CHAR,ND=Y                        
AgCDA    LKOUT C,D#COMAMT,(D,B#SAVED,SECAGCDA),CHAR,ND=Y                        
CDPct    LKOUT C,D#DSCPCT,(D,B#SAVED,SECCDPCT),CHAR,ND=Y                        
CDAmt    LKOUT C,D#DSCAMT,(D,B#SAVED,SECCDAM),CHAR,ND=Y                         
TaxPc    LKOUT C,D#TAXPCT,(D,B#SAVED,SECTAXP),CHAR,ND=Y                         
TaxAm    LKOUT C,D#TAXAMT,(D,B#SAVED,SECTAXA),CHAR,ND=Y                         
BilDt    LKOUT C,D#BLDDAT,(D,B#SAVED,SECBLDDT),CHAR,ND=Y                        
GrsBd    LKOUT C,D#GRSBD,(D,B#SAVED,SECGRSBD),CHAR,ND=Y                         
NetBd    LKOUT C,D#NETBD,(D,B#SAVED,SECNETBD),CHAR,ND=Y                         
GLCDB    LKOUT C,D#GLCDB,(D,B#SAVED,SECGLCDB),CHAR,ND=Y                         
NLCDB    LKOUT C,D#NLCDB,(D,B#SAVED,SECNLCDB),CHAR,ND=Y                         
InvNo    LKOUT C,D#INVNUM,(D,B#SAVED,SECINVNO),CHAR,ND=Y                        
PayDt    LKOUT C,D#PAYDAT,(D,B#SAVED,SECPAYDT),CHAR,ND=Y                        
GrsPd    LKOUT C,D#GRSPD,(D,B#SAVED,SECGRSPD),CHAR,ND=Y                         
NetPd    LKOUT C,D#NETPD,(D,B#SAVED,SECNETPD),CHAR,ND=Y                         
GLCDP    LKOUT C,D#GLCDP,(D,B#SAVED,SECGLCDP),CHAR,ND=Y                         
NLCDP    LKOUT C,D#NLCDP,(D,B#SAVED,SECNLCDP),CHAR,ND=Y                         
Payee    LKOUT C,D#PAYREP,(D,B#SAVED,SECPAYEE),CHAR,ND=Y                        
ChkNo    LKOUT C,D#CHECKN,(D,B#SAVED,SECCHKNO),CHAR,ND=Y                        
ChkDt    LKOUT C,D#CKDATE,(D,B#SAVED,SECCHKDT),CHAR,ND=Y                        
PlCst    LKOUT C,D#PLCOST,(D,B#SAVED,SECPLCST),CHAR,ND=Y                        
S_Rep    LKOUT C,D#SPREP,(D,B#SAVED,SECSREP),CHAR,ND=Y                          
GrsAc    LKOUT C,D#GRSAC,(D,B#SAVED,SECGRSAC),CHAR,ND=Y                         
NetAc    LKOUT C,D#NETAC,(D,B#SAVED,SECNETAC),CHAR,ND=Y                         
GLCDA    LKOUT C,D#GLCAC,(D,B#SAVED,SECGLCDA),CHAR,ND=Y                         
NLCDA    LKOUT C,D#NLCAC,(D,B#SAVED,SECNLCDA),CHAR,ND=Y                         
TStat    LKOUT C,D#TSHSTA,(D,B#SAVED,SECTSTAT),CHAR,ND=Y                        
PagNt    LKOUT C,D#TSHNOT,(D,B#SAVED,SECPAGNT),CHAR,ND=Y                        
TCom1    LKOUT C,D#TSHCO1,(D,B#SAVED,SECTCOMS),CHAR,ND=Y                        
TCom2    LKOUT C,D#TSHCO2,(D,B#SAVED,SECTCOMS),CHAR,ND=Y                        
TCom3    LKOUT C,D#TSHCO3,(D,B#SAVED,SECTCOMS),CHAR,ND=Y                        
TCom4    LKOUT C,D#TSHCO4,(D,B#SAVED,SECTCOMS),CHAR,ND=Y                        
DECir    LKOUT C,D#DEFCIR,(D,B#SAVED,SECDECIR),CHAR,ND=Y                        
Rpnts    LKOUT C,D#REPNTS,(D,B#SAVED,SECRPNTS),CHAR,ND=Y                        
EImps    LKOUT C,D#ESTIMP,(D,B#SAVED,SECEIMPS),CHAR,ND=Y                        
AImps    LKOUT C,D#ACTIMP,(D,B#SAVED,SECAIMPS),CHAR,ND=Y                        
Click    LKOUT C,D#CLICK,(D,B#SAVED,SECCLICK),CHAR,ND=Y                         
PView    LKOUT C,D#VIEWS,(D,B#SAVED,SECPVIEW),CHAR,ND=Y                         
ConUV    LKOUT C,D#CONUVL,(D,B#SAVED,SECCUV),CHAR,ND=Y                          
ConLE    LKOUT C,D#CONLIE,(D,B#SAVED,SECCLE),CHAR,ND=Y                          
RefNo    LKOUT C,D#REFNUM,(D,B#SAVED,SECREFNO),CHAR,ND=Y                        
AddAB    LKOUT C,D#ACTADD,(D,B#SAVED,SECAADD),CHAR,ND=Y                         
ChaAB    LKOUT C,D#ACTCHA,(D,B#SAVED,SECACHA),CHAR,ND=Y                         
DelAB    LKOUT C,D#ACTDEL,(D,B#SAVED,SECADEL),CHAR,ND=Y                         
Upld_    LKOUT C,D#ACTULD,(D,B#SAVED,SECAULD),CHAR,ND=Y                         
Dwld_    LKOUT C,D#ACTDLD,(D,B#SAVED,SECADLD),CHAR,ND=Y                         
INSOR    LKOUT C,D#ACTIOR,(D,B#SAVED,SECAIOR),CHAR,ND=Y                         
AddIn    LKOUT C,D#ACTADI,(D,B#SAVED,SECAADI),CHAR,ND=Y,               +        
               PCVERSION=2.0.0.0                                                
ChaIn    LKOUT C,D#ACTCHI,(D,B#SAVED,SECACHI),CHAR,ND=Y,               +        
               PCVERSION=2.0.0.0                                                
DelIn    LKOUT C,D#ACTDLI,(D,B#SAVED,SECADLI),CHAR,ND=Y,               +        
               PCVERSION=2.0.0.0                                                
PayIn    LKOUT C,D#ACTPAY,(D,B#SAVED,SECAPAY),CHAR,ND=Y,               +        
               PCVERSION=2.0.0.0                                                
OvrIn    LKOUT C,D#ACTOVR,(D,B#SAVED,SECAOVR),CHAR,ND=Y,               +        
               PCVERSION=2.0.0.0                                                
Clst_    LKOUT C,D#CLRSTA,(D,B#SAVED,SECINSCS),CHAR,ND=Y,              +        
               PCVERSION=2.0.0.0                                                
Dsst_    LKOUT C,D#DISSTA,(D,B#SAVED,SECINSDS),CHAR,ND=Y,              +        
               PCVERSION=2.0.0.0                                                
Msst_    LKOUT C,D#MATSTA,(D,B#SAVED,SECINSMS),CHAR,ND=Y,              +        
               PCVERSION=2.0.0.0                                                
TSRcd    LKOUT C,D#TEAREC,(D,B#SAVED,SECTSREC),CHAR,ND=Y,              +        
               PCVERSION=2.0.0.0                                                
NetPy    LKOUT C,D#NPYBLE,(D,B#SAVED,SECINSNP),CHAR,ND=Y,              +        
               PCVERSION=2.0.0.0                                                
GrsPy    LKOUT C,D#GPYBLE,(D,B#SAVED,SECINSGP),CHAR,ND=Y,              +        
               PCVERSION=2.0.0.0                                                
InvNm    LKOUT C,D#VINVNO,(D,B#SAVED,SECINVNM),CHAR,ND=Y,              +        
               PCVERSION=2.0.0.0                                                
SpDsp    LKOUT C,D#SPCDSC,(D,B#SAVED,SECINSSD),CHAR,ND=Y,              +        
               PCVERSION=2.0.0.0                                                
AcCPM    LKOUT C,D#ACPM,(D,B#SAVED,SECACPM),CHAR,ND=Y,                 +        
               PCVERSION=2.0.0.0                                                
InsDt    LKOUT C,D#INSDAT,(D,B#SAVED,SECINSDT),CHAR,ND=Y,              +        
               PCVERSION=2.0.0.0                                                
IssNm    LKOUT C,D#ISSNM,(D,B#SAVED,SECISSNM),CHAR,ND=Y,               +        
               PCVERSION=2.0.0.0                                                
NmPan    LKOUT C,D#ILLPAN,(D,B#SAVED,SEC#IPAN),CHAR,ND=Y,              +        
               PCVERSION=2.0.0.0                                                
NmReg    LKOUT C,D#REGDSP,(D,B#SAVED,SEC#RPAN),CHAR,ND=Y,              +        
               PCVERSION=2.0.0.0                                                
SFH__    LKOUT C,D#SPECFH,(D,B#SAVED,SECSFH),CHAR,ND=Y,                +        
               PCVERSION=2.0.0.0                                                
ShpDt    LKOUT C,D#SHPDAT,(D,B#SAVED,SECSHPDT),CHAR,ND=Y,              +        
               PCVERSION=2.0.0.0                                                
Shows    LKOUT C,D#SHOWGS,(D,B#SAVED,SECSHWGS),CHAR,ND=Y,              +        
               PCVERSION=2.0.0.0                                                
SitLo    LKOUT C,D#SITELO,(D,B#SAVED,SECSITEL),CHAR,ND=Y,              +        
               PCVERSION=2.0.0.0                                                
TSApp    LKOUT C,D#TSHAPR,(D,B#SAVED,SECTSAPP),CHAR,ND=Y,              +        
               PCVERSION=2.0.0.0                                                
TRepo    LKOUT C,D#REPROQ,(D,B#SAVED,SECTREPO),CHAR,ND=Y,              +        
               PCVERSION=2.0.0.0                                                
GST__    LKOUT C,D#GST,(D,B#SAVED,SECGST),CHAR,ND=Y,                   +        
               PCVERSION=2.0.0.0                                                
PST__    LKOUT C,D#PST,(D,B#SAVED,SECPST),CHAR,ND=Y,                   +        
               PCVERSION=2.0.0.0                                                
InsD2    LKOUT C,D#INSDA2,(D,B#SAVED,SEC2NDID),CHAR,ND=Y,              +        
               PCVERSION=2.0.0.0                                                
DRR__    LKOUT C,D#ACTDRR,(D,B#SAVED,SECADRR),CHAR,ND=Y,               +        
               PCVERSION=3.2.0.2                                                
C2Fac    LKOUT C,D#C2FACT,(D,B#SAVED,SECC2FAC),CHAR,ND=Y,              +        
               PCVERSION=3.3.0.9                                                
C2Grs    LKOUT C,D#C2FGRS,(D,B#SAVED,SECC2GRS),CHAR,ND=Y,              +        
               PCVERSION=3.3.0.9                                                
C2Net    LKOUT C,D#C2FNET,(D,B#SAVED,SECC2NET),CHAR,ND=Y,              +        
               PCVERSION=3.3.0.9                                                
B_ESR    LKOUT C,D#USEESR,(D,B#SAVED,SECBESR),CHAR,ND=Y,               +        
               PCVERSION=3.3.0.19                                               
Dupli    LKOUT C,D#DUPLIC,(D,B#SAVED,SECBDUP),CHAR,ND=Y,               +        
               PCVERSION=3.3.0.19                                               
ReqIv    LKOUT C,D#REQINV,(D,B#SAVED,SECBQIV),CHAR,ND=Y,               +        
               PCVERSION=3.4.0.31                                               
RecIv    LKOUT C,D#RECINV,(D,B#SAVED,SECBRIV),CHAR,ND=Y,               +        
               PCVERSION=3.4.0.31                                               
PurO#    LKOUT C,D#PUROR#,(D,B#SAVED,SECPURO#),CHAR,ND=Y,              *        
               PCVERSION=3.4.0.31                                               
PCNet    LKOUT C,D#PC_NET,(D,B#SAVED,SECPCNET),CHAR,ND=Y,              *        
               PCVERSION=3.4.0.31                                               
PCGro    LKOUT C,D#PC_GRO,(D,B#SAVED,SECPCGRO),CHAR,ND=Y,              *        
               PCVERSION=3.4.0.31                                               
EsCPM    LKOUT C,D#ECPM,(D,B#SAVED,SEC_ECPM),CHAR,ND=Y,                *        
               PCVERSION=9.9.9.9                                                
SRCom    LKOUT C,D#SRCOM1,(D,B#SAVED,SECSRCOM),CHAR,ND=Y,              *        
               PCVERSION=3.6.0.7                                                
PVIV#    LKOUT C,D#PVINV#,(D,B#SAVED,SECPVIV#),CHAR,ND=Y,              *        
               PCVERSION=4.0.0.0                                                
C2Rat    LKOUT C,D#COS2$$,(D,B#SAVED,SECC2RAT),CHAR,ND=Y,              *        
               PCVERSION=4.0.0.4                                                
         LKOUT E                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
                                                                                
AGYR     LKOUT R,E#MED                                                          
Array    LKOUT C,E#MED,(A,ARYAGY)                                               
         LKOUT E                                                                
                                                                                
NATV     LKOUT R,E#NATVAL                                                       
PNation  LKOUT C,D#NATION,(D,B#SAVED,NATNALTY),CHAR,PCVERSION=4.0.0.0           
         LKOUT E                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
                                                                                
CCOC     DC    AL2(CCOCX-*)                                                     
         DC    AL2(E#CCO),C'ARRAY'                                              
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(E#CCO)                                                       
         DC    CL5'DLCCO'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYCCO-SVRDEF),AL1(0,0)                                      
         DC    XL4'00'                                                          
                                                                                
CCOCX    DS    0X                                                               
*                                                                               
IHSC     DC    AL2(IHSCX-*)                                                     
         DC    AL2(E#IHS),C'INVHD'                                              
         DC    AL1(0,0,0)                                                       
         DC    XL4'00000000'                                                    
*                                                                               
         DC    AL2(D#INVDAT)                                                    
         DC    CL5'INDT?'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SECINVDT-SAVED),AL1(LD_CHARQ,L'SECINVDT)                     
         DC    XL4'02000000'                                                    
*                                                                               
         DC    AL2(D#COMMNT)                                                    
         DC    CL5'INHC?'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SECINVHC-SAVED),AL1(LD_CHARQ,L'SECINVHC)                     
         DC    XL4'02000000'                                                    
*                                                                               
         DC    AL2(D#VINVNO)                                                    
         DC    CL5'INNM?'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SECINVNM-SAVED),AL1(LD_CHARQ,L'SECINVNM)                     
         DC    XL4'02000000'                                                    
*                                                                               
         DC    AL2(D#IPRSTR)                                                    
         DC    CL5'INSP?'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SECINVSP-SAVED),AL1(LD_CHARQ,L'SECINVSP)                     
         DC    XL4'02000000'                                                    
*                                                                               
         DC    AL2(D#SPREP)                                                     
         DC    CL5'INSR?'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SECINVSR-SAVED),AL1(LD_CHARQ,L'SECINVSR)                     
         DC    XL4'02000000'                                                    
*                                                                               
         DC    AL2(D#INVSTA)                                                    
         DC    CL5'INST?'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SECINVST-SAVED),AL1(LD_CHARQ,L'SECINVST)                     
         DC    XL4'02000000'                                                    
*                                                                               
         DC    AL2(D#INVTOT)                                                    
         DC    CL5'INTT?'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SECINVTT-SAVED),AL1(LD_CHARQ,L'SECINVTT)                     
         DC    XL4'02000000'                                                    
*                                                                               
         DC    AL2(D#ITOTYP)                                                    
         DC    CL5'INTP?'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SECINVTP-SAVED),AL1(LD_CHARQ,L'SECINVTP)                     
         DC    XL4'02000000'                                                    
*                                                                               
         DC    AL2(D#PUBCOD)                                                    
         DC    CL5'IvPub'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SECIVPUB-SAVED),AL1(LD_CHARQ,L'SECIVPUB)                     
         DC    XL4'02000000'                                                    
*                                                                               
IHSCX    DS    0X                                                               
*                                                                               
IISC     DC    AL2(IISCX-*)                                                     
         DC    AL2(E#IIS),C'INVIT'                                              
         DC    AL1(0,0,0)                                                       
         DC    XL4'00000000'                                                    
*                                                                               
         DC    AL2(D#ADCAP)                                                     
         DC    CL5'INAD?'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SECINVAD-SAVED),AL1(LD_CHARQ,L'SECINVAD)                     
         DC    XL4'02000000'                                                    
*                                                                               
         DC    AL2(D#GRSORD)                                                    
         DC    CL5'INGA?'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SECINVGA-SAVED),AL1(LD_CHARQ,L'SECINVGA)                     
         DC    XL4'02000000'                                                    
*                                                                               
         DC    AL2(D#INSDAT)                                                    
         DC    CL5'INID?'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SECINVID-SAVED),AL1(LD_CHARQ,L'SECINVID)                     
         DC    XL4'02000000'                                                    
*                                                                               
         DC    AL2(D#COMMNT)                                                    
         DC    CL5'INIC?'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SECINVIC-SAVED),AL1(LD_CHARQ,L'SECINVIC)                     
         DC    XL4'02000000'                                                    
*                                                                               
         DC    AL2(D#NETORD)                                                    
         DC    CL5'INNA?'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SECINVNA-SAVED),AL1(LD_CHARQ,L'SECINVNA)                     
         DC    XL4'02000000'                                                    
*                                                                               
         DC    AL2(D#NUMLIN)                                                    
         DC    CL5'INNL?'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SECINVNL-SAVED),AL1(LD_CHARQ,L'SECINVNL)                     
         DC    XL4'02000000'                                                    
*                                                                               
         DC    AL2(D#PREMUM)                                                    
         DC    CL5'INPR?'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SECINVPR-SAVED),AL1(LD_CHARQ,L'SECINVPR)                     
         DC    XL4'02000000'                                                    
*                                                                               
         DC    AL2(D#UNTRAT)                                                    
         DC    CL5'INRT?'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SECINVRT-SAVED),AL1(LD_CHARQ,L'SECINVRT)                     
         DC    XL4'02000000'                                                    
*                                                                               
         DC    AL2(D#SPCDSC)                                                    
         DC    CL5'INSD?'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SECINVSD-SAVED),AL1(LD_CHARQ,L'SECINVSD)                     
         DC    XL4'02000000'                                                    
                                                                                
IvDCM    LKOUT C,D#DCOMCD,(D,B#SAVED,SECIVDCM),CHAR,ND=Y,              *        
               PCVERSION=3.6.0.1                                                
IvClt    LKOUT C,D#CLTCOD,(D,B#SAVED,SECIVCLT),CHAR,ND=Y,              +        
               PCVERSION=2.0.0.0                                                
IvPrd    LKOUT C,D#PRDCOD,(D,B#SAVED,SECIVPRD),CHAR,ND=Y,              +        
               PCVERSION=2.0.0.0                                                
*                                                                               
IISCX    DS    0X                                                               
*                                                                               
SETTGS   LKOUT R,E#SETTGS                                                       
WebIO    LKOUT C,D#WEBIOS,(D,B#SAVED,STGEIO),CHAR,ND=Y,                +        
               PCVERSION=2.3.0.0,FILTROUT=TSTVRSN1                              
SoapE    LKOUT C,D#SOAPEP,(D,B#SAVED,SOAPENDP),CHAR,ND=Y,              +        
               PCVERSION=3.2.0.17,FILTROUT=TSTVRSN1                             
EmalE    LKOUT C,D#EMALEP,(D,B#SAVED,EMALENDP),CHAR,ND=Y,              +        
               PCVERSION=3.2.0.17,FILTROUT=TSTVRSN1                             
EnhSR    LKOUT C,D#USEESR,(D,B#SAVED,STGESR),CHAR,ND=Y,                +        
               PCVERSION=3.3.0.2,FILTROUT=TSTVRSN1                              
SoapE    LKOUT C,D#ESREP1,(D,B#SAVED,ESRENDP1),CHAR,ND=Y,              +        
               PCVERSION=3.3.0.2,FILTROUT=TSTVRSN1                              
EmalE    LKOUT C,D#ESREP2,(D,B#SAVED,ESRENDP2),CHAR,ND=Y,              +        
               PCVERSION=3.3.0.2,FILTROUT=TSTVRSN1                              
WSTmO    LKOUT C,D#WSTMOT,(D,B#SAVED,WSTIMOUT),CBIN,ND=Y,              +        
               PCVERSION=3.4.0.31,FILTROUT=TSTVRSN1                             
EMTmO    LKOUT C,D#EMTMOT,(D,B#SAVED,EMTIMOUT),CBIN,ND=Y,              +        
               PCVERSION=3.4.0.31,FILTROUT=TSTVRSN1                             
IOLgo    LKOUT C,D#IOLOGO,(D,B#SAVED,EIO_LOGO),CHAR,ND=Y,              +        
               PCVERSION=3.5.0.21,FILTROUT=TSTVRSN1                             
IOAlp    LKOUT C,D#IOALPH,(D,B#SAVED,EIOALPHA),CHAR,ND=Y,              +        
               PCVERSION=3.5.0.21,FILTROUT=TSTVRSN1                             
SRLgo    LKOUT C,D#SRLOGO,(D,B#SAVED,ESR_LOGO),CHAR,ND=Y,              +        
               PCVERSION=3.5.0.21,FILTROUT=TSTVRSN1                             
SRAlp    LKOUT C,D#SRALPH,(D,B#SAVED,ESRALPHA),CHAR,ND=Y,              +        
               PCVERSION=3.5.0.21,FILTROUT=TSTVRSN1                             
PrvEE    LKOUT C,D#PRVEPE,(D,B#SAVED,PRVEPEND),CHAR,ND=Y,              +        
               PCVERSION=3.5.0.41,FILTROUT=TSTVRSN1                             
SoaEE    LKOUT C,D#SOAEPE,(D,B#SAVED,SOAEPEND),CHAR,ND=Y,              +        
               PCVERSION=3.5.0.41,FILTROUT=TSTVRSN1                             
         LKOUT E                                                                
*                                                                               
PRTSDR   LKOUT R,E#PRTSDR,PCVERSION=3.6.0.7                                     
WebIO    LKOUT C,D#WEBIOS,(D,B#SAVED,STGEIO),CHAR,ND=Y                          
EnhSR    LKOUT C,D#USEESR,(D,B#SAVED,STGESR),CHAR,ND=Y                          
WSTmO    LKOUT C,D#WSTMOT,(D,B#SAVED,WSTIMOUT),CBIN,ND=Y                        
EMTmO    LKOUT C,D#EMTMOT,(D,B#SAVED,EMTIMOUT),CBIN,ND=Y                        
IOLgo    LKOUT C,D#IOLOGO,(D,B#SAVED,EIO_LOGO),CHAR,ND=Y                        
IOAlp    LKOUT C,D#IOALPH,(D,B#SAVED,EIOALPHA),CHAR,ND=Y                        
SRLgo    LKOUT C,D#SRLOGO,(D,B#SAVED,ESR_LOGO),CHAR,ND=Y                        
SRAlp    LKOUT C,D#SRALPH,(D,B#SAVED,ESRALPHA),CHAR,ND=Y                        
PRout    LKOUT P,,GETSDR                                                        
Array    LKOUT C,255,(A,ARYSDR)                                                 
         LKOUT E                                                                
*                                                                               
OPTCNL   DC    AL2(OPTCNLX-*)                                                   
         DC    AL2(E#OPTCNL),C'OCON '                                           
         DC    AL1(0,0,0)                                                       
         DC    XL4'00000000'                                                    
*                                                                               
         DC    AL2(D#SNDEIO)                                                    
         DC    CL5'SdEIO'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(OPTSDEIO-SAVED),AL1(LD_CHARQ,L'OPTSDEIO)                     
         DC    XL4'03040005'                                                    
*                                                                               
         DC    AL2(D#SNDESR)                                                    
         DC    CL5'SdESR'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(OPTSDESR-SAVED),AL1(LD_CHARQ,L'OPTSDESR)                     
         DC    XL4'03040005'                                                    
*                                                                               
         DC    AL2(D#IMPORT)                                                    
         DC    CL5'Impor'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(OPTIMPOR-SAVED),AL1(LD_CHARQ,L'OPTIMPOR)                     
         DC    XL4'04000000'                                                    
*                                                                               
OPTCNLX  DS    0X                                                               
*                                                                               
ESTS     LKOUT R,E#ESTACT,PCVERSION=3.4.0.31                                    
         LKOUT C,D#ACTADD,(D,B#SAVED,SEST_ADD),CHAR,ND=Y                        
         LKOUT C,D#ACTCHA,(D,B#SAVED,SEST_CHG),CHAR,ND=Y                        
         LKOUT C,D#ACTDIS,(D,B#SAVED,SEST_DIS),CHAR,ND=Y                        
         LKOUT C,D#ACTREP,(D,B#SAVED,SEST_REP),CHAR,ND=Y                        
         LKOUT C,D#ACTCPY,(D,B#SAVED,SEST_CPY),CHAR,ND=Y                        
         LKOUT E                                                                
                                                                                
SFMFC    LKOUT R,E#SFMFCO,PCVERSION=3.4.0.31                                    
         LKOUT C,D#PCEFFD,(D,B#SAVED,SACTULZE),CHAR,ND=Y                        
         LKOUT E                                                                
                                                                                
CCGRP    LKOUT R,E#CCFCON,PCVERSION=3.5.0.19                                    
         LKOUT C,1,(D,B#SAVED,SECCCGR1),CHAR                                    
         LKOUT C,2,(D,B#SAVED,SECCCGR2),CHAR                                    
         LKOUT C,3,(D,B#SAVED,SECCCGR3),CHAR                                    
         LKOUT C,4,(D,B#SAVED,SECCCGR4),CHAR                                    
         LKOUT C,5,(D,B#SAVED,SECCCGR5),CHAR                                    
         LKOUT C,6,(D,B#SAVED,SECCCGR6),CHAR                                    
         LKOUT C,7,(D,B#SAVED,SECCCGR7),CHAR                                    
         LKOUT C,8,(D,B#SAVED,SECCCGR8),CHAR                                    
         LKOUT E                                                                
                                                                                
ADFS     LKOUT R,E#OLDSEC,PCVERSION=3.5.0.41                                    
         LKOUT C,P#ADFILE,(D,B#SAVED,SADF_Y_N),CHAR,ND=Y                        
         LKOUT E                                                                
                                                                                
IDBC     LKOUT R,E#IDBCON,PCVERSION=3.5.0.41                                    
         LKOUT C,D#UNTRAT,(D,B#SAVED,NULLBYTE),CHAR                             
         LKOUT C,D#INSDAT,(D,B#SAVED,NULLBYTE),CHAR                             
         LKOUT C,D#PLCOST,(D,B#SAVED,NULLBYTE),CHAR                             
         LKOUT C,D#ACTDEL,(D,B#SAVED,NULLBYTE),CHAR,PCVERSION=3.6.0.1           
         LKOUT E                                                                
                                                                                
DCMREC   LKOUT R,E#DCMRPY                                                       
Array    LKOUT C,E#DCMRPY,(A,ARYDCMR),PCVERSION=3.6.0.1                         
         LKOUT E                                                                
                                                                                
INVORG   LKOUT R,E#IDINVO                                                       
Array    LKOUT C,E#IDINVO,(A,ARYINVO),PCVERSION=4.2.0.2                         
         LKOUT E                                                                
                                                                                
INSORG   LKOUT R,E#IDINSO                                                       
Array    LKOUT C,E#IDINSO,(A,ARYINSO),PCVERSION=4.2.0.2                         
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* Array definition for invoice origins                                *         
***********************************************************************         
                                                                                
ARYINVO  LKOUT A,(R,NXTIVO),MULTIROW=Y,ROWNAME=DUMMY_D                          
OrgCode  LKOUT C,001,(D,B#WORKD,FULL2),CHAR                                     
OrgName  LKOUT C,002,(D,B#WORKD,TEMP2),CHAR                                     
OrgLock  LKOUT C,003,(D,B#WORKD,BYTE2),CHAR                                     
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Array definition for insertion origins                              *         
***********************************************************************         
                                                                                
ARYINSO  LKOUT A,(R,NXTISO),MULTIROW=Y,ROWNAME=DUMMY_D                          
OrgCode  LKOUT C,001,(D,B#WORKD,BYTE2),UBIN                                     
OrgName  LKOUT C,002,(D,B#WORKD,TEMP2),CHAR                                     
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Array definition for Agency record download                         *         
***********************************************************************         
                                                                                
ARYAGY   LKOUT A,(R,GETMED),MULTIROW=Y,ROWNAME=PAGYRECD                         
PMedCod  LKOUT C,D#MEDCOD,(D,B#SAVED,MEDCODE),CHAR                              
PMedNam  LKOUT C,D#MEDNAM,(D,B#SAVED,MEDNAME),CHAR                              
Array    LKOUT C,E#ACH,(A,ARYACH)                                               
         LKOUT E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* Array definition for Additional Charges download                    *         
***********************************************************************         
                                                                                
ARYACH   LKOUT A,(D,B#ACH,ACHTABD),NROWS=(B#SAVED,ACHTABN),NEWEL=B,    +        
               ROWWIDTH=ACHTABL                                                 
ACCod    LKOUT C,D#ACHCOD,(D,,ACHCODE),CHAR                                     
ACDsc    LKOUT C,D#ACHDSC,(D,,ACHDESC),CHAR                                     
         LKOUT E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* Array definition for Custom Columns download                        *         
***********************************************************************         
                                                                                
ARYCCO   LKOUT A,(R,NXTCCO),MULTIROW=Y,ROWNAME=CCOTABD                          
CCName   LKOUT C,D#CCONAM,(D,B#CCO,CCONAME),CHAR                                
CCCode   LKOUT C,D#CCOCOD,(D,B#CCO,CCOCODE),CHAR                                
CCDTyp   LKOUT C,D#CCOTYP,(D,B#CCO,CCODTYPE),CHAR                               
CCDTot   LKOUT C,D#CCOTOT,(D,B#CCO,CCOTOTAL),CHAR                               
CCHed1   LKOUT C,D#CCOHD1,(D,B#CCO,CCOHEAD1),CHAR                               
CCHed2   LKOUT C,D#CCOHD2,(D,B#CCO,CCOHEAD2),CHAR                               
CCMedi   LKOUT C,D#MEDCOD,(D,B#CCO,CCOMEDS),CHAR                                
CCSeq#   LKOUT C,D#CCSEQN,(D,B#CCO,CCOSEQ#),UBIN,PCVERSION=2.2.0.0              
CCLeng   LKOUT C,D#CCOLEN,(D,B#CCO,CCOLENG),UBIN,PCVERSION=2.2.0.0              
CCDeci   LKOUT C,D#CCODEC,(D,B#CCO,CCODEC#),UBIN,PCVERSION=2.2.0.0              
CCGrp#   LKOUT C,D#CCGRP#,(D,B#CCO,CCGRP_#),UBIN,PCVERSION=3.5.0.19             
CCROAB   LKOUT C,D#RDONAB,(D,B#CCO,CCRONAB),CHAR,ND=Y                           
CCDLS1   LKOUT C,D#CCDROP,(D,B#CCO,CCDLS_01),CHAR,ND=Y,                +        
               PCVERSION=4.0.0.9,FILTROUT=TSTCDL                                
CCDLS2   LKOUT C,D#CCDROP,(D,B#CCO,CCDLS_02),CHAR,ND=Y,                +        
               PCVERSION=4.0.0.9,FILTROUT=TSTCDL                                
CCDLS3   LKOUT C,D#CCDROP,(D,B#CCO,CCDLS_03),CHAR,ND=Y,                +        
               PCVERSION=4.0.0.9,FILTROUT=TSTCDL                                
CCDLS4   LKOUT C,D#CCDROP,(D,B#CCO,CCDLS_04),CHAR,ND=Y,                +        
               PCVERSION=4.0.0.9,FILTROUT=TSTCDL                                
CCDLS5   LKOUT C,D#CCDROP,(D,B#CCO,CCDLS_05),CHAR,ND=Y,                +        
               PCVERSION=4.0.0.9,FILTROUT=TSTCDL                                
CCDLS6   LKOUT C,D#CCDROP,(D,B#CCO,CCDLS_06),CHAR,ND=Y,                +        
               PCVERSION=4.0.0.9,FILTROUT=TSTCDL                                
CCDLS7   LKOUT C,D#CCDROP,(D,B#CCO,CCDLS_07),CHAR,ND=Y,                +        
               PCVERSION=4.0.0.9,FILTROUT=TSTCDL                                
CCDLS8   LKOUT C,D#CCDROP,(D,B#CCO,CCDLS_08),CHAR,ND=Y,                +        
               PCVERSION=4.0.0.9,FILTROUT=TSTCDL                                
CCDLS9   LKOUT C,D#CCDROP,(D,B#CCO,CCDLS_09),CHAR,ND=Y,                +        
               PCVERSION=4.0.0.9,FILTROUT=TSTCDL                                
CCDLSA   LKOUT C,D#CCDROP,(D,B#CCO,CCDLS_10),CHAR,ND=Y,                +        
               PCVERSION=4.0.0.9,FILTROUT=TSTCDL                                
CCDLSB   LKOUT C,D#CCDROP,(D,B#CCO,CCDLS_11),CHAR,ND=Y,                +        
               PCVERSION=4.0.0.9,FILTROUT=TSTCDL                                
CCDLSC   LKOUT C,D#CCDROP,(D,B#CCO,CCDLS_12),CHAR,ND=Y,                +        
               PCVERSION=4.0.0.9,FILTROUT=TSTCDL                                
         LKOUT E                                                                
*                                                                               
TSTCDL   CLI   QCCDLIS,YESQ        Test Column drop down list found             
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAPS FOR CFM MEDIA DOWNLOAD                                  *         
***********************************************************************         
                                                                                
OUTCFM   DS    0X                                                               
                                                                                
R#CFM    EQU   1                                                                
         DC    AL2(OUTCFMX-*)                                                   
         DC    AL2(1)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
                                                                                
CFM      DC    AL2(CFMX-*)                                                      
         DC    AL2(1),C'SECUR'                                                  
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(1)                                                           
         DC    CL5'LimAc'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(LIMACCS-SAVED),AL1(LD_CHARQ,L'LIMACCS)                       
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(1)                                                           
         DC    CL5'Array'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYCFM-SVRDEF),AL1(0,0)                                      
         DC    XL4'00'                                                          
                                                                                
CFMX     DS    0X                                                               
                                                                                
OUTCFMX  DS    0X                                                               
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR CFM MEDIA DOWNLOAD                             *         
***********************************************************************         
                                                                                
ARYCFM   DS    0X                                                               
                                                                                
         DC    AL1(B#SAVED,LX_IEOTQ,0)                                          
         DC    AL2(MEDTAB-SAVED)                                                
         DC    AL1(0,0)                                                         
         DC    AL2(MEDTABL,0)                                                   
         DC    AL1(CFMCOLN)                                                     
         DC    XL4'00'                                                          
                                                                                
CFMCOLS  DS    0X                                                               
                                                                                
         DC    AL2(2),C'MedCd'                                                  
         DC    AL1(0,0,0)                                                       
         DC    AL2(0),AL1(L'MEDTCODE)                                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(3),C'MedNm'                                                  
         DC    AL1(0,0,0)                                                       
         DC    AL2(L'MEDTCODE),AL1(L'MEDTNAME)                                  
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
CFMCOLN  EQU   (*-CFMCOLS)/LX_COLSL                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR DISCREPANCY COMMENT DOWNLOAD                   *         
***********************************************************************         
                                                                                
ARYDCMR  LKOUT A,(R,GET_DCMR),MULTIROW=Y,ROWNAME=DCM_OUTD                       
DCMCd    LKOUT C,D#DCOMCD,(D,,DCM_CODE),CHAR,ND=Y                               
MedCd    LKOUT C,00000003,(D,,DCM_MEDC),CHAR,ND=Y                               
CutDt    LKOUT C,00000005,(D,,DCM_CODT),BDAT,ND=Y                               
DComm    LKOUT C,00000007,(D,,DCM_COMM),CHAR,ND=Y                               
         LKOUT E                                                                
                                                                                
***********************************************************************         
* CALL DDLINK TO GET SELF-DEFINING RECORD                             *         
***********************************************************************         
                                                                                
GETSDR   ST    R4,FULL1                                                         
         L     R4,ALP                                                           
         USING LP_D,R4                                                          
         GOTOR LP_AGSDR,DMCB,LP_D,ASDRREC,0                                     
         L     R4,FULL1                                                         
         J     EXIT                                                             
                                                                                
***********************************************************************         
* SEND SELF-DEFINING ELEMENTS                                         *         
***********************************************************************         
                                                                                
ARYSDR   LKOUT A,(D,B#SDRREC,SDRRFRST),EOT=EOR,ROWWIDTH=(V,SDELEN),    +        
               ROWID=(SDELD,0)                                                  
                                                                                
***********************************************************************         
* FIELDS SENT FROM SELF-DEFINING RECORD ARE AS FOLLOWS:-              *         
*                                                                     *         
*        25    HTTP AUTHENTICATION USER ID/PASSWORD                   *         
*        30    EIO PREVIEW ENDPOINT                                   *         
*        32    EIO E-MAIL ENDPOINT                                    *         
*        34    ESR PREVIEW ENDPOINT                                   *         
*        36    ESR E-MAIL ENDPOINT                                    *         
*        38    TARGET URL ENDING                                      *         
*        40    PREVIEW ENDPOINT ENDING                                *         
***********************************************************************         
                                                                                
Defvl    LKOUT C,255,SDELD,SDEL                                                 
         LKOUT E                                                                
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
TSTVRSN1 NTR1  BASE=*,LABEL=*      TEST VERSION FOR SETTING RECORD              
         L     R4,ALP                                                           
         USING LP_D,R4                                                          
         CLC   LP_VRSN1,=AL1(3,6,0,7)                                           
         JL    EXITY                                                            
         J     EXITN                                                            
         LTORG                                                                  
         DROP  RB,R4                                                            
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
GET_DCMR NTR1  BASE=*,LABEL=*      PROCESS DISCREPANCY COMMENT REC DL           
                                                                                
         L     R0,AIO5                                                          
         LHI   R1,DCM_LENQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR OUTPUT BLOCK                           
                                                                                
         L     R4,ALP                                                           
         USING LP_D,R4                                                          
         L     R3,AIO5                                                          
         STCM  R3,15,LP_ADATA                                                   
         USING DCM_OUTD,R3                                                      
         L     R2,IOADDR                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   G_DCM20                                                          
         XC    IOKEY,IOKEY                                                      
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    RE,IOKEY                                                         
         USING DCMKEY,RE                                                        
         MVC   DCMKAGY,AGY                                                      
         MVI   DCMKMED,DCMKMDQ                                                  
         MVI   DCMKRCD,DCMKR1Q                                                  
         MVI   DCMKRC2,DCMKR2Q                                                  
         DROP  RE                                                               
                                                                                
         LHI   R1,IOHI+IOPRTDIR+IO1                                             
         GOTOR (#IOEXEC,AIOEXEC)                                                
         BNE   G_DCM_N                                                          
         B     G_DCM26                                                          
                                                                                
G_DCM20  LHI   R1,IOSQ+IOPRTDIR+IO1                                             
         GOTOR (#IOEXEC,AIOEXEC)                                                
         BNE   G_DCM_N                                                          
                                                                                
G_DCM26  CLC   IOKEY(DCMKDCM-DCMKEY),IOKEYSAV                                   
         BNE   G_DCM_X                                                          
         LHI   R1,IOGET+IOPRTFIL+IO1                                            
         GOTOR (#IOEXEC,AIOEXEC)                                                
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     RE,AIO1                                                          
         USING DCMRECD,RE                                                       
         MVC   DCM_CODE,DCMKDCM                                                 
         LA    R2,DCMFRST                                                       
         DROP  RE                                                               
                                                                                
         USING DCMHELEM,R2                                                      
G_DCM30  CLI   DCMHELCD,0          END OF RECORD?                               
         BE    G_DCM40                                                          
         CLI   DCMHELCD,DCMHELCQ                                                
         BE    G_DCM34                                                          
         SR    RE,RE                                                            
         IC    RE,DCMHELEN                                                      
         AR    R2,RE                                                            
         B     G_DCM30                                                          
                                                                                
G_DCM34  MVC   DCM_MEDC,DCMHMEDS                                                
         MVC   DCM_CODT,DCMHCTDT                                                
         DROP  R2                                                               
                                                                                
G_DCM40  L     RE,AIO1                                                          
         USING DCMRECD,RE                                                       
         LA    R2,DCMFRST                                                       
         DROP  RE                                                               
                                                                                
         USING DCMCELEM,R2                                                      
G_DCM50  CLI   DCMCELCD,0          END OF RECORD?                               
         BE    G_DCM60                                                          
         CLI   DCMCELCD,DCMCELCQ                                                
         BE    G_DCM54                                                          
         SR    RE,RE                                                            
         IC    RE,DCMCELEN                                                      
         AR    R2,RE                                                            
         B     G_DCM50                                                          
                                                                                
G_DCM54  SR    RE,RE                                                            
         IC    RE,DCMCELEN                                                      
         SHI   RE,DCMCELNQ                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DCM_COMM(0),DCMCCOMM                                             
         DROP  R2                                                               
                                                                                
G_DCM60  DS    0H                                                               
         B     G_DCM_Y                                                          
*                                                                               
G_DCM_X  MVI   LP_RMODE,LP_RLAST   NO MORE                                      
*                                                                               
G_DCM_Y  MVC   LP_ADATA,AIO5                                                    
         J     EXITY                                                            
*                                                                               
G_DCM_N  MVC   LP_ADATA,AIO5                                                    
         J     EXITN                                                            
                                                                                
         LTORG                                                                  
         DROP  RB,R4,R3                                                         
*                                                                               
***********************************************************************         
* Return list of invoice origins                                      *         
***********************************************************************         
*                                                                               
NXTIVO   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,ALP                                                           
         USING LP_D,R4                                                          
*                                                                               
         XC    FULL2,FULL2         Invoice origin code                          
         XC    TEMP2,TEMP2         Invoice origin name                          
         MVI   BYTE2,0             Invoice origin lock flag                     
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTIVO10                                                         
         LA    RE,INVORGTB                                                      
         ST    RE,FULL1                                                         
*                                                                               
NXTIVO10 L     RE,FULL1                                                         
         CLI   0(RE),X'FF'         End of table?                                
         JE    NXTIVO_X                                                         
         MVC   FULL2(03),00(RE)    Set invoice origin code                      
         MVC   TEMP2(17),03(RE)    Set invoice origin name                      
         MVC   BYTE2(01),20(RE)    Set invoice origin lock flag                 
         LA    RE,INVORGLQ(RE)     Point to next entry                          
         ST    RE,FULL1                                                         
         MVC   LP_ADATA,LP_ABLK1   Point to WORKD                               
         J     EXITY                                                            
*                                                                               
NXTIVO_X MVI   LP_RMODE,LP_RLAST   NO MORE                                      
         MVC   LP_ADATA,LP_ABLK1   Point to WORKD                               
         J     EXITY                                                            
*                                                                               
INVORGTB DC    C'ADB',C'Print Buy Toolkit',C'N'                                 
INVORGLQ EQU   *-INVORGTB                                                       
         DC    C'IPS',C'IPS              ',C'N'                                 
         DC    C'PRM',C'Prisma           ',C'Y'                                 
         DC    C'RAD',C'Radia            ',C'Y'                                 
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB,R4                                                            
*                                                                               
***********************************************************************         
* Return list of insertion origins                                    *         
***********************************************************************         
*                                                                               
NXTISO   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,ALP                                                           
         USING LP_D,R4                                                          
*                                                                               
         MVI   BYTE2,0             Insertion origin code                        
         XC    TEMP2,TEMP2         Insertion origin name                        
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTISO10                                                         
         LA    RE,INSORGTB                                                      
         ST    RE,FULL1                                                         
*                                                                               
NXTISO10 L     RE,FULL1                                                         
         CLI   0(RE),X'FF'         End of table?                                
         JE    NXTISO_X                                                         
         MVC   BYTE2(01),00(RE)    Set insertion origin code                    
         MVC   TEMP2(17),01(RE)    Set insertion origin name                    
         LA    RE,INSORGLQ(RE)     Point to next entry                          
         ST    RE,FULL1                                                         
         MVC   LP_ADATA,LP_ABLK1   Point to WORKD                               
         J     EXITY                                                            
*                                                                               
NXTISO_X MVI   LP_RMODE,LP_RLAST   NO MORE                                      
         MVC   LP_ADATA,LP_ABLK1   Point to WORKD                               
         J     EXITY                                                            
*                                                                               
INSORGTB DC    AL1(PPIDPPKQ),C'PrintPak         '                               
INSORGLQ EQU   *-INSORGTB                                                       
         DC    AL1(PPIDCPYQ),C'SFM Buy Copy     '                               
         DC    AL1(PPIDMOVQ),C'SFM Buy Move     '                               
         DC    AL1(PPIDADBQ),C'Print Buy Toolkit'                               
         DC    AL1(PPIDIDKQ),C'iDesk            '                               
         DC    AL1(PPIDPBUQ),C'Print Buy Upload '                               
         DC    AL1(PPIDIMPQ),C'PBT Imports      '                               
         DC    AL1(PPIDPRMQ),C'Prisma           '                               
         DC    AL1(PPIDRADQ),C'Radia            '                               
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB,R4                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SAVED    DSECT                                                                  
*                                                                               
AGY      DS    CL2                 AGENCY CODE                                  
*                                                                               
NULLBYTE DS    X                                                                
*                                                                               
NATNALTY DS    CL(L'PAGYNAT)       Nationality                                  
NAT_USAQ EQU   C'U'                USA                                          
NAT_CANQ EQU   C'C'                Canada                                       
*                                                                               
QCCDLIS  DS    C                   Column drop down list switch                 
YESQ     EQU   C'Y'                                                             
NOQ      EQU   C'N'                                                             
*                                                                               
                                                                                
SECVALS  DS    0X                                                               
*                                                                               
* FIELD SECURITY VALUES                                                         
*                                                                               
SECRATE  DS    C                   RATE                                         
SECPREM  DS    C                   PREMIUM                                      
SECADNO  DS    C                   AD NUMBER                                    
SECINSTA DS    C                   INSERTION STATUS                             
SECSCLDT DS    C                   SPACE CLOSING DATE                           
SECONSDT DS    C                   ON SALE DATE                                 
SECBBLDT DS    C                   BILLABLE DATE                                
SECPBLDT DS    C                   PAYABLE DATE                                 
SECMCLDT DS    C                   MATERIALS CLOSING DATE                       
SECMCLXD DS    C                   MATERIALS CLOSING EXTENSION DATE             
SECRCOMS DS    C                   REGULAR COMMENTS                             
SECICOMS DS    C                   INSERTION ORDER COMMENTS                     
SECPINSS DS    C                   POSITION INSTRUCTIONS                        
SECGRSOR DS    C                   GROSS ORDERED                                
SECNETOR DS    C                   NET ORDERED                                  
SECGLCDO DS    C                   GROSS LESS CASH DISCOUNT ORDERED             
SECNLCDO DS    C                   NET LESS CASH DISCOUNT ORDERED               
SECAGCDP DS    C                   AGENCY CASH DISCOUNT PERCENTAGE              
SECAGCDA DS    C                   AGENCY CASH DISCOUNT AMOUNT                  
SECCDPCT DS    C                   CASH DISCOUNT PERCENTAGE                     
SECCDAM  DS    C                   CASH DISCOUNT AMOUNT                         
SECTAXP  DS    C                   TAX PERCENTAGE                               
SECTAXA  DS    C                   TAX AMOUNT                                   
SECBLDDT DS    C                   DATE BILLED                                  
SECGRSBD DS    C                   GROSS BILLED                                 
SECNETBD DS    C                   NET BILLED                                   
SECGLCDB DS    C                   GROSS LESS CASH DISCOUNT BILLED              
SECNLCDB DS    C                   NET LESS CASH DISCOUNT BILLED                
SECINVNO DS    C                   INVOICE NUMBER                               
SECPAYDT DS    C                   DATE PAID                                    
SECGRSPD DS    C                   GROSS PAID                                   
SECNETPD DS    C                   NET PAID                                     
SECGLCDP DS    C                   GROSS LESS CASH DISCOUNT PAID                
SECNLCDP DS    C                   NET LESS CASH DISCOUNT PAID                  
SECPAYEE DS    C                   PAYEE                                        
SECCHKNO DS    C                   CHECK NUMBER                                 
SECCHKDT DS    C                   CHECK DATE                                   
SECPLCST DS    C                   PLANNED COST                                 
SECSREP  DS    C                   SPECIAL REP                                  
SECGRSAC DS    C                   GROSS ADDITIONAL CHARGES                     
SECNETAC DS    C                   NET ADDITIONAL CHARGES                       
SECGLCDA DS    C                   GROSS LESS CD ADDITIONAL CHARGES             
SECNLCDA DS    C                   NET LESS CD ADDITIONAL CHARGES               
SECTSTAT DS    C                   TEARSHEET STATUS                             
SECTREPO DS    C                   TEARSHEET REPRODUCTION QUALITY               
SECPAGNT DS    C                   PAGE NOTATION                                
SECTCOMS DS    C                   TEARSHEET COMMENTS                           
SECDECIR DS    C                   DAILY EFFECTIVE CIRCULATION                  
SECRPNTS DS    C                   REPAINTS                                     
SECEIMPS DS    C                   ESTIMATED IMPRESSIONS                        
SECAIMPS DS    C                   ACTUAL IMPRESSIONS                           
SECCLICK DS    C                   CLICK THROUGH                                
SECPVIEW DS    C                   PAGE VIEWS                                   
SECCUV   DS    C                   CONTRACT UNIT VALUE                          
SECCLE   DS    C                   CONTRACT LINEAGE EQUIVALENCY                 
SECREFNO DS    C                   REFERENCE NUMBER                             
*                                                                               
SECINSCS DS    C                   INSERTION CLEARANCE STATUS                   
SECINSDS DS    C                   INSERTION DISCREPANCY STATUS                 
SECINSMS DS    C                   INSERTION MATCHING STATUS                    
SECTSREC DS    C                   TS RECEIVED                                  
*                                                                               
SECINVDT DS    C                   INVOICE DATE                                 
SECINVHC DS    C                   INVOICE HEADER COMMENT                       
SECINVNM DS    C                   INVOICE NUMBER                               
SECINVSP DS    C                   INVOICE SERVICE PERIOD                       
SECINVSR DS    C                   INVOICE SPECIAL REP                          
SECINVST DS    C                   INVOICE STATUS                               
SECINVTT DS    C                   INVOICE TOTAL                                
SECINVTP DS    C                   INVOICE TOTAL TYPE                           
*                                                                               
SECINVAD DS    C                   INVOICE AD CAPTION                           
SECINVGA DS    C                   INVOICE GROSS AMOUNT                         
SECINVID DS    C                   INVOICE INSERTION DATE                       
SECINVIC DS    C                   INVOICE ITEM COMMENT                         
SECINVNA DS    C                   INVOICE NET AMOUNT                           
SECINVNL DS    C                   INVOICE NUMBER OF LINES                      
SECINVPR DS    C                   INVOICE PREMIUM                              
SECINVRT DS    C                   INVOICE RATE                                 
SECINVSD DS    C                   INVOICE SPACE DESCRIPTION                    
*                                                                               
SECINSNP DS    C                   INSERTION NET PAYABLE                        
SECINSGP DS    C                   INSERTION GROSS PAYABLE                      
SECINSSD DS    C                   INSERTION SPACE DESCRIPTION                  
*                                                                               
SECACPM  DS    C                   ACTUAL CPM                                   
SECINSDT DS    C                   INSERTION DATE                               
SECIVCLT DS    C                   INVOICE CLIENT                               
SECIVPRD DS    C                   INVOICE PRODUCT                              
SECIVPUB DS    C                   INVOICE PUBLICATION                          
SECISSNM DS    C                   ISSUE NAME                                   
SEC#IPAN DS    C                   NUMBER OF ILLUMINATED PANELS                 
SEC#RPAN DS    C                   NUMBER OF REGULAR PANELS                     
SECSFH   DS    C                   SPECIAL FINANCIAL HANDLING                   
SECSHPDT DS    C                   SHIPPED DATE                                 
SECSHWGS DS    C                   SHOWINGS/GRP                                 
SECSITEL DS    C                   SITE LOCATION                                
SECTSAPP DS    C                   TEARSHEET APPROVED                           
SECTSSTA DS    C                   TEARSHEET STATUS                             
SECGST   DS    C                   GST TAX CODE                                 
SECPST   DS    C                   PST TAX CODE                                 
SEC2NDID DS    C                   2ND INSERTION DATE                           
SECC2FAC DS    C                   COS2 FACTOR                                  
SECC2GRS DS    C                   COS2 GROSS AMOUNT                            
SECC2NET DS    C                   COS2 NET AMOUNT                              
SECPURO# DS    C                   PURCHASE ORDER #                             
SECPCNET DS    C                   PLANNED COST NET AMOUNT                      
SECPCGRO DS    C                   PLANNED COST GROSS AMOUNT                    
SECCCGR1 DS    C                   CUSTOM COLUMN GROUP 1                        
SECCCGR2 DS    C                   CUSTOM COLUMN GROUP 2                        
SECCCGR3 DS    C                   CUSTOM COLUMN GROUP 3                        
SECCCGR4 DS    C                   CUSTOM COLUMN GROUP 4                        
SECCCGR5 DS    C                   CUSTOM COLUMN GROUP 5                        
SECCCGR6 DS    C                   CUSTOM COLUMN GROUP 6                        
SECCCGR7 DS    C                   CUSTOM COLUMN GROUP 7                        
SECCCGR8 DS    C                   CUSTOM COLUMN GROUP 8                        
SEC_ECPM DS    C                   ESTIMATED COST PER MILLION                   
SECIVDCM DS    C                   INVOICE DISCREPANCY COMMENT                  
SECSRCOM DS    C                   SPECIAL REMITTANCE COMMENT                   
SECPVIV# DS    C                   Vendor Invoice Number from Pay               
SECC2RAT DS    C                   Cost 2 Rate                                  
*                                                                               
* ACTION SECURITY VALUES                                                        
*                                                                               
SECAADD  DS    C                   ADD BUYS                                     
SECAADI  DS    C                   ADD INVOICE                                  
SECACHA  DS    C                   CHANGE BUYS                                  
SECACHI  DS    C                   CHANGE INVOICE                               
SECADEL  DS    C                   DELETE BUYS                                  
SECADLI  DS    C                   DELETE INVOICE                               
SECATRF  DS    C                   CHANGE TRAFFIC DATA                          
SECAACT  DS    C                   CHANGE ACCOUNTING DATA                       
SECAULD  DS    C                   UPLOAD BUYS                                  
SECADLD  DS    C                   DOWNLOAD BUYS                                
SECAREP  DS    C                   REPORT ON BUYS                               
SECACUS  DS    C                   USE CUSTOM COLUMNS                           
SECAIOR  DS    C                   ISSUE INSERTION ORDERS                       
SECAPAY  DS    C                   PAY                                          
SECAOVR  DS    C                   OVERRIDE PAY PROFILE                         
SECADRR  DS    C                   DISCREPANCY RESOLUTION REPORT                
SECBESR  DS    C                   BUY - ENHANCED SPACE RESERVATION             
SECBDUP  DS    C                   BUY - DUPLICATE ACTION                       
SECBQIV  DS    C                   BUY - REQUEST INVOICE                        
SECBRIV  DS    C                   BUY - RECEIVE INVOICE                        
SECVALSL EQU   *-SECVALS                                                        
                                                                                
SESTACTS DS    0X                  SECURITY FOR ESTIMATE SFM ACTIONS            
SEST_ADD DS    C                   ESTIMATE - ADD                               
SEST_CHG DS    C                   ESTIMATE - CHANGE                            
SEST_DIS DS    C                   ESTIMATE - DISPLAY                           
SEST_REP DS    C                   ESTIMATE - REPORT                            
SEST_CPY DS    C                   ESTIMATE - COPY                              
SESTACLQ EQU   *-SESTACTS                                                       
                                                                                
SADFACTS DS    0X                  SECURITY FOR AD FILE                         
SADF_Y_N DS    C                   YES OR NO                                    
SADFACLQ EQU   *-SADFACTS                                                       
                                                                                
SFMFCONS DS    0X                  SECURITY FOR SFM FIELD CONTROLS              
SACTULZE DS    C                   EST ACTULALIZE BILL THRU DATE                
SFMFCLNQ EQU   *-SFMFCONS                                                       
                                                                                
STGVALS  DS    0X                  SETTING VALUES                               
STGEIO   DS    C                   ENHANCED INSERTION ORDER SETTING             
SOAPENDP DS    CL80                EIO SOAP AND PREVIEW END POINT               
EMALENDP DS    CL80                EIO EMAIL URL END POINT                      
STGESR   DS    C                   ENHANCED SPACE RESERVATION                   
ESRENDP1 DS    CL80                ESR SOAP AND PREVIEW END POINT               
ESRENDP2 DS    CL80                ESR EMAIL URL END POINT                      
WSTIMOUT DS    XL2                 WEB SERVER TIME-OUT                          
EMTIMOUT DS    XL2                 E-MAIL TIME-OUT                              
EIO_LOGO DS    C                   USE EIO LOGO Y/N                             
EIOALPHA DS    CL(L'AGY)           AGENCY ALPHA                                 
ESR_LOGO DS    C                   USE ESR LOGO Y/N                             
ESRALPHA DS    CL(L'AGY)           AGENCY ALPHA                                 
PRVEPEND DS    CL20                PREVIEW ENDPOINT ENDING                      
SOAEPEND DS    CL20                SOAP ENDPOINT ENDING                         
STGVALSL EQU   *-STGVALS                                                        
                                                                                
OPTVALS  DS    0X                  OPTION CONTROL VALUES                        
OPTSDEIO DS    C                   SEND EIO                                     
OPTSDESR DS    C                   SEND ESR                                     
OPTIMPOR DS    C                   Import                                       
OPTVALSL EQU   *-OPTVALS                                                        
                                                                                
MEDVALS  DS    0X                  ** MEDIA VALUES **                           
MEDCODE  DS    CL(L'PAGYKMED)      MEDIA CODE                                   
MEDNAME  DS    CL15                MEDIA NAME                                   
MEDVALSL EQU   *-MEDVALS                                                        
                                                                                
ACHTABN  DS    AL2                 N'ENTRIES IN ACHTAB                          
                                                                                
         DS    0F                                                               
SECBLOCK DS    (SECLENQ)X          SECRET BLOCK                                 
                                                                                
LIMACCS  DS    CL(L'LP_ACCS)       LIMIT ACCESS                                 
                                                                                
MEDTAB   DS    0X                  ** MEDIA TABLE (CFM DOWNLOAD) **             
MEDTCODE DS    CL(L'PAGYKMED)      MEDIA CODE                                   
MEDTNAME DS    CL(L'PAGYMED)       MEDIA NAME                                   
MEDTABL  EQU   *-MEDTAB                                                         
MEDTABMX EQU   16                  MAXIMUM N'MEDIAS                             
         DS    (MEDTABMX-1)XL(MEDTABL),X                                        
                                                                                
SAVEL    EQU   *-SAVED                                                          
                                                                                
CCOTABD  DSECT                     ** CUSTOM COLUMN TABLE **                    
CCONAME  DS    CL(L'PCOLDESC)      CUSTOM COLUMN NAME/DESCRIPTION               
CCOCODE  DS    CL(L'PCOLKCOD)      CUSTOM COLUMN CODE                           
CCODTYPE DS    CL(L'PCOLTYP)       CUSTOM COLUMN TYPE                           
CCOTOTAL DS    CL(L'PCOLTOT)       CUSTOM COLUMN HOW TO TOTAL                   
CCOHEAD1 DS    CL(L'PCOLHDR1)      CUSTOM COLUMN HEADING ONE                    
CCOHEAD2 DS    CL(L'PCOLHDR2)      CUSTOM COLUMN HEADING TWO                    
CCOMEDS  DS    CL11                MEDIA CODES                                  
CCOSEQ#  DS    XL(L'PCOLSQN)       CUSTOM COLUMN SEQUENCE NUMBER                
CCOLENG  DS    XL(L'PCOLMLEN)      CUSTOM COLUMN MAX LENGTH                     
CCODEC#  DS    XL(L'PCOLDECS)      CUSTOM COLUMN # OF DECIMALS                  
CCGRP_#  DS    XL(L'PCOLFCON)      CUSTOM COLUMN FCON GROUP NUMBER              
CCRONAB  DS    CL(L'PCOLRDWR)      Read only in AdBuyer                         
CCDLSTR  DS    0X                  Drop down list starts                        
CCDLS_01 DS    CL50                                                             
CCDLS_02 DS    CL50                                                             
CCDLS_03 DS    CL50                                                             
CCDLS_04 DS    CL50                                                             
CCDLS_05 DS    CL50                                                             
CCDLS_06 DS    CL50                                                             
CCDLS_07 DS    CL50                                                             
CCDLS_08 DS    CL50                                                             
CCDLS_09 DS    CL50                                                             
CCDLS_10 DS    CL50                                                             
CCDLS_11 DS    CL50                                                             
CCDLS_12 DS    CL50                                                             
CCDLEND  DS    X                   End of drop down list                        
CCOTABL  EQU   *-CCOTABD                                                        
                                                                                
ACHTABD  DSECT                     ** ADDITIONAL CHARGES TABLE **               
ACHCODE  DS    CL(L'PSPLCODE)      ADDITIONAL CHARGE RATE CODE                  
ACHTYPE  DS    CL(L'PSPLTYPE)      ADDITIONAL CHARGE TYPE INDICATOR             
ACHDESC  DS    CL(L'PSPLDESC)      ADDITIONAL CHARGE DESCRIPTION                
ACHTABL  EQU   *-ACHTABD                                                        
                                                                                
DCM_OUTD DSECT                     DISCREPANCY COMMENT OUTPUT                   
DCM_CODE DS    CL(L'DCMKDCM)                                                    
DCM_MEDC DS    CL(L'DCMHMEDS)                                                   
DCM_CODT DS    XL(L'DCMHCTDT)                                                   
DCM_COMM DS    CL78                                                             
DCM_LENQ EQU   *-DCM_OUTD                                                       
                                                                                
DUMMY_D  DSECT                                                                  
DUM_LIN1 DS    CL1                                                              
                                                                                
* PPLNKWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE PPLNKWRK                                                       
B#ACH    EQU   3                   ADDITIONAL CHARGES RECORD                    
B#CCO    EQU   5                   custom column RECORD                         
B#DCMREC EQU   5                   DISCREPANCY COMMENT RECORD                   
B#SDRREC EQU   5                   SELF-DEFINING RECORD                         
ASDRREC  EQU   LP_BLKS+((B#SDRREC-1)*L'LP_BLKS),,C'A'                           
                                                                                
EOR      EQU   0                   END OF RECORD ELEMENT                        
         PRINT ON                                                               
                                                                                
* PCOLREC                                                                       
         PRINT OFF                                                              
PCOLRECD DSECT                                                                  
PCOLKRCQ EQU   X'61'               CUSTOM COLUMN RECORD TYPE                    
PCOLELEQ EQU   X'61'               CUSTOM COLUMN ELEMENT CODE                   
       ++INCLUDE PCOLREC                                                        
         PRINT ON                                                               
                                                                                
* PSPCGREC                                                                      
         PRINT OFF                                                              
PSPLRECD DSECT                                                                  
       ++INCLUDE PSPCGREC                                                       
PSPELQ   EQU   X'10'               ADDITIONAL CHARGE DEFINITION ELEMENT         
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE PPGENSCH          EIO SETUP RECORD                             
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE PPPPIDEL          PID element for buy record                   
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE GEGENSDR                                                       
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE PPGENDCM          DISCREPANCY COMMENT RECORD                   
DCMKMDQ  EQU   C'A'                                                             
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE FAXTRAINF         EXTRA USER INFO                              
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054PPLNK12   11/06/18'                                      
         END                                                                    
