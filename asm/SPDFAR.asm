*          DATA SET SPDFAR     AT LEVEL 026 AS OF 05/13/20                      
*PHASE SPDFARA                                                                  
SPDFAR   TITLE 'Spot System Daily File Activity Extract'                        
         PRINT NOGEN                                                            
SPDFAR   CSECT                                                                  
                                                                                
         J     SPNTRY                                                           
                                                                                
         DC    A(SFLIST)           A(SYSTEM/FILE LIST)                          
         DC    A(FILTAB)           A(FILE DEFINITION TABLE)                     
         DC    A(PRGTAB)           A(PROGRAM TABLE)                             
                                                                                
SPNTRY   NMOD1 0,*SPDFAR*                                                       
         LR    RA,R1                                                            
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
         L     R9,DI_AWRK                                                       
         USING WORKD,R9            R9=A(LOCAL WORKING STORAGE)                  
         L     R8,ALITS                                                         
         USING LITERALS,R8         R8=A(GLOBAL LITERALS)                        
         L     R7,DI_ACOM                                                       
         USING COMFACSD,R7         R7=A(COMFACS)                                
                                                                                
         STM   R2,RB,DI_R2RB       SAVE REGISTERS                               
                                                                                
         CLI   DI_MODE,DI_MINIQ    TEST INITIALIZATION                          
         JE    SPINIT                                                           
         CLI   DI_MODE,DI_MAGFQ    TEST FIRST FOR NEW AGENCY                    
         JE    SPAGYF                                                           
         DC    H'0'                                                             
         DROP  RB                                                               
                                                                                
ALITS    DC    A(LITERALS)                                                      
         EJECT                                                                  
***********************************************************************         
* Initialization                                                      *         
***********************************************************************         
                                                                                
SPINIT   L     R2,DI_AAGY          BUILD AGENCY/MEDIA TABLE                     
         USING DA_D,R2             R2=A(AGENCY/MEDIA TABLE)                     
                                                                                
         LA    R3,KEY                                                           
         USING AGYHDRD,R3                                                       
         XC    AGYKEY,AGYKEY                                                    
         MVI   AGYKTYPE,AGYKTYPQ                                                
         GOTOR CDATAMGR,DMCB,DMRDHI,SPTDIR,AGYHDRD,AGYHDRD                      
         J     SPINIT04                                                         
                                                                                
SPINIT02 LA    R3,KEY                                                           
         GOTOR CDATAMGR,DMCB,DMRSEQ,SPTDIR,AGYHDRD,AGYHDRD                      
                                                                                
SPINIT04 JE    *+6                 TEST FOR DATAMGR ERRORS                      
         DC    H'0'                                                             
         CLI   AGYKTYPE,AGYKTYPQ                                                
         JNE   SPINIT12                                                         
         GOTOR CDATAMGR,DMCB,GETREC,SPTFIL,AGYKDA,IO,WORK                       
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,IO               POINT TO RECORD                              
                                                                                
         MVC   DA_ALF,AGYKAGY      SET AGENCY ALPHA ID                          
         MVI   DA_CTRY,CTRYUSAQ                                                 
         CLI   AGYPCNDA,C'C'       TEST CANADIAN                                
         JNE   *+8                                                              
         MVI   DA_CTRY,CTRYCANQ                                                 
         LA    R3,AGYEL                                                         
         USING AGYMEDEL,R3         R3=A(FIRST ELEMENT ON RECORD)                
         LA    R4,DA_MEDS                                                       
         USING DA_MEDS,R4          R4=A(MEDIA LIST IN AGENCY TABLE)             
         SR    R0,R0                                                            
                                                                                
SPINIT06 CLI   AGYMEDEL,0          TEST END-OF-RECORD                           
         JE    SPINIT10                                                         
         CLI   AGYMEDEL,AGYMEDEQ   TEST MEDIA ELEMENT                           
         JNE   SPINIT08                                                         
         MVC   DA_MCOD,AGYMEDCD    BUILD MEDIA ENTRY                            
         MVC   DA_MNUM,AGYMEDBT                                                 
         NI    DA_MNUM,DA_MBITQ                                                 
         MVC   DA_AGB,AGYMEDBT                                                  
         NI    DA_AGB,DA_ABITQ                                                  
         AHI   R4,DA_MLNQ                                                       
                                                                                
SPINIT08 IC    R0,AGYMEDLN         BUMP TO NEXT ELEMENT ON RECORD               
         AR    R3,R0                                                            
         J     SPINIT06                                                         
                                                                                
SPINIT10 AHI   R2,DA_LNQ           BUMP TO NEXT AGENCY TABLE ENTRY              
         J     SPINIT02                                                         
                                                                                
SPINIT12 C     R2,DI_AAGY          TEST ANY AGENCY RECORDS FOUND                
         JE    EXITN                                                            
         DROP  R2,R3,R4                                                         
                                                                                
         LA    R2,CORPHS           LOAD CORE RESIDENT PHASES                    
         LA    R3,APHASES                                                       
         LA    R4,CORPHSN                                                       
         SR    R0,R0                                                            
         ICM   R0,B'1110',=X'D9000A'                                            
         LA    R1,DMCB                                                          
         L     RF,CCALLOV                                                       
SPINIT14 ICM   R0,B'0001',0(R2)                                                 
         GOTOR (RF),(R1),0,(R0)                                                 
         MVC   0(4,R3),0(R1)                                                    
         AHI   R2,L'CORPHS                                                      
         AHI   R3,L'APHASES                                                     
         JCT   R4,SPINIT14                                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* First time for new agency                                           *         
***********************************************************************         
                                                                                
SPAGYF   GOTOR DI_ABFIN,DMCB,('BUFFAINI',ABUFF1),BUFFREC,COMFACSD               
         JE    EXITY                                                            
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* Get client record                                                   *         
***********************************************************************         
                                                                                
GETCLT   NTR1  LABEL=NO                                                         
         XC    BUFFKEY(BUFFKEYL),BUFFKEY                                        
         L     RF,DI_ADSD          EXTRACT AGENCY ALPHA                         
         MVC   BUFFRAGY,DS_ALF-DS_D(RF)                                         
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING CLTRECD,R2          R2=A(CLIENT KEY)                             
         MVC   CKEYAM,DI_RAGY      SET AGENCY/MEDIA                             
         L     RF,0(R1)                                                         
         MVC   CKEYCLT,0(RF)       SET CLIENT CODE                              
         MVC   BUFFRKEY,KEY                                                     
         MVC   WORK(BUFFKEYL),BUFFKEY                                           
                                                                                
         GOTOR DI_ABFIN,DMCB,('BUFFAGET',ABUFF1),BUFFREC,COMFACSD               
         CLI   BUFFERRS-BUFFPARM(R1),0                                          
         JE    EXITY                                                            
                                                                                
         LA    R0,BUFFREC          BUILD NEW BUFFER RECORD                      
         LHI   R1,BUFFRECL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   BUFFKEY(BUFFKEYL),WORK                                           
         GOTOR CDATAMGR,DMCB,DMREAD,SPTDIR,CKEY,CKEY                            
         JE    GETCLT02                                                         
         MVI   BUFFFLAG,BUFFFRNF   SET NOT FOUND IF CAN'T READ                  
         J     GETCLT04                                                         
                                                                                
GETCLT02 GOTOR CDATAMGR,DMCB,GETREC,SPTFIL,CKDA,BUFFRECD                        
         JE    GETCLT04                                                         
         DC    H'0'                                                             
                                                                                
GETCLT04 GOTOR DI_ABFIN,DMCB,('BUFFAPUT',ABUFF1),BUFFREC,COMFACSD               
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Edit out of week start day                                          *         
***********************************************************************         
                                                                                
EDTOWS   LM    R2,R3,DI_AINP                                                    
         CLI   0(R2),0             TEST NO OUT OF WEEK START DAY                
         BER   RE                                                               
         SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         MHI   R1,L'DAYTAB                                                      
         LA    R1,DAYTAB-L'DAYTAB(R1)                                           
         MVC   0(L'DAYTAB,R3),0(R1)                                             
         BR    RE                                                               
                                                                                
***********************************************************************         
* Edit REP record options                                             *         
***********************************************************************         
                                                                                
EDTROPT  LM    R2,R3,DI_AINP                                                    
         L     RF,DI_AOUT          RF=A(OUTPUT)                                 
         TM    0(R2),RPOPT1_TRADE                                               
         JZ    *+14                                                             
         MVI   DI_LOUT,5                                                        
         MVC   0(5,RF),=C'TRADE'                                                
         BR    RE                                                               
                                                                                
***********************************************************************         
* Edit client frozen options                                          *         
***********************************************************************         
                                                                                
EDTCFRZ  LM    R2,R3,DI_AINP                                                    
         L     RF,DI_AOUT          RF=A(OUTPUT)                                 
         TM    0(R2),COP2FRZ       TEST CLIENT FROZEN LOCK                      
         JZ    *+14                                                             
         MVI   DI_LOUT,3                                                        
         MVC   0(3,RF),=C'FRZ'                                                  
         BR    RE                                                               
                                                                                
***********************************************************************         
* Edit station lock                                                   *         
***********************************************************************         
                                                                                
EDTSLCK  LM    R2,R3,DI_AINP                                                    
         MVI   0(R3),C'N'                                                       
         TM    0(R2),X'04'         TEST STATION LOCK                            
         JZ    *+8                                                              
         MVI   0(R3),C'Y'                                                       
         BR    RE                                                               
                                                                                
***********************************************************************         
* Edit country                                                        *         
***********************************************************************         
                                                                                
EDTCTRY  NTR1  LABEL=NO                                                         
         LM    R2,R3,DI_AINP                                                    
         MVI   0(R3),C'U'          ASSUME IT'S U.S.                             
         CLI   0(R2),C'C'          CANADA?                                      
         JNE   *+8                                                              
         MVI   0(R3),C'C'                                                       
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Edit locked month                                                   *         
***********************************************************************         
                                                                                
EDTLOCK  NTR1  LABEL=NO                                                         
         LM    R2,R3,DI_AINP                                                    
         OC    0(L'ELOCKYM,R2),0(R2)                                            
         JZ    EXIT                                                             
         MVC   WORK(L'ELOCKYM),0(R2)                                            
         NI    WORK+1,FF-X'C0'                                                  
         GOTOR CDATCON,DMCB,(3,WORK),(6,(R3))                                   
         TM    L'ELOCKYR(R2),X'80'                                              
         JZ    *+8                                                              
         MVI   6(R3),C'-'          ...AND PRIOR                                 
         TM    L'ELOCKYR(R2),X'40'                                              
         JZ    *+8                                                              
         MVI   6(R3),C'+'          ...AND SUBSEQUENT                            
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Edit client code                                                    *         
***********************************************************************         
                                                                                
EDTCLT   NTR1  LABEL=NO                                                         
         GOTOR GETCLT,DI_AINP      GET CLIENT RECORD                            
         LA    R2,BUFFRECD                                                      
         USING CLTRECD,R2          R2=A(CLIENT RECORD)                          
         LM    RF,R0,DI_AINP       RF=A(INPUT),R0=A(OUTPUT)                     
         GOTOR VCLUNPK,DMCB,(CPROF+6,(RF)),(R0)                                 
         MVI   DI_LOUT,3                                                        
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Edit product code                                                   *         
***********************************************************************         
                                                                                
EDTPRDB  NTR1  LABEL=NO            PRODUCT EDIT FOR BFORM                       
         L     R2,DI_AINP                                                       
         L     RF,DI_AOUT          RF=A(OUTPUT)                                 
         MVI   DI_LOUT,3                                                        
         CLI   0(R2),0             TEST PRODUCT PRESENT                         
         JE    EXIT                                                             
         MVC   0(L'CPLPMNEM,RF),0(R2)                                           
         CLC   =C'AAA',0(R2)                                                    
         JNE   EXIT                                                             
         MVC   0(L'CPLPMNEM,RF),=C'ALL'                                         
         J     EXIT                                                             
                                                                                
*                                                                               
EDTPRD   NTR1  LABEL=NO            PRODUCT EDIT FOR BFORM                       
         L     R2,DI_AINP                                                       
         CLI   0(R2),0             TEST PRODUCT PRESENT                         
         JE    EXIT                                                             
         GOTOR GETCLT,DMCB,RCLT    GET CLIENT RECORD                            
         LA    R1,BUFFRECD+(CPLDATA-CLTRECD)                                    
         USING CPLDATA,R1          R1=A(PRODUCT LIST)                           
         LHI   R0,CPLDMAXN         R0=MAXIMUM NUMBER OF PRODUCTS                
         L     RF,DI_AOUT          RF=A(OUTPUT)                                 
EDTPRD02 CLC   CPLPNUMB,0(R2)      MATCH PRODUCT NUMBER TO INPUT                
         JNE   *+14                                                             
         MVC   0(L'CPLPMNEM,RF),CPLPMNEM                                        
         J     EXIT                                                             
         AHI   R1,CPLDATAL         BUMP TO NEXT PRODUCT                         
         JCT   R0,EDTPRD02                                                      
         MVC   0(L'CPLPMNEM,RF),=C'???'                                         
         J     EXIT                                                             
         DROP  R1                                                               
                                                                                
***********************************************************************         
* Edit estimate                                                       *         
***********************************************************************         
                                                                                
EDTEST   NTR1  LABEL=NO            ESTIMATE EDIT                                
         L     R2,DI_AINP                                                       
         L     RF,DI_ADRD                                                       
         MVI   DI_LOUT,3                                                        
                                                                                
         CLI   2(RF),BFMRECQ       BFORM?                                       
         JNE   EDTEST02                                                         
         L     RF,DI_AOUT          RF=A(OUTPUT)                                 
         MVC   0(L'CPLPMNEM,RF),=C'ALL'                                         
         CLI   0(R2),0             TEST ESTIMATE PRESENT                        
         JE    EXIT                                                             
         EDITR (1,(R2)),(3,(RF)),ZERO=BLANK,ALIGN=LEFT                          
         J     EXIT                                                             
                                                                                
EDTEST02 CLI   2(RF),AORRECQ       AOR?                                         
         JNE   EXIT                                                             
         L     RF,DI_AOUT          RF=A(OUTPUT)                                 
         MVC   0(L'CPLPMNEM,RF),=C'ALL'                                         
         CLI   0(R2),X'FF'                                                      
         JE    EXIT                                                             
         EDITR (1,1(R2)),(3,(RF)),ZERO=BLANK,ALIGN=LEFT                         
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Edit market                                                         *         
***********************************************************************         
                                                                                
EDTMKT   NTR1  LABEL=NO            ESTIMATE EDIT                                
         L     R2,DI_AINP                                                       
         L     RF,DI_ADRD                                                       
         MVI   DI_LOUT,3                                                        
                                                                                
         CLI   2(RF),BFMRECQ       BFORM?                                       
         JNE   EDTMKT02                                                         
         L     RF,DI_AOUT          RF=A(OUTPUT)                                 
         MVC   0(L'CPLPMNEM,RF),=C'ALL'                                         
         CLI   0(R2),0             TEST ESTIMATE PRESENT                        
         JE    EXIT                                                             
         MVI   DI_LOUT,4                                                        
         EDITR (2,(R2)),(4,(RF)),ZERO=BLANK,ALIGN=LEFT                          
         J     EXIT                                                             
                                                                                
EDTMKT02 DS    0H                                                               
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Edit network percent                                                *         
***********************************************************************         
                                                                                
EDTNWPCT L     R1,DI_AINP                                                       
         L     RF,DI_AOUT                                                       
         CLI   0(R1),FF            TEST 'NOT BOUGHT'                            
         JNE   *+12                                                             
         MVC   0(2,RF),=C'NB'                                                   
         BR    RE                                                               
         ICM   R0,15,0(R1)         GET VALUE                                    
         EDITR (R0),(7,WORK),3,ALIGN=LEFT                                       
         MVC   0(7,RF),WORK                                                     
         BR    RE                                                               
                                                                                
***********************************************************************         
* Edit Cost2 precent                                                  *         
***********************************************************************         
                                                                                
EDTCOST2 L     RF,DI_AINP                                                       
         OC    0(L'ECOST2,RF),0(RF)                                             
         BZR   RE                                                               
         ICM   R0,15,0(RF)                                                      
         CLI   0(RF),X'80'                                                      
         JNE   *+6                                                              
         SR    R0,R0                                                            
         EDITR (R0),(8,WORK),6,ALIGN=LEFT,FILL=0,DROP=5                         
         L     RF,DI_AOUT                                                       
         MVC   0(8,RF),WORK                                                     
         BR    RE                                                               
                                                                                
***********************************************************************         
* Edit profit within precent                                          *         
***********************************************************************         
                                                                                
EDTPWP   L     R1,DI_AINP                                                       
         OC    0(L'EPWPCT,R1),0(R1)                                             
         BZR   RE                                                               
         L     RF,DI_AOUT                                                       
         CLI   0(R1),X'80'                                                      
         JNE   *+10                                                             
         MVI   0(RF),C'0'                                                       
         BR    RE                                                               
         SR    R0,R0                                                            
         ICM   R0,7,0(R1)                                                       
         EDITR (R0),(8,WORK),2,ALIGN=LEFT,TRAIL=0,FLOAT=-                       
         MVC   0(8,RF),WORK                                                     
         BR    RE                                                               
                                                                                
***********************************************************************         
* Edit Rep code                                                       *         
***********************************************************************         
                                                                                
EDTREP   NTR1  LABEL=NO                                                         
         LM    R2,R3,DI_AINP                                                    
         OC    0(L'EREP,R2),0(R2)                                               
         JZ    EXIT                                                             
         GOTOR VRCPACK,DMCB,(C'U',(R2)),(R3)                                    
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Edit request range                                                  *         
***********************************************************************         
                                                                                
EDTREQR  L     R1,DI_AINP                                                       
         OC    0(L'EREQLO+L'EREQHI,R1),0(R1)                                    
         BZR   RE                                                               
         L     RF,DI_AOUT                                                       
         CLC   0(2,R1),=C'NO'                                                   
         JNE   *+12                                                             
         MVC   0(2,RF),0(R1)                                                    
         BR    RE                                                               
         SR    R0,R0                                                            
         IC    R0,0(R1)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(3,RF),DUB                                                      
         MVI   3(RF),C'-'                                                       
         IC    R0,L'EREQLO(R1)                                                  
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  4(3,RF),DUB                                                      
         BR    RE                                                               
                                                                                
***********************************************************************         
* Edit media office                                                   *         
***********************************************************************         
                                                                                
EDTOFF   NTR1  LABEL=NO            *DISPLAY OFFICE CODE AND NAME*               
         XC    OFCBLK,OFCBLK                                                    
         LA    R3,OFCBLK           USING WORK FOR OFCBLOCK                      
         USING OFFICED,R3                                                       
*                                                                               
         L     R2,DI_ADAD          CURRENT AGENCY/MEDIA TABLE                   
         USING DA_D,R2             R2=A(AGENCY/MEDIA TABLE)                     
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAGY,DA_ALF       AGENCY ALPHA ID                              
         DROP  R2                                                               
*                                                                               
         L     R1,DI_AINP                                                       
         MVC   OFCOFC,0(R1)                                                     
         GOTOR VOFFICER,DMCB,(C'2',OFFICED),(X'01',DI_ACOM),           X        
               (C'S',DUB)                                                       
         L     R1,DI_AOUT                                                       
         MVC   0(2,R1),OFCOFC2                                                  
         MVI   DI_LINP,2           2 BYTE OUTPUT                                
         J     EXIT                                                             
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Edit Effective MOS                                                  *         
***********************************************************************         
                                                                                
EDTEMOS  NTR1  LABEL=NO                                                         
         L     R2,DI_AINP          ** BFORM 10 ELEMENT KEY **                   
         L     R3,DI_AOUT                                                       
         MVC   WORK(L'BFRCDDTE),0(R2)                                           
         XC    WORK(2),=2X'FF'     IT WAS COMPLEMENTED                          
         MVI   WORK+2,X'01'                                                     
         GOTOR CDATCON,DMCB,(3,WORK),(9,(R3))                                   
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Edit bill basis (Estimate & Product)                                *         
***********************************************************************         
                                                                                
EDTBBASE L     R1,DI_AINP                                                       
         MVC   WORK(L'EBILLBAS),0(R1)                                           
         NI    WORK,X'F0'                                                       
*                                                                               
         CLI   WORK,0                 ANY BILL BASIS?                           
         JNE   EDTBBE2                                                          
         OC    1(L'EBILLCOM,R1),1(R1) ANY COMM PCT? (E/PBILLCOM)                
         BZR   RE                                                               
         MVC   WORK(5),=C'GROSS'                                                
         J     EDTBBE4                                                          
*                                                                               
EDTBBE2  CLI   WORK,X'80'                                                       
         JNE   *+14                                                             
         MVC   WORK(5),=C'GROSS'                                                
         J     EDTBBE4                                                          
         CLI   WORK,X'10'                                                       
         JNE   *+14                                                             
         MVC   WORK(5),=C'NET  '                                                
         J     EDTBBE4                                                          
         CLI   WORK,X'C0'                                                       
         JNE   *+14                                                             
         MVC   WORK(5),=C'CGROS'                                                
         J     EDTBBE4                                                          
         CLI   WORK,X'40'                                                       
         JNE   *+14                                                             
         MVC   WORK(5),=C'CGROS'                                                
         J     EDTBBE4                                                          
         CLI   WORK,X'50'                                                       
         JNE   *+10                                                             
         MVC   WORK(5),=C'CNET '                                                
*                                                                               
EDTBBE4  L     R1,DI_AOUT                                                       
         MVC   0(5,R1),WORK                                                     
         BR    RE                                                               
                                                                                
***********************************************************************         
* Edit commission basis (Estimate & Product)                          *         
***********************************************************************         
                                                                                
EDTCBASE L     R1,DI_AINP                                                       
         MVC   WORK(L'EBILLBAS),0(R1)                                           
         NI    WORK,X'0F'                                                       
*                                                                               
         CLI   WORK,0                 ANY BILL BASIS?                           
         JNE   EDTCBE2                                                          
         OC    1(L'EBILLCOM,R1),1(R1) ANY COMM PCT? (E/PBILLCOM)                
         BZR   RE                                                               
         MVC   WORK(5),=C'GROSS'                                                
         J     EDTCBE4                                                          
*                                                                               
EDTCBE2  CLI   WORK,X'01'                                                       
         JNE   *+10                                                             
         MVC   WORK(5),=C'NET  '                                                
*                                                                               
EDTCBE4  L     R1,DI_AOUT                                                       
         MVC   0(5,R1),WORK                                                     
         BR    RE                                                               
                                                                                
***********************************************************************         
* Edit bill basis BFORM                                               *         
***********************************************************************         
                                                                                
EDTBBASB L     R1,DI_AINP                                                       
         MVC   WORK(L'EBILLBAS),0(R1)                                           
         NI    WORK,X'F0'                                                       
*                                                                               
         CLI   WORK,0                 ANY BILL BASIS?                           
         JNE   *+14                                                             
         MVC   WORK(5),=C'GROSS'                                                
         J     EDTBBB2                                                          
*                                                                               
         CLI   WORK,X'10'                                                       
         JNE   *+14                                                             
         MVC   WORK(5),=C'NET  '                                                
         J     EDTBBB2                                                          
         CLI   WORK,X'40'                                                       
         JNE   *+14                                                             
         MVC   WORK(5),=C'CGROS'                                                
         J     EDTBBB2                                                          
         CLI   WORK,X'50'                                                       
         JNE   *+10                                                             
         MVC   WORK(5),=C'CNET '                                                
*                                                                               
EDTBBB2  L     R1,DI_AOUT                                                       
         MVC   0(5,R1),WORK                                                     
         BR    RE                                                               
                                                                                
***********************************************************************         
* Edit commission basis BFORM                                         *         
***********************************************************************         
                                                                                
EDTCBASB L     R1,DI_AINP                                                       
         MVC   WORK(L'EBILLBAS),0(R1)                                           
         NI    WORK,X'0F'                                                       
*                                                                               
         CLI   WORK,0                 ANY BILL BASIS?                           
         JNE   EDTCBE2                                                          
         OC    1(L'EBILLCOM,R1),1(R1) ANY COMM PCT? (E/PBILLCOM)                
         BZR   RE                                                               
         MVC   WORK(5),=C'GROSS'                                                
         J     EDTCBB4                                                          
*                                                                               
EDTCBB2  CLI   WORK,X'01'                                                       
         JNE   *+10                                                             
         MVC   WORK(5),=C'NET  '                                                
*                                                                               
EDTCBB4  L     R1,DI_AOUT                                                       
         MVC   0(5,R1),WORK                                                     
         BR    RE                                                               
                                                                                
***********************************************************************         
* Edit Effective Date - displaying MMM/YY                             *         
***********************************************************************         
                                                                                
EDTEFF   L     R2,DI_AINP                                                       
         L     R3,DI_AOUT                                                       
         OC    0(L'AOREFF,R2),0(R2)   ANY DATA?  (2 BYTES)                      
         BZR   RE                   - NOPE                                      
*                                                                               
         LR    R0,RE               SAVE OFF RE                                  
         GOTOR CDATCON,DMCB,(3,(R2)),(6,(R3))                                   
         LR    RE,R0               RESTORE RE                                   
*                                                                               
EDTEFFX  BR    RE                                                               
                                                                                
***********************************************************************         
* Edit Effective Date 2 - displaying MMMDD/YY                         *         
***********************************************************************         
                                                                                
EDTEFF2  L     R2,DI_AINP                                                       
         L     R3,DI_AOUT                                                       
         OC    0(L'PCTADATE,R2),0(R2)   ANY DATA?  (3 BYTES)                    
         BZR   RE                   - NOPE                                      
*                                                                               
         LR    R0,RE               SAVE OFF RE                                  
         GOTOR CDATCON,DMCB,(3,(R2)),(5,(R3))                                   
         LR    RE,R0               RESTORE RE                                   
*                                                                               
EDTEFF2X BR    RE                                                               
                                                                                
***********************************************************************         
* Edit Effective Date                                                 *         
***********************************************************************         
                                                                                
EDTBAS   L     R2,DI_AINP                                                       
         L     R3,DI_AOUT                                                       
         MVC   0(2,R3),=C'G '                                                   
         CLI   0(R2),0                                                          
         JE    EDTBASX                                                          
         MVC   0(2,R3),=C'A '                                                   
         CLI   0(R2),X'80'                                                      
         JE    EDTBASX                                                          
         MVC   0(2,R3),=C'N '                                                   
         CLI   0(R2),X'40'                                                      
         JE    EDTBASX                                                          
         MVC   0(2,R3),=C'XX'                                                   
*                                                                               
EDTBASX  BR    RE                                                               
                                                                                
***********************************************************************         
* Edit PST Code                                                       *         
***********************************************************************         
                                                                                
EDTPST   L     R2,DI_AINP                                                       
         L     R3,DI_AOUT                                                       
         OC    0(L'AORPST,R2),0(R2)   ANY DATA?                                 
         BZR   RE                   - NOPE                                      
*                                                                               
         LA    R4,BLOCK                                                         
         USING PSTBLKD,R4                                                       
         XC    0(200,R4),0(R4)     CLEAR INTERFACE BLOCK                        
         XC    200(200,R4),200(R4)                                              
         MVI   PSTACT,PSTFMTQ      ACTION = FORMAT                              
         ST    R2,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         ST    R3,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,DI_ACOM     A(COMFACS)                                   
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'                                           
         GOTOR CCALLOV,DMCB                                                     
         CLI   DMCB+4,X'FF'                                                     
         JNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTOR (RF),DMCB,(R4)                                                   
*                                                                               
EDTPSTX  J     EXIT                                                             
                                                                                
***********************************************************************         
* Edit Character ALL                                                  *         
***********************************************************************         
                                                                                
EDTCALL  L     R2,DI_AINP                                                       
         L     R3,DI_AOUT                                                       
         MVI   DI_LOUT,1                                                        
         CLI   0(R2),0             ANY DATA?                                    
         JE    EDTCALLX             - NOPE                                      
*                                                                               
         MVC   0(1,R3),0(R2)                                                    
         CLI   0(R2),X'FF'         IS IT ALL?                                   
         JNE   EDTCALLX                                                         
         MVC   0(3,R3),=C'ALL'                                                  
         MVI   DI_LOUT,3                                                        
*                                                                               
EDTCALLX BR    RE                                                               
                                                                                
***********************************************************************         
* Edit Percentage                                                     *         
***********************************************************************         
                                                                                
EDTPCT   L     R2,DI_AINP                                                       
         L     R3,DI_AOUT                                                       
         MVI   DI_LOUT,5                                                        
         OC    0(L'PCTPCT,R2),0(R2)   ANY DATA?                                 
         JZ    EDTPCTX              - NOPE                                      
*                                                                               
         TM    0(R2),X'80'         IS IT ZERO?                                  
         JZ    EDTPCT20                                                         
         MVC   0(3,R3),=C'0.0'                                                  
         MVI   DI_LOUT,3                                                        
         J     EDTPCTX                                                          
*                                                                               
EDTPCT20 EDITR (2,(R2)),(5,(R3)),1,ALIGN=LEFT                                   
         MVI   DI_LOUT,5                                                        
*                                                                               
EDTPCTX  BR    RE                                                               
                                                                                
***********************************************************************         
* Edit Bill % Split                                                   *         
***********************************************************************         
                                                                                
EDTBPS   L     R2,DI_AINP                                                       
         L     R3,DI_AOUT                                                       
         MVI   DI_LOUT,2                                                        
         MVC   0(2,R3),=C'NO'                                                   
         TM    0(R2),COP4BPCT      X'10' BILL PCT SPLIT                         
         JZ    EDTPCTX              - NOPE                                      
*                                                                               
         MVI   DI_LOUT,3                                                        
         MVC   0(3,R3),=C'YES'      - IT'S THERE                                
*                                                                               
EDTBPSX  BR    RE                                                               
                                                                                
***********************************************************************         
* Edit PID                                                            *         
***********************************************************************         
                                                                                
EDTPID   L     R2,DI_AINP                                                       
         MVI   DI_LOUT,1                                                        
         OC    0(L'ADDRCHBY,R2),0(R2)   ANY DATA?                               
         JZ    EDTPIDX              - NOPE                                      
*                                                                               
         LA    R3,KEY                                                           
         USING AGYHDRD,R3                                                       
         XC    AGYKEY,AGYKEY                                                    
         MVI   AGYKTYPE,AGYKTYPQ                                                
         L     RF,DI_ADSD          EXTRACT AGENCY ALPHA                         
         MVC   AGYKAGY,DS_ALF-DS_D(RF)                                          
         LR    R0,RE               SAVE RE                                      
         GOTOR CDATAMGR,DMCB,DMRDHI,SPTDIR,AGYHDRD,AGYHDRD                      
         JE    *+6                 TEST FOR DATAMGR ERRORS                      
         DC    H'0'                                                             
         LR    RE,R0               RESTORE RE                                   
*                                                                               
         CLI   AGYKTYPE,AGYKTYPQ                                                
         JNE   EDTPIDX                                                          
         L     RF,DI_ADSD          EXTRACT AGENCY ALPHA                         
         CLC   AGYKAGY,DS_ALF-DS_D(RF)                                          
         JNE   EDTPIDX                                                          
*                                                                               
         LR    R0,RE               SAVE RE                                      
         GOTOR CDATAMGR,DMCB,GETREC,SPTFIL,AGYKDA,BUFFRECD,WORK                 
         JE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0               RESTORE RE                                   
         LA    R3,BUFFRECD         POINT TO RECORD                              
         MVC   WORK+2(2),AGYKAGY   SET AGENCY ALPHA ID                          
         DROP  R3                                                               
*                                                                               
         MVC   WORK(2),0(R2)                                                    
         CLC   WORK(2),=X'0FFF'    DDS PID?                                     
         JH    EDTPID20             - NOPE                                      
         MVC   0(3,R3),=C'DDS'                                                  
         MVI   DI_LOUT,3                                                        
         J     EDTPIDX                                                          
*                                                                               
EDTPID20 DS    0H                                                               
         LA    RF,KEY                                                           
         USING CT0REC,RF                                                        
         XC    KEY,KEY                                                          
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,WORK+2      SECURITY AGENCY                              
         MVC   CT0KNUM,WORK        PID                                          
         DROP  RF                                                               
         LR    R0,RE               SAVE RE                                      
         GOTOR CDATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,BUFFRECD             
         LR    RE,R0               RESTORE RE                                   
         LA    RF,BUFFRECD                                                      
         CLC   KEY(25),0(RF)                                                    
         JNE   EDTPIDX                                                          
*                                                                               
         LA    RF,28(RF)           POINT TO THE FIRST ELEMENT                   
EDTPID50 CLI   0(RF),0             WE DONE?                                     
         JE    EDTPIDX              - YUP                                       
         CLI   0(RF),X'C3'         ID ELEMENT?                                  
         JE    EDTPID90                                                         
*                                                                               
         XR    R0,R0               LET'S BUMP IT                                
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         J     EDTPID50                                                         
*                                                                               
EDTPID90 L     R3,DI_AOUT                                                       
         MVC   0(8,R3),2(RF)       MOVE IN THE ID                               
         MVI   DI_LOUT,8                                                        
EDTPIDX  BR    RE                                                               
*                                                                               
***********************************************************************         
* Edit commission percentage                                          *         
***********************************************************************         
                                                                                
EDTCOMP  NTR1  LABEL=NO                                                         
         L     R1,DI_AINP                                                       
         OC    0(L'EPWPCT,R1),0(R1)                                             
         JZ    EXIT                                                             
         ICM   R0,15,0(R1)                                                      
         LR    R5,R0               SAVE OFF R0                                  
         EDITR (R0),(9,WORK),4,ALIGN=LEFT,FLOAT=+                               
         L     RF,DI_AOUT                                                       
         MVC   0(9,RF),WORK                                                     
***      TM    0(R1),X'80'                                                      
***      JZ    *+8                                                              
         LTR   R5,R5                                                            
         JNM   *+8                 COM.% NEGATIVE, MINUS SIGN                   
         MVI   0(RF),C'-'                                                       
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Edit Client Group ID - Client Record                                *         
***********************************************************************         
EDTGRPC  L     R2,DI_AINP                                                       
         L     R3,DI_AOUT                                                       
*                                                                               
         LA    R1,SPCGRTAB                                                      
         LHI   R0,(SPCGRTBX-SPCGRTAB)/3                                         
*                                                                               
EGRPC10  CLC   0(1,R2),2(R1)                                                    
         JE    EGRPC15                                                          
         LA    R1,3(R1)                                                         
         JCT   R0,EGRPC10                                                       
         J     EGRPCX                                                           
*                                                                               
EGRPC15  MVC   RCLGID,0(R1)        SAVE IT                                      
*                                                                               
         L     R3,DI_AREC                                                       
         USING CLTRECD,R3                                                       
*                                                                               
         XC    KEY,KEY             GET CGRDEF RECORD                            
         MVC   KEY(2),=X'0D06'                                                  
         MVC   KEY+2(1),CKEYAM                                                  
         MVC   KEY+3(1),0(R2)                                                   
*                                                                               
         LR    R0,RE               SAVE RE                                      
         GOTOR CDATAMGR,DMCB,DMRDHI,SPTDIR,KEY,KEY                              
         JE    *+6                 TEST FOR DATAMGR ERRORS                      
         DC    H'0'                                                             
         LR    RE,R0               RESTORE RE                                   
*                                                                               
         LR    R0,RE               SAVE RE                                      
         GOTOR CDATAMGR,DMCB,GETREC,SPTFIL,KEY+14,BUFFRECD,WORK                 
         JE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0               RESTORE RE                                   
*                                                                               
         USING CLGRECD,R3                                                       
         XC    FULL,FULL                                                        
         MVC   FULL(2),1(R2)       CGRP+1                                       
         UNPK  WORK(5),FULL(3)                                                  
*                                                                               
         LA    R3,BUFFRECD         POINT TO RECORD                              
         ZIC   RF,CLGBK1LN                                                      
         ZIC   R2,CLGBK2LN                                                      
         AR    RF,R2               GET LENGTH OF ID                             
*                                                                               
         LA    R1,DUB                                                           
         LA    R2,WORK                                                          
         XC    DUB,DUB                                                          
*                                                                               
EGRPC20  MVC   0(1,R1),0(R2)                                                    
         AHI   R1,1                                                             
         AHI   R2,1                                                             
         JCT   RF,EGRPC20                                                       
*                                                                               
         L     R3,DI_AOUT                                                       
         MVC   0(2,R3),RCLGID                                                   
         CLI   1(R3),C' '                                                       
         JNE   EGRPC30                                                          
         MVC   1(4,R3),DUB                                                      
         J     *+10                                                             
EGRPC30  MVC   2(4,R3),DUB                                                      
*                                                                               
EGRPC40  MVI   DI_LOUT,5                                                        
         CLI   4(R3),0                                                          
         JE    *+8                                                              
         MVI   DI_LOUT,6                                                        
*                                                                               
EGRPCX   BR    RE                                                               
         DROP  R3                                                               
***********************************************************************         
* Edit Client Group ID                                                *         
***********************************************************************         
                                                                                
EDTGRID  L     R2,DI_AINP                                                       
         L     R3,DI_AOUT                                                       
*                                                                               
         LA    R1,SPCGRTAB                                                      
         LHI   R0,(SPCGRTBX-SPCGRTAB)/3                                         
*                                                                               
EGRID10  CLC   0(1,R2),2(R1)                                                    
         JE    *+12                                                             
         LA    R1,3(R1)                                                         
         JCT   R0,EGRID10                                                       
*                                                                               
         MVC   0(2,R3),0(R1)                                                    
         MVC   RCLGID,0(R3)        SAVE IT                                      
         MVI   DI_LOUT,2                                                        
         BR    RE                                                               
*                                                                               
***********************************************************************         
* Edit Product Group ID                                               *         
***********************************************************************         
                                                                                
EDTGRPP  L     R3,DI_AIO1                                                       
         USING PRGRECD,R3                                                       
*                                                                               
         XC    KEY,KEY             GET CGRDEF RECORD                            
         MVC   KEY(6),0(R3)                                                     
*                                                                               
         LR    R0,RE               SAVE RE                                      
         GOTOR CDATAMGR,DMCB,DMRDHI,SPTDIR,KEY,KEY                              
         JE    *+6                 TEST FOR DATAMGR ERRORS                      
         DC    H'0'                                                             
         LR    RE,R0               RESTORE RE                                   
*                                                                               
         LR    R0,RE               SAVE RE                                      
         GOTOR CDATAMGR,DMCB,GETREC,SPTFIL,KEY+14,BUFFRECD,WORK                 
         JE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0               RESTORE RE                                   
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),PRGKGRP                                                  
         UNPK  WORK(5),FULL(3)                                                  
*                                                                               
         LA    R3,BUFFRECD         POINT TO RECORD                              
         LA    R3,PRGEL                                                         
         USING PRGEL01,R3                                                       
         ZIC   RF,PRGBK1LN                                                      
         ZIC   R2,PRGBK2LN                                                      
         AR    RF,R2               GET LENGTH OF ID                             
*                                                                               
         LA    R1,DUB                                                           
         LA    R2,WORK                                                          
         XC    DUB,DUB                                                          
*                                                                               
EGRPP10  MVC   0(1,R1),0(R2)                                                    
         AHI   R1,1                                                             
         AHI   R2,1                                                             
         JCT   RF,EGRPP10                                                       
*                                                                               
         L     R3,DI_AOUT                                                       
         MVC   0(2,R3),RCLGID                                                   
         CLI   1(R3),C' '                                                       
         JNE   EGRPP20                                                          
         MVC   1(4,R3),DUB                                                      
         MVI   DI_LOUT,5                                                        
         BR    RE                                                               
EGRPP20  MVC   2(4,R3),DUB                                                      
         MVI   DI_LOUT,6                                                        
EGRPPX   BR    RE                                                               
         DROP  R3                                                               
***********************************************************************         
* Edit Client Group ID                                                *         
***********************************************************************         
                                                                                
EDTGRP   L     R3,DI_AIO1                                                       
         USING CLGRECD,R3                                                       
*                                                                               
         XC    KEY,KEY             GET CGDEF RECORD                             
         MVC   KEY(4),0(R3)                                                     
*                                                                               
         LR    R0,RE               SAVE RE                                      
         GOTOR CDATAMGR,DMCB,DMRDHI,SPTDIR,KEY,KEY                              
         JE    *+6                 TEST FOR DATAMGR ERRORS                      
         DC    H'0'                                                             
         LR    RE,R0               RESTORE RE                                   
*                                                                               
         LR    R0,RE               SAVE RE                                      
         GOTOR CDATAMGR,DMCB,GETREC,SPTFIL,KEY+14,BUFFRECD,WORK                 
         JE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0               RESTORE RE                                   
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),CLGKGRP                                                  
         UNPK  WORK(5),FULL(3)                                                  
*                                                                               
         LA    R3,BUFFRECD         POINT TO RECORD                              
         ZIC   RF,CLGBK1LN                                                      
         ZIC   R2,CLGBK2LN                                                      
         AR    RF,R2               GET LENGTH OF ID                             
*                                                                               
         LA    R1,DUB                                                           
         LA    R2,WORK                                                          
         XC    DUB,DUB                                                          
*                                                                               
EGRP10   MVC   0(1,R1),0(R2)                                                    
         AHI   R1,1                                                             
         AHI   R2,1                                                             
         JCT   RF,EGRP10                                                        
*                                                                               
         L     R3,DI_AOUT                                                       
         MVC   0(2,R3),RCLGID                                                   
         CLI   1(R3),C' '                                                       
         JNE   EGRP20                                                           
         MVC   1(4,R3),DUB                                                      
         MVI   DI_LOUT,5                                                        
         BR    RE                                                               
EGRP20   MVC   2(4,R3),DUB                                                      
         MVI   DI_LOUT,6                                                        
         BR    RE                                                               
         DROP  R3                                                               
*                                                                               
***********************************************************************         
* Edit station master record's client code                                      
***********************************************************************         
                                                                                
EDTCLTM  NTR1  LABEL=NO                                                         
         LM    RE,RF,DI_AINP       RE=A(INPUT),RF=A(OUTPUT)                     
         MVC   0(L'STAKCLT,RF),0(RE)                                            
         CLC   0(L'STAKCLT,RF),=C'000'                                          
         JNE   *+10                                                             
         MVC   0(L'STAKCLT,RF),=C'ALL'                                          
         MVI   DI_LOUT,3                                                        
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Filter routine for NETDEF records (rejects CBLDEF records)          *         
***********************************************************************         
                                                                                
CNDFLT   L     R2,DI_AREC          POINT TO INPUT RECORD                        
         AHI   R2,NDEFEL-NDEFRECD                                               
         USING NDEFEL02,R2         LOCATE CABLE/NETWORK ELEMENT                 
         SR    R0,R0                                                            
CNDFLT02 CLI   NDEFEL02,EOR        NOT FOUND - IS NETWORK                       
         BER   RE                                                               
         CLI   NDEFEL02,NDEFNELQ                                                
         JE    *+14                                                             
         IC    R0,NDEFEL02+1                                                    
         AR    R2,R0                                                            
         J     CNDFLT02                                                         
         CLI   NDEFNET,NDEFCABQ    TEST CABLE                                   
         J     SETCCC              FLIP CONDITION CODE                          
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Filter routine for CBLDEF records (rejects NETDEF records)          *         
***********************************************************************         
                                                                                
CCDFLT   L     R2,DI_AREC          POINT TO INPUT RECORD                        
         AHI   R2,NDEFEL-NDEFRECD                                               
         USING NDEFEL02,R2         LOCATE CABLE/NETWORK ELEMENT                 
         SR    R0,R0                                                            
CCDFLT02 CLI   NDEFEL02,EOR        NOT FOUND - IS NETWORK                       
         JE    SETCCC                                                           
         CLI   NDEFEL02,NDEFNELQ                                                
         JE    *+14                                                             
         IC    R0,NDEFEL02+1                                                    
         AR    R2,R0                                                            
         J     CCDFLT02                                                         
         CLI   NDEFNET,NDEFCABQ    TEST CABLE                                   
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Exits etc.                                                          *         
***********************************************************************         
                                                                                
SETCCC   JE    *+8                 SET CONVERSE CONDITION CODE                  
         CR    RE,RE               NOT EQUAL TO EQUAL/NOT ZERO TO ZERO          
         BR    RE                                                               
         LTR   RE,RE               EQUAL TO NOT EQUAL/ZERO TO NOT ZERO          
         BR    RE                                                               
                                                                                
EXITL    DS    0H                  SET CONDITION CODE TO LOW                    
EXITN    LHI   RE,0                SET CONDITION CODE TO NOT EQUAL              
         J     EXITCC                                                           
EXITY    LHI   RE,1                SET CONDITION CODE TO EQUAL                  
         J     EXITCC                                                           
EXITH    LHI   RE,2                SET CONDITION CODE TO HIGH                   
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* File table                                                          *         
***********************************************************************         
                                                                                
FILTAB   DS    0XL(DF_LNQ)                                                      
                                                                                
         DC    X'21',CL(L'DF_NAMEF)'SPTFIL'                                     
         DC    AL1(DF_TDA),AL1(13),AL1(1),AL1(24),AL4(FILRECS)                  
                                                                                
         DC    X'37',CL(L'DF_NAMEF)'XSPFIL'                                     
         DC    AL1(DF_TDA),AL1(32),AL1(1),AL1(42),AL4(XFILRECS)                 
                                                                                
         DC    X'22',CL(L'DF_NAMEF)'STAFILE'                                    
         DC    AL1(DF_TIS),AL1(15),AL1(1),AL1(00),AL4(STARECS)                  
                                                                                
FILTABX  DC    AL1(DF_EOTQ)                                                     
                                                                                
***********************************************************************         
* Record definition tables                                            *         
***********************************************************************         
                                                                                
FILRECS  DS    0X                  ** SPTFIL RECORD DEFINITIONS **              
                                                                                
FILCLT   DS    0X                  ** CLIENT RECORD DEFINITION **               
         DC    AL1(FILCLTX-*)                                                   
CLTRECQ  EQU   1                   RECORD INDENTIFIER                           
         DC    AL2(CLTRECQ)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    CL(L'DR_NAMEF)'CLIENT'                                           
         DC    CL(L'DR_NAMES)'Client'                                           
         DC    AL1(CKEYAM-CKEY)                                                 
         DC    AL1(CKEYAM-CKEY)                                                 
         DC    AL1(0)                                                           
         DC    AL1(DR_ITAGM)                                                    
         DC    AL4(0)                                                           
         DC    AL4(CLTKEY)                                                      
         DC    AL4(CLTFLD)                                                      
                                                                                
CLTARG1  DC    AL1(CLTARG2-*)                                                   
         DC    AL1(CKEYTYPE-CKEY)                                               
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(CKEYTYPQ)                                                    
                                                                                
CLTARG2  DC    AL1(CLTARG3-*)                                                   
         DC    AL1(CKEYAM-CKEY)                                                 
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'CKEYAM)                                                    
                                                                                
CLTARG3  DC    AL1(CLTARG4-*)                                                   
         DC    AL1(CKEYCLT-CKEY)                                                
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'CKEYCLT)                                                   
                                                                                
CLTARG4  DC    AL1(CLTARG5-*)                                                   
         DC    AL1(CKEYREST-CKEY)                                               
         DC    AL1(DR_ATBZ)                                                     
         DC    AL1(L'CKEYREST)                                                  
                                                                                
CLTARG5  DS    0X                                                               
                                                                                
FILCLTX  DS    0X                                                               
                                                                                
FILPRD   DS    0X                  ** PRODUCT RECORD DEFINITION **              
         DC    AL1(FILPRDX-*)                                                   
PRDRECQ  EQU   2                   RECORD INDENTIFIER                           
         DC    AL2(PRDRECQ)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    CL(L'DR_NAMEF)'PRODUCT'                                          
         DC    CL(L'DR_NAMES)'Product'                                          
         DC    AL1(PKEYAM-PKEY)                                                 
         DC    AL1(PKEYAM-PKEY)                                                 
         DC    AL1(0)                                                           
         DC    AL1(DR_ITAGM)                                                    
         DC    AL4(0)                                                           
         DC    AL4(PRDKEY)                                                      
         DC    AL4(PRDFLD)                                                      
                                                                                
PRDARG1  DC    AL1(PRDARG2-*)                                                   
         DC    AL1(PKEYTYPE-PKEY)                                               
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(PKEYTYPQ)                                                    
                                                                                
PRDARG2  DC    AL1(PRDARG3-*)                                                   
         DC    AL1(PKEYAM-PKEY)                                                 
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'PKEYAM)                                                    
                                                                                
PRDARG3  DC    AL1(PRDARG4-*)                                                   
         DC    AL1(PKEYCLT-PKEY)                                                
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'PKEYCLT)                                                   
                                                                                
PRDARG4  DC    AL1(PRDARG5-*)                                                   
         DC    AL1(PKEYPRD-PKEY)                                                
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'PKEYPRD)                                                   
                                                                                
PRDARG5  DC    AL1(PRDARG6-*)                                                   
         DC    AL1(PKEYREST-PKEY)                                               
         DC    AL1(DR_ATBZ)                                                     
         DC    AL1(L'PKEYREST)                                                  
                                                                                
PRDARG6  DS    0X                                                               
                                                                                
FILPRDX  DS    0X                                                               
                                                                                
FILEST   DS    0X                  ** ESTIMATE RECORD DEFINITION **             
         DC    AL1(FILESTX-*)                                                   
ESTRECQ  EQU   3                   RECORD IDENTIFIER                            
         DC    AL2(ESTRECQ)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    CL(L'DR_NAMEF)'ESTIMATE'                                         
         DC    CL(L'DR_NAMES)'Estimate'                                         
         DC    AL1(EKEYAM-EKEY)                                                 
         DC    AL1(EKEYAM-EKEY)                                                 
         DC    AL1(0)                                                           
         DC    AL1(DR_ITAGM)                                                    
         DC    AL4(0)                                                           
         DC    AL4(ESTKEY)                                                      
         DC    AL4(ESTFLD)                                                      
                                                                                
ESTARG1  DC    AL1(ESTARG2-*)                                                   
         DC    AL1(EKEYTYPE-EKEY)                                               
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(EKEYTYPQ)                                                    
                                                                                
ESTARG2  DC    AL1(ESTARG3-*)                                                   
         DC    AL1(EKEYAM-EKEY)                                                 
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'EKEYAM)                                                    
                                                                                
ESTARG3  DC    AL1(ESTARG4-*)                                                   
         DC    AL1(EKEYCLT-EKEY)                                                
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'EKEYCLT)                                                   
                                                                                
ESTARG4  DC    AL1(ESTARG5-*)                                                   
         DC    AL1(EKEYPRD-EKEY)                                                
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'EKEYPRD)                                                   
                                                                                
ESTARG5  DC    AL1(ESTARG6-*)                                                   
         DC    AL1(EKEYEST-EKEY)                                                
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'EKEYEST)                                                   
                                                                                
ESTARG6  DC    AL1(ESTARG7-*)                                                   
         DC    AL1(EKEYREST-EKEY)                                               
         DC    AL1(DR_ATBZ)                                                     
         DC    AL1(L'EKEYREST)                                                  
                                                                                
ESTARG7  DS    0X                                                               
                                                                                
FILESTX  DS    0X                                                               
                                                                                
FILCND   DS    0X                  ** NETDEF RECORD DEFINITION **               
         DC    AL1(FILCNDX-*)                                                   
CNDRECQ  EQU   4                   RECORD IDENTIFIER                            
         DC    AL2(CNDRECQ)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    CL(L'DR_NAMEF)'NETDEF'                                           
         DC    CL(L'DR_NAMES)'Netdef'                                           
         DC    AL1(NDEFKAGY-NDEFKEY)                                            
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(DR_ITALF)                                                    
         DC    AL4(CNDFLT)                                                      
         DC    AL4(CNDKEY)                                                      
         DC    AL4(CNDFLD)                                                      
                                                                                
CNDARG1  DC    AL1(CNDARG2-*)                                                   
         DC    AL1(NDEFKTYP-NDEFKEY)                                            
         DC    AL1(DR_ATEQU)                                                    
         DC    AL2(NDEFRECQ)                                                    
                                                                                
CNDARG2  DS    0X                                                               
                                                                                
FILCNDX  DS    0X                                                               
                                                                                
FILCCD   DS    0X                  ** CBLDEF RECORD DEFINITION **               
         DC    AL1(FILCCDX-*)                                                   
CCDRECQ  EQU   5                   RECORD IDENTIFIER                            
         DC    AL2(CCDRECQ)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    CL(L'DR_NAMEF)'CBLDEF'                                           
         DC    CL(L'DR_NAMES)'Cbldef'                                           
         DC    AL1(NDEFKAGY-NDEFKEY)                                            
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(DR_ITALF)                                                    
         DC    AL4(CCDFLT)                                                      
         DC    AL4(CCDKEY)                                                      
         DC    AL4(CCDFLD)                                                      
                                                                                
CCDARG1  DC    AL1(CCDARG2-*)                                                   
         DC    AL1(NDEFKTYP-NDEFKEY)                                            
         DC    AL1(DR_ATEQU)                                                    
         DC    AL2(NDEFRECQ)                                                    
                                                                                
CCDARG2  DS    0X                                                               
                                                                                
FILCCDX  DS    0X                                                               
                                                                                
FILBFM   DS    0X                  ** BFORM RECORD DEFINITION **                
         DC    AL1(FILBFMX-*)                                                   
BFMRECQ  EQU   6                   RECORD IDENTIFIER                            
         DC    AL2(BFMRECQ)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    CL(L'DR_NAMEF)'BFORM'                                            
         DC    CL(L'DR_NAMES)'BForm'                                            
         DC    AL1(BFKAGYMD-BFKEY)                                              
         DC    AL1(BFKAGYMD-BFKEY)                                              
         DC    AL1(0)                                                           
         DC    AL1(DR_ITAGM)                                                    
         DC    AL4(0)                                                           
         DC    AL4(BFMKEY)                                                      
         DC    AL4(BFMFLD)                                                      
                                                                                
BFMARG1  DC    AL1(BFMARG2-*)                                                   
         DC    AL1(BFKTYPE-BFKEY)                                               
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(BFKTYPEQ,BFKSTYPQ)                                           
                                                                                
BFMARG2  DS    0X                                                               
                                                                                
FILBFMX  DS    0X                                                               
                                                                                
FILAOR   DS    0X                  ** AOR RECORD DEFINITION **                  
         DC    AL1(FILAORX-*)                                                   
AORRECQ  EQU   7                   RECORD IDENTIFIER                            
         DC    AL2(AORRECQ)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    CL(L'DR_NAMEF)'AOR'                                              
         DC    CL(L'DR_NAMES)'AOR'                                              
         DC    AL1(AORKAGMD-AORKEY)                                             
         DC    AL1(AORKAGMD-AORKEY)                                             
         DC    AL1(0)                                                           
         DC    AL1(DR_ITAGM)                                                    
         DC    AL4(0)                                                           
         DC    AL4(ARCKEY)                                                      
         DC    AL4(ARCFLD)                                                      
                                                                                
AORARG1  DC    AL1(AORARG2-*)                                                   
         DC    AL1(AORKTYP-AORKEY)                                              
         DC    AL1(DR_ATEQU)                                                    
AORKTYPQ EQU   X'0D45'                                                          
         DC    AL2(AORKTYPQ)                                                    
                                                                                
AORARG2  DS    0X                                                               
                                                                                
FILAORX  DS    0X                                                               
                                                                                
FILCGD   DS    0X                  ** CLIENT GROUP DEFINITION **                
         DC    AL1(FILCGDX-*)                                                   
CGDRECQ  EQU   6                   RECORD IDENTIFIER                            
         DC    AL2(CGDRECQ)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    CL(L'DR_NAMEF)'CGDEF'                                            
         DC    CL(L'DR_NAMES)'CGDEF'                                            
         DC    AL1(GRPKAGMD-GRPKEY)                                             
         DC    AL1(GRPKAGMD-GRPKEY)                                             
         DC    AL1(0)                                                           
         DC    AL1(DR_ITAGM)                                                    
         DC    AL4(0)                                                           
         DC    AL4(CGDKEY)                                                      
         DC    AL4(CGDFLD)                                                      
                                                                                
CGDARG1  DC    AL1(CGDARG2-*)                                                   
         DC    AL1(GRPKTYP-GRPKEY)                                              
         DC    AL1(DR_ATEQU)                                                    
CGDKTYPQ EQU   X'0D04'                                                          
         DC    AL2(CGDKTYPQ)                                                    
                                                                                
CGDARG2  DC    AL1(CGDARG3-*)                                                   
         DC    AL1(GRPKID-GRPKEY)                                               
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'GRPKID)                                                    
                                                                                
CGDARG3  DC    AL1(CGDARG4-*)                                                   
         DC    AL1(GRPKCODE-GRPKEY)                                             
         DC    AL1(DR_ATBZ)                                                     
         DC    AL1(L'GRPKCODE)                                                  
                                                                                
CGDARG4  DS    0X                                                               
                                                                                
FILCGDX  DS    0X                                                               
                                                                                
FILCGG   DS    0X                  ** CLIENT GROUP **                           
         DC    AL1(FILCGGX-*)                                                   
CGGRECQ  EQU   7                   RECORD IDENTIFIER                            
         DC    AL2(CGGRECQ)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    CL(L'DR_NAMEF)'CGROUP'                                           
         DC    CL(L'DR_NAMES)'CGROUP'                                           
         DC    AL1(GRPKAGMD-GRPKEY)                                             
         DC    AL1(GRPKAGMD-GRPKEY)                                             
         DC    AL1(0)                                                           
         DC    AL1(DR_ITAGM)                                                    
         DC    AL4(0)                                                           
         DC    AL4(CGGKEY)                                                      
         DC    AL4(CGGFLD)                                                      
                                                                                
CGGARG1  DC    AL1(CGGARG2-*)                                                   
         DC    AL1(GRPKTYP-GRPKEY)                                              
         DC    AL1(DR_ATEQU)                                                    
CGGKTYPQ EQU   X'0D04'                                                          
         DC    AL2(CGGKTYPQ)                                                    
                                                                                
CGGARG2  DC    AL1(CGGARG3-*)                                                   
         DC    AL1(GRPKID-GRPKEY)                                               
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'GRPKID)                                                    
                                                                                
CGGARG3  DC    AL1(CGGARG4-*)                                                   
         DC    AL1(GRPKCODE-GRPKEY)                                             
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'GRPKCODE)                                                  
                                                                                
CGGARG4  DS    0X                                                               
                                                                                
FILCGGX  DS    0X                                                               
                                                                                
FILPGD   DS    0X                  ** PRODUCT GROUP DEFINITION **               
         DC    AL1(FILPGDX-*)                                                   
PGDRECQ  EQU   8                   RECORD IDENTIFIER                            
         DC    AL2(PGDRECQ)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    CL(L'DR_NAMEF)'PGRDEF'                                           
         DC    CL(L'DR_NAMES)'PGRDEF'                                           
         DC    AL1(PRGKAGMD-PRGKEY)                                             
         DC    AL1(PRGKAGMD-PRGKEY)                                             
         DC    AL1(0)                                                           
         DC    AL1(DR_ITAGM)                                                    
         DC    AL4(0)                                                           
         DC    AL4(PGDKEY)                                                      
         DC    AL4(PGDFLD)                                                      
                                                                                
PGDARG1  DC    AL1(PGDARG2-*)                                                   
         DC    AL1(PRGKTYP-PRGKEY)                                              
         DC    AL1(DR_ATEQU)                                                    
PGDKTYPQ EQU   X'0D01'                                                          
         DC    AL2(PGDKTYPQ)                                                    
                                                                                
PGDARG2  DC    AL1(PGDARG3-*)                                                   
         DC    AL1(PRGKID-PRGKEY)                                               
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'PRGKID)                                                    
                                                                                
PGDARG3  DC    AL1(PGDARG4-*)                                                   
         DC    AL1(PRGKGRP-PRGKEY)                                              
         DC    AL1(DR_ATBZ)                                                     
         DC    AL1(L'PRGKGRP)                                                   
                                                                                
PGDARG4  DS    0X                                                               
                                                                                
FILPGDX  DS    0X                                                               
                                                                                
FILPGG   DS    0X                  ** PRODUCT GROUP **                          
         DC    AL1(FILPGGX-*)                                                   
PGGRECQ  EQU   9                   RECORD IDENTIFIER                            
         DC    AL2(PGGRECQ)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    CL(L'DR_NAMEF)'PGROUP'                                           
         DC    CL(L'DR_NAMES)'PGROUP'                                           
         DC    AL1(PRGKAGMD-PRGKEY)                                             
         DC    AL1(PRGKAGMD-PRGKEY)                                             
         DC    AL1(0)                                                           
         DC    AL1(DR_ITAGM)                                                    
         DC    AL4(0)                                                           
         DC    AL4(PGGKEY)                                                      
         DC    AL4(PGGFLD)                                                      
                                                                                
PGGARG1  DC    AL1(PGGARG2-*)                                                   
         DC    AL1(PRGKTYP-PRGKEY)                                              
         DC    AL1(DR_ATEQU)                                                    
PGGKTYPQ EQU   X'0D01'                                                          
         DC    AL2(PGGKTYPQ)                                                    
                                                                                
PGGARG2  DC    AL1(PGGARG3-*)                                                   
         DC    AL1(PRGKID-PRGKEY)                                               
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'PRGKID)                                                    
                                                                                
PGGARG3  DC    AL1(PGGARG4-*)                                                   
         DC    AL1(PRGKGRP-PRGKEY)                                              
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'PRGKGRP)                                                   
                                                                                
PGGARG4  DS    0X                                                               
                                                                                
FILPGGX  DS    0X                                                               
                                                                                
FILRECX  DC    AL1(DR_EOTQ)                                                     
         EJECT                                                                  
                                                                                
***********************************************************************         
* Station record definition table                                     *         
***********************************************************************         
                                                                                
XFILRECS DS    0X                  ** XSPTFIL RECORD DEFINITIONS **             
                                                                                
FILBPC   DS    0X                  ** BILL PCT RECORD DEFINITION **             
         DC    AL1(FILBPCX-*)                                                   
BPCRECQ  EQU   1                   RECORD IDENTIFIER                            
         DC    AL2(BPCRECQ)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    CL(L'DR_NAMEF)'BPCT'                                             
         DC    CL(L'DR_NAMES)'BPCT'                                             
         DC    AL1(BPCKAM-BPCKEY)                                               
         DC    AL1(BPCKAM-BPCKEY)                                               
         DC    AL1(0)                                                           
         DC    AL1(DR_ITAGM)                                                    
         DC    AL4(0)                                                           
         DC    AL4(BPRKEY)                                                      
         DC    AL4(BPRFLD)                                                      
                                                                                
BPCARG1  DC    AL1(BPCARG2-*)                                                   
         DC    AL1(BPCKTYP-BPCKEY)                                              
         DC    AL1(DR_ATEQU)                                                    
BPCKTYPQ EQU   X'0E0D'                                                          
         DC    AL2(BPCKTYPQ)                                                    
                                                                                
BPCARG2  DS    0X                                                               
                                                                                
FILBPCX  DS    0X                                                               
                                                                                
XFILRECX DC    AL1(DR_EOTQ)                                                     
         EJECT                                                                  
                                                                                
***********************************************************************         
* Station record definition table                                     *         
***********************************************************************         
                                                                                
STARECS  DS    0X                  ** STAFILE RECORD DEFINITIONS **             
                                                                                
STASTA   DS    0X                  ** MASTER RECORD DEFINITION **               
         DC    AL1(STASTAX-*)                                                   
STARECQ  EQU   1                   RECORD INDENTIFIER                           
         DC    AL2(STARECQ)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    CL(L'DR_NAMEF)'MASTER'                                           
         DC    CL(L'DR_NAMES)'Master'                                           
         DC    AL1(STAKAGY-STAKEY)                                              
         DC    AL1(STAKMED-STAKEY)                                              
         DC    AL1(0)                                                           
         DC    AL1(DR_ITALF+DR_ITMED)                                           
         DC    AL4(0)                                                           
         DC    AL4(STNKEY)                                                      
         DC    AL4(STNFLD)                                                      
                                                                                
STAARG1  DC    AL1(STAARG2-*)                                                   
         DC    AL1(STAKTYPE-STAKEY)                                             
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(STAKTYPQ)                                                    
                                                                                
STAARG2  DC    AL1(STAARG3-*)                                                   
         DC    AL1(STAKMED-STAKEY)                                              
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'STAKMED)                                                   
                                                                                
STAARG3  DC    AL1(STAARG4-*)                                                   
         DC    AL1(STAKCALL-STAKEY)                                             
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'STAKCALL)                                                  
                                                                                
STAARG4  DS    0X                                                               
                                                                                
STASTAX  DS    0X                                                               
                                                                                
STAADD   DS    0X                  ** ADDRESS RECORD DEFINITION **              
         DC    AL1(STAADDX-*)                                                   
ADDRECQ  EQU   2                   RECORD INDENTIFIER                           
         DC    AL2(ADDRECQ)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    CL(L'DR_NAMEF)'ADDRESS'                                          
         DC    CL(L'DR_NAMES)'Address'                                          
         DC    AL1(ADDKAGY-ADDRKEY)                                             
         DC    AL1(ADDKMED-ADDRKEY)                                             
         DC    AL1(0)                                                           
         DC    AL1(DR_ITALF+DR_ITMED)                                           
         DC    AL4(0)                                                           
         DC    AL4(ADDKEY)                                                      
         DC    AL4(ADDFLD)                                                      
                                                                                
ADDARG1  DC    AL1(ADDARG2-*)                                                   
         DC    AL1(ADDKTYPE-ADDRKEY)                                            
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(ADDKTYPQ)                                                    
                                                                                
ADDARG2  DC    AL1(ADDARG3-*)                                                   
         DC    AL1(ADDKMED-ADDRKEY)                                             
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'ADDKMED)                                                   
                                                                                
ADDARG3  DC    AL1(ADDARG4-*)                                                   
         DC    AL1(ADDKCALL-ADDRKEY)                                            
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'ADDKCALL)                                                  
                                                                                
ADDARG4  DS    0X                                                               
                                                                                
STAADDX  DS    0X                                                               
                                                                                
STAREP   DS    0X                  ** REP RECORD DEFINITION **                  
         DC    AL1(REPRECX-*)                                                   
REPRECQ  EQU   3                   RECORD INDENTIFIER                           
         DC    AL2(REPRECQ)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    CL(L'DR_NAMEF)'REP'                                              
         DC    CL(L'DR_NAMES)'Rep'                                              
         DC    AL1(REPKAGY-REPKEY)                                              
         DC    AL1(REPKMED-REPKEY)                                              
         DC    AL1(0)                                                           
         DC    AL1(DR_ITALF+DR_ITMED)                                           
         DC    AL4(0)                                                           
         DC    AL4(REPRKEY)                                                     
         DC    AL4(REPFLD)                                                      
                                                                                
REPARG1  DC    AL1(REPARG2-*)                                                   
         DC    AL1(REPKTYPE-REPKEY)                                             
         DC    AL1(DR_ATEQU)                                                    
REPKTYPQ EQU   C'R'                                                             
         DC    AL1(REPKTYPQ)                                                    
                                                                                
REPARG2  DC    AL1(REPARG3-*)                                                   
         DC    AL1(REPKMED-REPKEY)                                              
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'REPKMED)                                                   
                                                                                
REPARG3  DC    AL1(REPARG4-*)                                                   
         DC    AL1(REPKREP-REPKEY)                                              
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'REPKREP)                                                   
                                                                                
REPARG4  DS    0X                                                               
                                                                                
REPADDX  DS    0X                                                               
                                                                                
REPRECX  DC    AL1(DR_EOTQ)                                                     
         EJECT                                                                  
                                                                                
***********************************************************************         
* Record key and data definitions                                     *         
***********************************************************************         
                                                                                
CLTKEY   DS    0X                  ** CLIENT KEY DEFINITIONS **                 
                                                                                
         DC    AL1(DK_TMEBQ+DK_TSAVQ)                                           
         DC    AL1(CKEYAM-CLTHDR,L'CKEYAM),AL4(0)                               
         DC    AL2(RAGM-WORKD)                                                  
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ+DK_TSAVQ)                                           
         DC    AL1(CKEYCLT-CLTHDR,L'CKEYCLT),AL4(EDTCLT)                        
         DC    AL2(RCLT-WORKD)                                                  
         DC    XL8'00'                                                          
                                                                                
CLTKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
CLTFLD   DS    0X                  ** CLIENT FIELD DEFINITIONS **               
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Client name'                                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(CNAME-CLTHDR)                                                
         DC    AL1(L'CNAME)                                                     
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Media office'                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(COFFICE-CLTHDR)                                              
         DC    AL1(L'COFFICE)                                                   
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTOFF)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Bill % Split'                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(COPT4-CLTHDR)                                                
         DC    AL1(L'COPT4)                                                     
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTBPS)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Client group 1'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(CGRP1-CLTHDR)                                                
         DC    AL1(L'CGRP1)                                                     
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTGRPC)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Client group 2'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(CGRP2-CLTHDR)                                                
         DC    AL1(L'CGRP2)                                                     
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTGRPC)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Client group 3'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(CGRP3-CLTHDR)                                                
         DC    AL1(L'CGRP3)                                                     
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTGRPC)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Client group 4'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(CGRP4-CLTHDR)                                                
         DC    AL1(L'CGRP4)                                                     
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTGRPC)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Client group 5'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(CGRP5-CLTHDR)                                                
         DC    AL1(L'CGRP5)                                                     
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTGRPC)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Client group 6'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(CGRP6-CLTHDR)                                                
         DC    AL1(L'CGRP6)                                                     
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTGRPC)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Client group 7'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(CGRP7-CLTHDR)                                                
         DC    AL1(L'CGRP7)                                                     
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTGRPC)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Client group 8'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(CGRP8-CLTHDR)                                                
         DC    AL1(L'CGRP8)                                                     
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTGRPC)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Client group 9'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(CGRP9-CLTHDR)                                                
         DC    AL1(L'CGRP9)                                                     
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTGRPC)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Client group 10'                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(CGRP10-CLTHDR)                                               
         DC    AL1(L'CGRP10)                                                    
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTGRPC)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Frozen Options'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(COPT2-CLTHDR)                                                
         DC    AL1(L'COPT2)                                                     
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTCFRZ)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'SAP Code'                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(CSAPCODE-CLTHDR)                                             
         DC    AL1(L'CSAPCODE)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
CLTFLDX  DC    AL1(DD_EOTQ)                                                     
                                                                                
PRDKEY   DS    0X                  ** PRODUCT KEY DEFINITIONS **                
                                                                                
         DC    AL1(DK_TMEBQ+DK_TSAVQ)                                           
         DC    AL1(PKEYAM-PRDHDR,L'PKEYAM),AL4(0)                               
         DC    AL2(RAGM-WORKD)                                                  
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ+DK_TSAVQ)                                           
         DC    AL1(PKEYCLT-PRDHDR,L'PKEYCLT),AL4(EDTCLT)                        
         DC    AL2(RCLT-WORKD)                                                  
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(PKEYPRD-PRDHDR,L'PKEYPRD),AL4(0)                             
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
PRDKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
PRDFLD   DS    0X                  ** PRODUCT FIELD DEFINITIONS **              
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Product name'                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PNAME-PRDHDR)                                                
         DC    AL1(L'PNAME)                                                     
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Bill to name'                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PADDR1-PRDHDR)                                               
         DC    AL1(L'PADDR1)                                                    
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Address line 1'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PADDR2-PRDHDR)                                               
         DC    AL1(L'PADDR2)                                                    
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Address line 2'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PADDR3-PRDHDR)                                               
         DC    AL1(L'PADDR3)                                                    
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Address line 3'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PADDR4-PRDHDR)                                               
         DC    AL1(L'PADDR4)                                                    
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Bill Basis'                                        
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PBILLBAS-PRDHDR)                                             
         DC    AL1(L'PBILLBAS)                                                  
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTBBASE)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Commission%'                                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PBILLCOM-PRDHDR)                                             
         DC    AL1(L'PBILLCOM)                                                  
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTCOMP)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Commission Basis'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PBILLBAS-PRDHDR)                                             
         DC    AL1(L'PBILLBAS)                                                  
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTCBASE)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Effective Month'                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PBILLDT-PRDHDR)                                              
         DC    AL1(L'PBILLDT)                                                   
         DC    AL1(DD_TBMOQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Product group 1'                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PGRP1-PRDHDR)                                                
         DC    AL1(L'PGRP1)                                                     
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTGRPC)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Product group 2'                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PGRP2-PRDHDR)                                                
         DC    AL1(L'PGRP2)                                                     
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTGRPC)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Product group 3'                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PGRP3-PRDHDR)                                                
         DC    AL1(L'PGRP3)                                                     
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTGRPC)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Product group 4'                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PGRP4-PRDHDR)                                                
         DC    AL1(L'PGRP4)                                                     
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTGRPC)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Product group 5'                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PGRP5-PRDHDR)                                                
         DC    AL1(L'PGRP5)                                                     
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTGRPC)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Product group 6'                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PGRP6-PRDHDR)                                                
         DC    AL1(L'PGRP6)                                                     
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTGRPC)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Product group 7'                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PGRP7-PRDHDR)                                                
         DC    AL1(L'PGRP7)                                                     
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTGRPC)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Product group 8'                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PGRP8-PRDHDR)                                                
         DC    AL1(L'PGRP8)                                                     
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTGRPC)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Product group 9'                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PGRP9-PRDHDR)                                                
         DC    AL1(L'PGRP9)                                                     
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTGRPC)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Product group 10'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PGRP10-PRDHDR)                                               
         DC    AL1(L'PGRP10)                                                    
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTGRPC)                                                     
         DC    XL12'00'                                                         
                                                                                
PRDFLDX  DC    AL1(DD_EOTQ)                                                     
                                                                                
ESTKEY   DS    0X                  ** ESTIMATE KEY DEFINITIONS **               
                                                                                
         DC    AL1(DK_TMEBQ+DK_TSAVQ)                                           
         DC    AL1(EKEYAM-ESTHDR,L'EKEYAM),AL4(0)                               
         DC    AL2(RAGM-WORKD)                                                  
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ+DK_TSAVQ)                                           
         DC    AL1(EKEYCLT-ESTHDR,L'EKEYCLT),AL4(EDTCLT)                        
         DC    AL2(RCLT-WORKD)                                                  
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(EKEYPRD-ESTHDR,L'EKEYPRD),AL4(0)                             
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TBINQ)                                                    
         DC    AL1(EKEYEST-ESTHDR,L'EKEYEST),AL4(0)                             
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
ESTKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
ESTFLD   DS    0X                  ** ESTIMATE FIELD DEFINITIONS **             
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Estimate name'                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EDESC-ESTHDR)                                                
         DC    AL1(L'EDESC)                                                     
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Start date'                                        
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(ESTART-ESTHDR)                                               
         DC    AL1(L'ESTART)                                                    
         DC    AL1(DD_TEDTQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'End date'                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EEND-ESTHDR)                                                 
         DC    AL1(L'EEND)                                                      
         DC    AL1(DD_TEDTQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Bill Basis'                                        
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EBILLBAS-ESTHDR)                                             
         DC    AL1(L'EBILLBAS)                                                  
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTBBASE)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Commission%'                                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EBILLCOM-ESTHDR)                                             
         DC    AL1(L'EBILLCOM)                                                  
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTCOMP)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Commission Basis'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EBILLBAS-ESTHDR)                                             
         DC    AL1(L'EBILLBAS)                                                  
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTCBASE)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Filters'                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EPROF-ESTHDR)                                                
         DC    AL1(3)                                                           
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Copy Code'                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(ECOPY-ESTHDR)                                                
         DC    AL1(L'ECOPY)                                                     
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Special Rep'                                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EREP-ESTHDR)                                                 
         DC    AL1(L'EREP)                                                      
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTREP)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Request Range'                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EREQLO-ESTHDR)                                               
         DC    AL1(L'EREQLO)                                                    
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTREQR)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Daypart Menu'                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EDAYMENU-ESTHDR)                                             
         DC    AL1(L'EDAYMENU)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Master/Slave'                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EMSTRIND-ESTHDR)                                             
         DC    AL1(L'EMSTRIND)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'OOW Start Day'                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EOWSDAY-ESTHDR)                                              
         DC    AL1(L'EOWSDAY)                                                   
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTOWS)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Master Est.'                                       
         DC    AL1(0)                                                           
         DC    AL1(DD_IZEBQ)                                                    
         DC    AL1(0)                                                           
         DC    AL2(EMSTREST-ESTHDR)                                             
         DC    AL1(L'EMSTREST)                                                  
         DC    AL1(DD_TBINQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Status'                                            
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(DD_IFMFQ)                                                    
         DC    AL2(ECNTRL-ESTHDR)                                               
         DC    AL1(L'ECNTRL)                                                    
         DC    AL1(DD_TMSKQ)                                                    
         DC    AL4(ESTCTRLM)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)' '                                                 
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(DD_ILMFQ)                                                    
         DC    AL2(ELOCKYM-ESTHDR)                                              
         DC    AL1(L'ELOCKYM)                                                   
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTLOCK)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Options'                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(DD_IFMFQ)                                                    
         DC    AL2(EFLAG1-ESTHDR)                                               
         DC    AL1(L'EFLAG1)                                                    
         DC    AL1(DD_TMSKQ)                                                    
         DC    AL4(ESTFLG1M)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'DAILY='                                            
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EDAILY-ESTHDR)                                               
         DC    AL1(L'EDAILY)                                                    
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    AL1(COMMA)                                                       
         DC    AL1(DD_LPFXN),AL2(0)                                             
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'SLN='                                              
         DC    AL1(0)                                                           
         DC    AL1(DD_IZEBQ)                                                    
         DC    AL1(0)                                                           
         DC    AL2(ESLN-ESTHDR)                                                 
         DC    AL1(L'ESLN)                                                      
         DC    AL1(DD_TBINQ)                                                    
         DC    AL4(0)                                                           
         DC    AL1(COMMA)                                                       
         DC    AL1(DD_LPFXN),AL2(0)                                             
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'COS2='                                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(ECOST2-ESTHDR)                                               
         DC    AL1(L'ECOST2)                                                    
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTCOST2)                                                    
         DC    AL1(COMMA)                                                       
         DC    AL1(DD_LPFXN),AL2(0)                                             
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'PW='                                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EPWPCT-ESTHDR)                                               
         DC    AL1(L'EPWPCT)                                                    
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPWP)                                                      
         DC    AL1(COMMA)                                                       
         DC    AL1(DD_LPFXN),AL2(0)                                             
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'CASH='                                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(ECASHPRD-ESTHDR)                                             
         DC    AL1(L'ECASHPRD)                                                  
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPRD)                                                      
         DC    AL1(COMMA)                                                       
         DC    AL1(DD_LPFXN),AL2(0)                                             
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'TRD='                                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(DD_ILMFQ)                                                    
         DC    AL2(ETRDPRD-ESTHDR)                                              
         DC    AL1(L'ETRDPRD)                                                   
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPRD)                                                      
         DC    AL1(COMMA)                                                       
         DC    AL1(DD_LPFXN),AL2(0)                                             
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Control'                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(ECONTROL-ESTHDR)                                             
         DC    AL1(L'ECONTROL)                                                  
         DC    AL1(DD_TMSKQ)                                                    
         DC    AL4(ESTCONTM)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Rate type'                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(DD_IFMFQ)                                                    
         DC    AL2(ERATE-ESTHDR)                                                
         DC    AL1(L'ERATE)                                                     
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)' '                                                 
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(DD_ILMFQ)                                                    
         DC    AL2(ERATECST-ESTHDR)                                             
         DC    AL1(L'ERATECST)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
ESTFLDX  DC    AL1(DD_EOTQ)                                                     
                                                                                
ESTCTRLM DS    0XL(DM_LNQ)         ** ECNTRL KEYWORDS **                        
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)'LOCK'                                   
         DC    CL(L'DM_Z)' ',CL(L'DM_O)'HOLD'                                   
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
                                                                                
ESTCONTM DS    0XL(DM_LNQ)         ** ECONTROL KEYWORDS **                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)'E'                                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)'NSC'                                    
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
                                                                                
ESTFLG1M DS    0XL(DM_LNQ)         ** EFLAG1 KEYWORDS **                        
         DC    CL(L'DM_Z)' ',CL(L'DM_O)'REQ=Y'                                  
         DC    CL(L'DM_Z)' ',CL(L'DM_O)'NMG=Y'                                  
         DC    CL(L'DM_Z)' ',CL(L'DM_O)'DARE=TRADE'                             
         DC    CL(L'DM_Z)' ',CL(L'DM_O)'DEMOS=N'                                
         DC    CL(L'DM_Z)' ',CL(L'DM_O)'OWPW'                                   
         DC    CL(L'DM_Z)' ',CL(L'DM_O)'WIM=Y'                                  
         DC    CL(L'DM_Z)' ',CL(L'DM_O)'SD'                                     
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
                                                                                
CNDKEY   DS    0X                  ** NETDEF KEY DEFINITIONS **                 
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(NDEFKNET-NDEFRECD,L'NDEFKNET),AL4(0)                         
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ+DK_TSAVQ)                                           
         DC    AL1(NDEFKCLT-NDEFRECD,L'NDEFKCLT),AL4(EDTCLT)                    
         DC    AL2(RCLT-WORKD)                                                  
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TBINQ)                                                    
         DC    AL1(NDEFKEST-NDEFRECD,L'NDEFKEST),AL4(0)                         
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
CNDKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
CNDFLD   DS    0X                  ** NETDEF FIELD DEFINITIONS **               
                                                                                
         DC    AL1(DD_LDIMQ)                                                    
         DC    CL(L'DD_NAME)'Station'                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(NDEFELQ)                                                     
         DC    AL1(NDEFSTA-NDEFEL01,L'NDEFSTA)                                  
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(CNDEL01K)                                                    
         DC    AL4(CNDEL01D)                                                    
         DC    XL4'00'                                                          
                                                                                
CNDFLDX  DC    AL1(DD_EOTQ)                                                     
                                                                                
CNDEL01K L     RF,DI_AREL          ** NETDEF 01 ELEMENT KEY **                  
         MVC   DI_EKEY(L'NDEFSTA),NDEFSTA-NDEFEL01(RF)                          
         BR    RE                                                               
                                                                                
CNDEL01D DS    0X                  ** NETDEF 01 ELEMENT DATA **                 
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Network%'                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(NDEFPCT-NDEFEL01)                                            
         DC    AL1(L'NDEFPCT)                                                   
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTNWPCT)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Offset'                                            
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(NDEFOSET-NDEFEL01)                                           
         DC    AL1(L'NDEFOSET)                                                  
         DC    AL1(DD_TBISQ)                                                    
         DC    AL1(0)                                                           
         DC    XL15'00'                                                         
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Region'                                            
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(NDEFRGN-NDEFEL01)                                            
         DC    AL1(L'NDEFRGN)                                                   
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_EOTQ)                                                     
                                                                                
CCDKEY   DS    0X                  ** CBLDEF KEY DEFINITIONS **                 
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(NDEFKNET-NDEFRECD,L'NDEFKNET),AL4(0)                         
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ+DK_TSAVQ)                                           
         DC    AL1(NDEFKCLT-NDEFRECD,L'NDEFKCLT),AL4(EDTCLT)                    
         DC    AL2(RCLT-WORKD)                                                  
         DC    XL8'00'                                                          
                                                                                
CCDKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
CCDFLD   DS    0X                  ** CBLDEF FIELD DEFINITIONS **               
                                                                                
         DC    AL1(DD_LDIMQ)                                                    
         DC    CL(L'DD_NAME)'Station'                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(NDEFELQ)                                                     
         DC    AL1(NDEFSTA-NDEFEL01,L'NDEFSTA)                                  
         DC    AL1(0)                                                           
         DC    AL4(CCDEL01F)                                                    
         DC    AL4(CCDEL01K)                                                    
         DC    AL4(CCDEL01D)                                                    
         DC    XL4'00'                                                          
                                                                                
CCDFLDX  DC    AL1(DD_EOTQ)                                                     
                                                                                
CCDEL01F L     RF,DI_AREL          ** CBLDEF 01 ELEMENT FILTER **               
         CLC   =C'ZZ',NDEFMSUF-NDEFEL01(RF)                                     
         J     SETCCC                                                           
                                                                                
CCDEL01K L     RF,DI_AREL          ** CBLDEF 01 ELEMENT KEY **                  
         MVC   DI_EKEY(L'NDEFMSUF),NDEFMSUF-NDEFEL01(RF)                        
         MVI   DI_EKEY+L'NDEFMSUF,C'/'                                          
         SR    R0,R0                                                            
         ICM   R0,3,NDEFMNUM-NDEFEL01(RF)                                       
         EDITR (R0),(4,DI_EKEY+L'NDEFMSUF+1),ALIGN=LEFT                         
         BR    RE                                                               
                                                                                
CCDEL01D DS    0X                  ** CBLDEF 01 ELEMENT DATA **                 
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Network%'                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(NDEFPCT-NDEFEL01)                                            
         DC    AL1(L'NDEFPCT)                                                   
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTNWPCT)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Offset'                                            
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(NDEFOSET-NDEFEL01)                                           
         DC    AL1(L'NDEFOSET)                                                  
         DC    AL1(DD_TBISQ)                                                    
         DC    AL1(0)                                                           
         DC    XL15'00'                                                         
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Alpha Market'                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(NDEFAMKT-NDEFEL01)                                           
         DC    AL1(L'NDEFAMKT)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_EOTQ)                                                     
                                                                                
BFMKEY   DS    0X                  ** BFORM KEY DEFINITIONS **                  
                                                                                
         DC    AL1(DK_TMEBQ+DK_TSAVQ)                                           
         DC    AL1(BFKAGYMD-BFKEY,L'BFKAGYMD),AL4(0)                            
         DC    AL2(RAGM-WORKD)                                                  
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ+DK_TSAVQ)                                           
         DC    AL1(BFKCLT-BFKEY,L'BFKCLT),AL4(EDTCLT)                           
         DC    AL2(RCLT-WORKD)                                                  
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(BFKPRD-BFKEY,L'BFKPRD),AL4(EDTPRDB)                          
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(BFKEST-BFKEY,L'BFKEST),AL4(EDTEST)                           
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(BFKMKT-BFKEY,L'BFKMKT),AL4(EDTMKT)                           
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
BFMKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
BFMFLD   DS    0X                  ** BFORM FIELD DEFINITIONS **                
                                                                                
         DC    AL1(DD_LDIMQ)                                                    
         DC    CL(L'DD_NAME)''                                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(BFRCDELQ)                                                    
         DC    AL1(BFRCDDTE-BFRCDELD)                                           
*        DC    AL1(L'BFRCDDTE)                                                  
         DC    AL1(BFRCDLNQ-2)                                                  
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(BFMEL10K)                                                    
         DC    AL4(BFMEL10D)                                                    
         DC    XL4'00'                                                          
                                                                                
BFMFLDX  DC    AL1(DD_EOTQ)                                                     
                                                                                
BFMEL10K L     RF,DI_AREL          ** BFORM 10 ELEMENT KEY **                   
         BR    RE                                                               
         MVC   WORK(L'BFRCDDTE),BFRCDDTE-BFRCDELD(RF)                           
         XC    WORK(2),=2X'FF'     IT WAS COMPLEMENTED                          
         MVI   WORK+2,X'01'                                                     
         LR    R0,RE                                                            
         GOTOR CDATCON,DMCB,(3,WORK),(9,DI_EKEY)                                
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
BFMEL10D DS    0X                  ** BFORM 10 ELEMENT DATA **                  
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Effective MOS'                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BFRCDDTE-BFRCDELD)                                           
         DC    AL1(L'BFRCDDTE)                                                  
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTEMOS)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Bill Basis'                                        
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BFRCDFML-BFRCDELD)                                           
         DC    AL1(1)                                                           
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTBBASB)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Bill Commission%'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BFRCDFML+1-BFRCDELD)                                         
         DC    AL1(4)                                                           
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTCOMP)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Commission Basis'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BFRCDFML-BFRCDELD)                                           
         DC    AL1(1)                                                           
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTCBASB)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_EOTQ)                                                     
                                                                                
ARCKEY   DS    0X                  ** AOR KEY DEFINITIONS **                    
                                                                                
         DC    AL1(DK_TMEBQ+DK_TSAVQ)                                           
         DC    AL1(AORKAGMD-AORKEY,L'AORKAGMD),AL4(0)                           
         DC    AL2(RAGM-WORKD)                                                  
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ+DK_TSAVQ)                                           
         DC    AL1(AORKCLT-AORKEY,L'AORKCLT),AL4(EDTCLT)                        
         DC    AL2(RCLT-WORKD)                                                  
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(AORKPRD-AORKEY,L'AORKPRD),AL4(0)                             
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(AORKEST-AORKEY,L'AORKEST),AL4(EDTEST)                        
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(AORKDPT-AORKEY,L'AORKDPT),AL4(EDTCALL)                       
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(AORKSTYP-AORKEY,L'AORKSTYP),AL4(EDTCALL)                     
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
ARCKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
ARCFLD   DS    0X                  ** AOR FIELD DEFINITIONS **                  
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Agency Name'                                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AORLIN1-AORKEY)                                              
         DC    AL1(L'AORLIN1)                                                   
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Address Line 2'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AORLIN2-AORKEY)                                              
         DC    AL1(L'AORLIN2)                                                   
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Address Line 3'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AORLIN3-AORKEY)                                              
         DC    AL1(L'AORLIN3)                                                   
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Address Line 4'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AORLIN4-AORKEY)                                              
         DC    AL1(L'AORLIN4)                                                   
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'AOR Commission%'                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AORPCT-AORKEY)                                               
         DC    AL1(L'AORPCT)                                                    
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTCOMP)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Effective Date'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AOREFF-AORKEY)                                               
         DC    AL1(L'AOREFF)                                                    
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTEFF)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Basis'                                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AORBAS-AORKEY)                                               
         DC    AL1(L'AORBAS)                                                    
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTBAS)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Killdate'                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AORKILL-AORKEY)                                              
         DC    AL1(L'AORKILL)                                                   
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTEFF)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'GST Code'                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AORGSTCD-AORKEY)                                             
         DC    AL1(L'AORGSTCD)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Receivable Acct Code'                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AORRCVBL-AORKEY)                                             
         DC    AL1(L'AORRCVBL)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Commission Acct Code'                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AORCOMM-AORKEY)                                              
         DC    AL1(L'AORCOMM)                                                   
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'PST Codes'                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AORPST-AORKEY)                                               
         DC    AL1(L'AORPST)                                                    
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPST)                                                      
         DC    XL12'00'                                                         
                                                                                
ARCFLDX  DC    AL1(DD_EOTQ)                                                     
                                                                                
BPRKEY   DS    0X                  ** BPCT KEY DEFINITIONS **                   
                                                                                
         DC    AL1(DK_TMEBQ+DK_TSAVQ)                                           
         DC    AL1(BPCKAM-BPCKEY,L'BPCKAM),AL4(0)                               
         DC    AL2(RAGM-WORKD)                                                  
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ+DK_TSAVQ)                                           
         DC    AL1(BPCKCLT-BPCKEY,L'BPCKCLT),AL4(EDTCLT)                        
         DC    AL2(RCLT-WORKD)                                                  
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(BPCKPRD-BPCKEY,L'BPCKPRD),AL4(0)                             
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TBINQ)                                                    
         DC    AL1(BPCKEST-BPCKEY,L'BPCKEST),AL4(0)                             
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
BPRKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
BPRFLD   DS    0X                  ** BPCT FIELD DEFINITIONS **                 
                                                                                
         DC    AL1(DD_LDIMQ)                                                    
         DC    CL(L'DD_NAME)'Effective MOS'                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PCTELQ)                                                      
         DC    AL1(PCTMONTH-PCTELEM,L'PCTMONTH)                                 
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(BPREL10K)                                                    
         DC    AL4(BPREL10D)                                                    
         DC    XL4'00'                                                          
                                                                                
BPRFLDX  DC    AL1(DD_EOTQ)                                                     
                                                                                
BPREL10K L     RF,DI_AREL          ** BPCT 10 ELEMENT KEY **                    
         MVC   WORK(L'PCTMONTH),PCTMONTH-PCTELEM(RF)                            
         LR    R0,RE                                                            
         GOTOR CDATCON,DMCB,(3,WORK),(9,DI_EKEY)                                
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
BPREL10D DS    0X                  ** BPCT 10 ELEMENT DATA **                   
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Current PCT'                                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCTPCT-PCTELEM)                                              
         DC    AL1(1)                                                           
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPCT)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'PID at Add'                                        
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCTAPID-PCTELEM)                                             
         DC    AL1(4)                                                           
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPID)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'PCT Add Date'                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCTADATE-PCTELEM)                                            
         DC    AL1(1)                                                           
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTEFF2)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'PCT at Change'                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCTCPPCT-PCTELEM)                                            
         DC    AL1(1)                                                           
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPCT)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'PID at Change'                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCTCPID-PCTELEM)                                             
         DC    AL1(4)                                                           
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPID)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'PCT Change Date'                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCTCDATE-PCTELEM)                                            
         DC    AL1(1)                                                           
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTEFF2)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'PCT at Older Change'                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCT2PPCT-PCTELEM)                                            
         DC    AL1(1)                                                           
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPCT)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'PID at Older Change'                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCT2PID-PCTELEM)                                             
         DC    AL1(4)                                                           
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPID)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'PCT Older Change Date'                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCT2DATE-PCTELEM)                                            
         DC    AL1(1)                                                           
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTEFF2)                                                     
         DC    XL12'00'                                                         
                                                                                
BPREL10X DC    AL1(DD_EOTQ)                                                     
                                                                                
CGDKEY   DS    0X                  ** CLIENT GROUP KEY DEFINTIONS **            
                                                                                
         DC    AL1(DK_TMEBQ+DK_TSAVQ)                                           
         DC    AL1(GRPKAGMD-GRPKEY,L'GRPKAGMD),AL4(0)                           
         DC    AL2(RAGM-WORKD)                                                  
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(GRPKID-GRPKEY,L'GRPKID),AL4(EDTGRID)                         
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
CGDKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
CGDFLD   DS    0X                  ** CLIENT GROUP FIELD DEFINTIONS **          
*                                                                               
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Break 1 Title'                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(GRPBRKCQ)                                                    
         DC    AL1(GRPBK1-GRPBRKCD)                                             
         DC    AL1(L'GRPBK1)                                                    
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Break 1 Title Length'                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(GRPBRKCQ)                                                    
         DC    AL1(GRPBK1LN-GRPBRKCD)                                           
         DC    AL1(L'GRPBK1LN)                                                  
         DC    AL1(DD_TBINQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Break 2 Title'                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(GRPBRKCQ)                                                    
         DC    AL1(GRPBK2-GRPBRKCD)                                             
         DC    AL1(L'GRPBK2)                                                    
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Break 2 Title Length'                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(GRPBRKCQ)                                                    
         DC    AL1(GRPBK2LN-GRPBRKCD)                                           
         DC    AL1(L'GRPBK2LN)                                                  
         DC    AL1(DD_TBINQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Sec Manager ID 1'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(GRPSCMCQ)                                                    
         DC    AL1(GRPSCM1-GRPSCMD)                                             
         DC    AL1(L'GRPSCM1)                                                   
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPID)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Sec Manager ID 2'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(GRPSCMCQ)                                                    
         DC    AL1(GRPSCM2-GRPSCMD)                                             
         DC    AL1(L'GRPSCM2)                                                   
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPID)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Sec Manager ID 3'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(GRPSCMCQ)                                                    
         DC    AL1(GRPSCM3-GRPSCMD)                                             
         DC    AL1(L'GRPSCM3)                                                   
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPID)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Sec Manager ID 4'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(GRPSCMCQ)                                                    
         DC    AL1(GRPSCM4-GRPSCMD)                                             
         DC    AL1(L'GRPSCM4)                                                   
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPID)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Sec Manager ID 5'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(GRPSCMCQ)                                                    
         DC    AL1(GRPSCM5-GRPSCMD)                                             
         DC    AL1(L'GRPSCM5)                                                   
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPID)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Sec Manager ID 6'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(GRPSCMCQ)                                                    
         DC    AL1(GRPSCM6-GRPSCMD)                                             
         DC    AL1(L'GRPSCM6)                                                   
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPID)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_EOTQ)                                                     
                                                                                
CGGKEY   DS    0X                  ** CLIENT GROUP KEY **                       
                                                                                
         DC    AL1(DK_TMEBQ+DK_TSAVQ)                                           
         DC    AL1(GRPKAGMD-GRPKEY,L'GRPKAGMD),AL4(0)                           
         DC    AL2(RAGM-WORKD)                                                  
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(GRPKID-GRPKEY,L'GRPKID),AL4(EDTGRID)                         
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(GRPKCODE-GRPKEY,L'GRPKCODE),AL4(EDTGRP)                      
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
CGGKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
CGGFLD   DS    0X                  ** CLIENT GROUP FIELD DEFINTIONS **          
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Break 1 name'                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CLGMEMQ)                                                     
         DC    AL1(CLGNAM1-CLGMEMD)                                             
         DC    AL1(L'CLGNAM1)                                                   
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Break 2 name'                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CLGMEMQ)                                                     
         DC    AL1(CLGNAM2-CLGMEMD)                                             
         DC    AL1(L'CLGNAM2)                                                   
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_EOTQ)                                                     
                                                                                
PGDKEY   DS    0X                  ** PRODUCT GROUP KEY DEFINTIONS **           
                                                                                
         DC    AL1(DK_TMEBQ+DK_TSAVQ)                                           
         DC    AL1(PRGKAGMD-PRGKEY,L'PRGKAGMD),AL4(0)                           
         DC    AL2(RAGM-WORKD)                                                  
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(PRGKCLT-PRGKEY,L'PRGKCLT),AL4(EDTCLT)                        
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(PRGKID-PRGKEY,L'PRGKID),AL4(EDTGRID)                         
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
PGDKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
PGDFLD   DS    0X                  ** PRODUCT GROUP FIELD DEFINTIONS **         
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Break 1 Title'                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(X'01')                                                       
         DC    AL1(PRGBK1-PRGEL01)                                              
         DC    AL1(L'PRGBK1)                                                    
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Break 1 Title Length'                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(X'01')                                                       
         DC    AL1(PRGBK1LN-PRGEL01)                                            
         DC    AL1(L'PRGBK1LN)                                                  
         DC    AL1(DD_TBINQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Break 2 Title'                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(X'01')                                                       
         DC    AL1(PRGBK2-PRGEL01)                                              
         DC    AL1(L'PRGBK2)                                                    
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Break 2 Title Length'                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(X'01')                                                       
         DC    AL1(PRGBK2LN-PRGEL01)                                            
         DC    AL1(L'PRGBK2LN)                                                  
         DC    AL1(DD_TBINQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Sec Manager ID 1'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PRGSCMCQ)                                                    
         DC    AL1(PRGSCM1-PRGSCMD)                                             
         DC    AL1(L'PRGSCM1)                                                   
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPID)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Sec Manager ID 2'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PRGSCMCQ)                                                    
         DC    AL1(PRGSCM2-PRGSCMD)                                             
         DC    AL1(L'PRGSCM2)                                                   
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPID)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Sec Manager ID 3'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PRGSCMCQ)                                                    
         DC    AL1(PRGSCM3-PRGSCMD)                                             
         DC    AL1(L'PRGSCM3)                                                   
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPID)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Sec Manager ID 4'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PRGSCMCQ)                                                    
         DC    AL1(PRGSCM4-PRGSCMD)                                             
         DC    AL1(L'PRGSCM4)                                                   
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPID)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Sec Manager ID 5'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PRGSCMCQ)                                                    
         DC    AL1(PRGSCM5-PRGSCMD)                                             
         DC    AL1(L'PRGSCM5)                                                   
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPID)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Sec Manager ID 6'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PRGSCMCQ)                                                    
         DC    AL1(PRGSCM6-PRGSCMD)                                             
         DC    AL1(L'PRGSCM6)                                                   
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPID)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_EOTQ)                                                     
                                                                                
PGGKEY   DS    0X                  ** PRODUCT GROUP KEY **                      
                                                                                
         DC    AL1(DK_TMEBQ+DK_TSAVQ)                                           
         DC    AL1(PRGKAGMD-PRGKEY,L'PRGKAGMD),AL4(0)                           
         DC    AL2(RAGM-WORKD)                                                  
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(PRGKCLT-PRGKEY,L'PRGKCLT),AL4(EDTCLT)                        
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(PRGKID-PRGKEY,L'PRGKID),AL4(EDTGRID)                         
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(PRGKGRP-PRGKEY,L'PRGKGRP),AL4(EDTGRPP)                       
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
PGGKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
PGGFLD   DS    0X                  ** PRODUCT GROUP FIELD DEFINTIONS **         
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Break 1 name'                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(X'10')                                                       
         DC    AL1(PRGNAM1-PRGEL10)                                             
         DC    AL1(L'PRGNAM1)                                                   
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Break 2 name'                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(X'10')                                                       
         DC    AL1(PRGNAM2-PRGEL10)                                             
         DC    AL1(L'PRGNAM2)                                                   
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Address Line 1'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(X'20')                                                       
         DC    AL1(PRGADDR1-PRGEL20)                                            
         DC    AL1(L'PRGADDR1)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Address Line 2'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(X'20')                                                       
         DC    AL1(PRGADDR2-PRGEL20)                                            
         DC    AL1(L'PRGADDR1)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Address Line 3'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(X'20')                                                       
         DC    AL1(PRGADDR3-PRGEL20)                                            
         DC    AL1(L'PRGADDR1)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Address Line 4'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(X'20')                                                       
         DC    AL1(PRGADDR4-PRGEL20)                                            
         DC    AL1(L'PRGADDR1)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_EOTQ)                                                     
                                                                                
STNKEY   DS    0X                  ** STATION KEY DEFINITIONS **                
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(STAKMED-STAKEY,L'STAKMED),AL4(0)                             
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(STAKCALL-STAKEY,L'STAKCALL),AL4(0)                           
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(STAKAGY-STAKEY,L'STAKAGY),AL4(0)                             
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(STAKCLT-STAKEY,L'STAKCLT),AL4(EDTCLTM)                       
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
STNKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
STNFLD   DS    0X                  ** STATION FIELD DEFINITIONS **              
*&&DO                                                                           
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Market number'                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(SMKT-STAKEY)                                                 
         DC    AL1(L'SMKT)                                                      
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
*&&                                                                             
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Station Lock'                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(SFLAG1-STAKEY)                                               
         DC    AL1(L'SFLAG1)                                                    
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTSLCK)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Market Code'                                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(SMKT-STAKEY)                                                 
         DC    AL1(L'SMKT)                                                      
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Paying Rep'                                        
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(SPAYREP-STAKEY)                                              
         DC    AL1(L'SPAYREP)                                                   
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_EOTQ)                                                     
                                                                                
ADDKEY   DS    0X                  ** ADDRESS KEY DEFINITIONS **                
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(ADDKMED-ADDRKEY,L'ADDKMED),AL4(0)                            
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(ADDKCALL-ADDRKEY,L'ADDKCALL),AL4(0)                          
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(ADDKAGY-ADDRKEY,L'ADDKAGY),AL4(0)                            
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
ADDKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
ADDFLD   DS    0X                  ** ADDRESS FIELD DEFINITIONS **              
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Station name'                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(ANAME-ADDRKEY)                                               
         DC    AL1(L'ANAME)                                                     
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Street Address'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(A1LINE-ADDRKEY)                                              
         DC    AL1(L'A1LINE)                                                    
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'City'                                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(A2LINE-ADDRKEY)                                              
         DC    AL1(L'A2LINE)                                                    
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'State'                                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(A3LINE-ADDRKEY)                                              
         DC    AL1(L'A3LINE)                                                    
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Zip Code'                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(ABIGZIP-ADDRKEY)                                             
         DC    AL1(L'ABIGZIP)                                                   
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Country'                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AZIP-ADDRKEY)                                                
         DC    AL1(L'AZIP)                                                      
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTCTRY)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Last Changed Date'                                 
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(ADDRCHDT-ADDRKEY)                                            
         DC    AL1(8)                                                           
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTEFF2)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Changed By'                                        
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(ADDRCHBY-ADDRKEY)                                            
         DC    AL1(4)                                                           
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPID)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Comments'                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(APHONE-ADDRKEY)                                              
         DC    AL1(L'ABIGZIP)                                                   
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'E-mail'                                            
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(ADREMAIL-ADDRKEY)                                            
         DC    AL1(L'ADREMAIL)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'SAP Interface'                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(ADRSAP-ADDRKEY)                                              
         DC    AL1(L'ADRSAP)                                                    
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_EOTQ)                                                     
                                                                                
                                                                                
REPRKEY  DS    0X                  ** REP KEY DEFINITIONS **                    
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(REPKMED-REPKEY,L'REPKMED),AL4(0)                             
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(REPKREP-REPKEY,L'REPKREP),AL4(0)                             
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(REPKAGY-REPKEY,L'REPKAGY),AL4(0)                             
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
REPKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
REPFLD   DS    0X                  ** REP FIELD DEFINITIONS **                  
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Rep name'                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RNAME-REPKEY)                                                
         DC    AL1(L'RNAME)                                                     
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Street Address'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(R1LINE-REPKEY)                                               
         DC    AL1(L'R1LINE)                                                    
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'City'                                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(R2LINE-REPKEY)                                               
         DC    AL1(L'R2LINE)                                                    
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'State'                                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(R3LINE-REPKEY)                                               
         DC    AL1(L'R3LINE)                                                    
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Zip Code'                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RBIGZIP-REPKEY)                                              
         DC    AL1(L'RBIGZIP)                                                   
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Country'                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RZIP-REPKEY)                                                 
         DC    AL1(L'RZIP)                                                      
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTCTRY)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Unwired Network?'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RUNWNET-REPKEY)                                              
         DC    AL1(L'RUNWNET)                                                   
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Options'                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RPOPT1-REPKEY)                                               
         DC    AL1(L'RPOPT1)                                                    
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTROPT)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_EOTQ)                                                     
                                                                                
         EJECT                                                                  
CTRYUSAQ EQU   1                   USA AGENCY                                   
CTRYCANQ EQU   2                   CANADIAN AGENCY                              
EOR      EQU   0                                                                
FF       EQU   X'FF'                                                            
COMMA    EQU   C','                                                             
                                                                                
LITERALS DS    0D                  ** GLOBAL LITERALS **                        
         LTORG                                                                  
                                                                                
ABUFF1   DC    A(BUFF1)            BUFFERIN CONTROL BLOCK 1                     
                                                                                
DMREAD   DC    C'DMREAD '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
GETREC   DC    C'GETREC '                                                       
                                                                                
CORPHS   DS    0AL1                ** CORE RESIDENT PHASE LIST **               
         DC    AL1(QCLPACK)                                                     
         DC    AL1(QCLUNPK)                                                     
         DC    AL1(QRCPACK)                                                     
         DC    AL1(QOFFICER)                                                    
CORPHSN  EQU   (*-CORPHS)/L'CORPHS                                              
                                                                                
DAYTAB   DS    0CL3                ** DAY TABLE **                              
         DC    C'MonTueWedThuFriSatSun'                                         
                                                                                
BBASTAB  DS    0XL6                ** BILL BASIS TABLE **                       
         DC    X'50',C'CNET '                                                   
         DC    X'40',C'CGROS'                                                   
         DC    X'10',C'NET  '                                                   
         DC    X'00',C'GROSS'                                                   
BBASTABN EQU   (*-BBASTAB)/L'BBASTAB                                            
                                                                                
CBASTAB  DS    0XL6                ** COMMISSION BASIS TABLE **                 
         DC    X'01',C'NET  '                                                   
         DC    X'00',C'GROSS'                                                   
CBASTABN EQU   (*-CBASTAB)/L'CBASTAB                                            
         EJECT                                                                  
***********************************************************************         
* System/File list                                                    *         
***********************************************************************         
                                                                                
SFLIST   DS    0C                                                               
                                                                                
         DC    C'SPOT   '                                                       
                                                                                
         DC    C'N'                                                             
SPTDIR   DC    C'SPTDIR '                                                       
         DC    C'N'                                                             
SPTFIL   DC    C'SPTFILE'                                                       
         DC    C'N'                                                             
XSPDIR   DC    C'XSPDIR '                                                       
         DC    C'N'                                                             
XSPFIL   DC    C'XSPFILE'                                                       
         DC    C'N'                                                             
         DC    C'STAFILE'                                                       
         DC    C'N'                                                             
         DC    C'STRFFL '                                                       
         DC    C'N'                                                             
         DC    C'STRFDR '                                                       
         DC    C'X'                                                             
         EJECT                                                                  
***********************************************************************         
* Program table                                                       *         
***********************************************************************         
                                                                                
PRGTAB   DS    0XL(DP_LNQ)                                                      
         DC    X'01',CL(L'DP_NAME)'PFM'                                         
         DC    X'17',CL(L'DP_NAME)'SFM'                                         
         DC    X'19',CL(L'DP_NAME)'FILE'                                        
         DC    X'31',CL(L'DP_NAME)'SUPERDSK'                                    
PRGTABX  DC    AL1(DP_EOTQ)                                                     
                                                                                
       ++INCLUDE SPCGRTAB                                                       
                                                                                
BUFF1    BUFFD TYPE=B,COLUMNS=0,KEYLEN=BUFFKEYL,BUFFERS=255                     
         EJECT                                                                  
WORKD    DSECT                     ** LOCAL WORKING STORAGE **                  
BYTE     DS    X                                                                
FULL     DS    F                                                                
DUB      DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    XL256                                                            
PSTOUT   DS    CL64                                                             
BLOCK    DS    480C                                                             
                                                                                
APHASES  DS    0A                  ** CORE RESIDENT PHASE ADDRESSES **          
VCLPACK  DS    A                                                                
VCLUNPK  DS    A                                                                
VRCPACK  DS    A                                                                
VOFFICER DS    A                                                                
                                                                                
RAGM     DS    XL(L'CKEYAM)        EXTRACT AGENCY/MEDIA VALUE                   
RCLT     DS    XL(L'CKEYCLT)       EXTRACTED CLIENT CODE                        
RCLGID   DS    XL2                 EXTRACTED CLIENT GROUP ID                    
                                                                                
KEY      DS    XL64                GENERAL KEY BUILDING AREA                    
                                                                                
OFCBLK   DS    XL(OFCLENQ)                                                      
                                                                                
BUFFREC  DS    0X                  ** BUFFERIN RECORD **                        
BUFFKEY  DS    0X                                                               
BUFFRAGY DS    CL(L'DS_ALF)        AGENCY ALPHA ID                              
BUFFRKEY DS    XL(L'KEY)           RECORD KEY                                   
BUFFKEYL EQU   *-BUFFREC                                                        
BUFFFLAG DS    X                   ** FLAG BYTE **                              
BUFFFRNF EQU   X'80'               RECORD NOT FOUND                             
BUFFRECD DS    0XL1500             RECORD FOLLOWS (IF FOUND)                    
BUFFRECL EQU   *-BUFFREC                                                        
                                                                                
         ORG   BUFFRECD                                                         
IO       DS    XL4096                                                           
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
* INCLUDED DSECTS                                                               
       ++INCLUDE DDBUFFD                                                        
       ++INCLUDE GEDFARD                                                        
       ++INCLUDE DDCOMFACSD                                                     
       ++INCLUDE DDCOREQUS                                                      
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTRECD  DSECT ,                                                                
       ++INCLUDE SPGENCLT                                                       
         ORG   CKEYCLT+L'CKEYCLT                                                
CKEYREST DS    XL(L'CKEY-(*-CKEY))                                              
PRDRECD  DSECT ,                                                                
       ++INCLUDE SPGENPRD                                                       
         ORG   PKEYPRD+L'PKEYPRD                                                
PKEYREST DS    XL(L'PKEY-(*-PKEY))                                              
ESTRECD  DSECT ,                                                                
       ++INCLUDE SPGENEST                                                       
         ORG   EKEYEST+L'EKEYEST                                                
EKEYREST DS    XL(L'EKEY-(*-EKEY))                                              
       ++INCLUDE SPGENBFML                                                      
       ++INCLUDE SPGENAOR                                                       
STARECD  DSECT ,                                                                
       ++INCLUDE SPGENSTA                                                       
ADDRECD  DSECT ,                                                                
       ++INCLUDE SPGENADD                                                       
       ++INCLUDE SPGENNDEF                                                      
       ++INCLUDE SPGENBPCT                                                      
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDPSTBLK                                                       
       ++INCLUDE SPGENCLG                                                       
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE SPGENGRP                                                       
       ++INCLUDE CTGENFILE                                                      
REPKEYD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026SPDFAR    05/13/20'                                      
         END                                                                    
