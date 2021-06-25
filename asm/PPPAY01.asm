*          DATA SET PPPAY01    AT LEVEL 056 AS OF 10/14/20                      
*PHASE T40301A                                                                  
*INCLUDE SRCHCALL                                                               
*INCLUDE PPGETADR                                                               
*                                                                               
*        TITLE 'T40301 - PAY PROGRAM HEADLINE EDIT'                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* BOBY 03/12/10 ADD GST OVERRIDE                                                
*                                                                               
* KWAN 09/18/07 CHECK FOR AB PROFILE - ACCESS PAY VIA ADBUYER                   
*                                                                               
* SMYE  12/04   ADD TO "CONTROL DATE CALCULATION FROM INVOICE DATE"             
*               FOR POSSIBLE DATA FROM PUB                                      
*                                                                               
* SMYE  08/03   LINK TO ADBUYER                                                 
*                                                                               
* SMYE  06/03   NEW OPTION FOR MULTIPLE REQHDRS (SINGLY OPTION) AND             
*               A STRUCTURAL REORGANIZATION                                     
*                                                                               
* SMYE 04/11/03 ADDED CODE AT OPT16E TO ALLOW FOR ENTRY OF A SEQUENCE           
*               NUMBER GREATER THAN 9 (UP TO 255)                               
*                                                                               
* KWAN 03/25/03 USE COMFACS' GETPROF INSTEAD OF INCLUDING IT                    
*                                                                               
* SMYE  07/02   TINY CHANGE FOR NEW PROFILE OFFICES AT CLT44A                   
*                                                                               
* SMYE  05/02   NEW LIMIT ACCESS SECURITY                                       
*                                                                               
* SMYE 4/19/02  PUBEDIT NOW CORE-RESIDENT                                       
*                                                                               
* SMYE 1/24/00  Y2K FIX FOR CONTROL DATE AND INVOICE DATE IN BOTH               
*               CKD2B AND INVD2B (SEE *Y2K*)                                    
*                                                                               
* SMYE 9/13/99  CHANGE *INCLUDE PPGETADK ABOVE "BACK" TO PPGETADR               
*                  AND "REVERSE" 7/26/99 CHANGE BELOW                           
* SMYE 7/26/99  CHANGE ++INCLUDE PPGETADRD TO ++INCLUDE PPGETADRDK              
*                                                                               
* BPLA   6/99   (R1) CHANGED TO DMCB FOR VDATCON CALL                           
*               AT DTE10                                                        
*                                                                               
* BPLA 8/98     CHANGE FOR Y2K ON A0 PROFILE                                    
*               ALSO CHANGE TO ACCEPT JAN/00 FOR JAN 2000                       
*               ALSO CATCH END DATE BEFORE START AS ERROR                       
*                                                                               
* SMYE 4/97     USE CALL TO PPGETADR TO GET ADDRESSES (IN PUB4..)               
*                                                                               
* BOBY 2/28/94  ADD PST HANDLING                                                
*                                                                               
* BPLA 3/2/93   DON'T SET CONTROL DATE TO TODAY FOR CR/CK SWITCH                
*                                                                               
* BPLA 2/25/93  ADD CHECKS TO HELP CATCH LEAP YEAR PROBELMS                     
*               WITH CONTROL DATE AND INVOICE DATE                              
* BPLA 1/6/93 IF AO PROFILE HAS SPECIAL OFFICE -REQUIRE EFFECTIVE DATE          
*             BEFORE ALLOWING PAYMENTS                                          
*                                                                               
* BPLA 8/26/91 ADD PUB NAME SEARCHING                                           
*                                                                               
* ROSA 1/11/91 ADD OPTION TO INCLUDE GST IN DETAILS WHEN XTEST      L02         
* ROSA 2/7/90  ADD CLIENT SECURITY CHECK                            L01         
*                                                                               
* BPLA 6/25/90 FIX ADDRESS/REP OFFICE LOOK-UP   LEVEL 23-34                     
*              SVPYOFF ADDED - USED FOR ADDRESS/REP LOOK-UP                     
*                                                                               
         TITLE 'T40301 - PAY PROGRAM HEADLINE EDIT'                             
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
T40301   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40301                                                         
         L     RC,0(R1)                                                         
         LA    R8,4095(RB)                                                      
         LA    R8,1(R8)                                                         
         USING T40301+4096,R8                                                   
*                                                                               
         USING GENOLD,RC                                                        
         LA    R7,4095(RC)                                                      
         LA    R7,1(R7)                                                         
         USING GENOLD+4096,R7                                                   
         L     RA,4(R1)                                                         
         USING T403FFD,RA                                                       
*                                                                               
         RELOC RELO01                                                           
*                                                                               
*        ERROR EQUATES                                                          
*                                                                               
MISSERR  EQU   1                                                                
INVERR   EQU   2                                                                
MEDERR   EQU   13                                                               
CLERR    EQU   14                                                               
PRERR    EQU   15                                                               
DTERR    EQU   20                                                               
CHKERR   EQU   190                                                              
CHKERR2  EQU   191                                                              
PBERR    EQU   18                                                               
PUBNFERR EQU   44                                                               
EEERR    EQU   122                                                              
LINERR   EQU   83                                                               
ESTERR   EQU   16                                                               
NOT1MON  EQU   69                  MUST BE IN ONE MONTH                         
BADOFCDT EQU   203       INVALID SPECIAL OFFICE START DATE                      
*                        (SAME AS MIXERR IN PPPAY02)                            
INVACN   EQU   204       INVALID ACN NUMBER                                     
BADINTR  EQU   205       MISSING OR INVALID CLT INTERFACE NO.                   
BADXFROP EQU   209       TRANSFER OPTIONS INCOMPATIBLE                          
ACCERR   EQU   207       ACCESS TO THIS CLIENT NOT AUTH           L01           
BADPSTPD EQU   231                 PAID INSERTIONS IN PERIOD                    
BADPSTPR EQU   232                 BAD PST PROVINCIAL CODE                      
BADPSTVL EQU   233                 BAD PST VALUE                                
MULTERR1 EQU   262       ONLY 1 INVOICE AND AMOUNT ALLOWED (MLTREQSW)           
MULTERR2 EQU   263                 SINGLY, IF USED, MUST BE ONLY OPTION         
*                                                                               
CCUSAID  EQU   797                                                              
         TITLE 'T40301 - PAY PROGRAM HEADLINE EDIT - MEDVAL'                    
***********************************************************************         
*                                                                     *         
*        MEDIA VALIDATION                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MEDVAL   DS    0H                                                               
*                                                                               
         LA    R2,PAYMDH                                                        
         LA    R3,MEDERR                                                        
         STH   R3,ADBERNUM           FOR USE IF EDIT ERROR                      
         LHI   RF,D#MEDCOD           OCCURS WHILE DOING AN                      
         STH   RF,ADBERFLD           ADBUYER CALL                               
         TM    4(R2),X'20'                                                      
         BO    MEDVALX                                                          
         BAS   RE,CLRMD                                                         
         BAS   RE,ANY                                                           
*READ AGY MEDIA REC                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),PAYMD                                                   
         MVI   KEY+3,1                                                          
         BAS   RE,HIGH                                                          
         CLC   KEY(4),KEYSAVE                                                   
         BNE   ERROR                                                            
         BAS   RE,GETREC                                                        
         L     R9,AREC                                                          
         USING PAGYREC,R9                                                       
         MVC   SVAGPROF,PAGYPROF                                                
         FOUT  PAYMDNMH,PAGYMED                                                 
         OI    4(R2),X'20'                                                      
*                                                                               
         XC    AGYFXREP,AGYFXREP   INIT FOREIGN EXCHANGE REP                    
*                                                                               
         CLI   PAGYNAT,C'C'        SKIP IF NOT CANADIAN AGENCY                  
         BNE   CANX                                                             
*                                                                               
         MVI   ELCODE,X'10'        SET FOR FX REP ELEMENT                       
         LA    R5,PAGYREC+33       POINT TO FIRST ELM IN RECORD                 
         USING PAGFXEL,R5          ESTABLISH FX REP ELEMENT                     
         BRAS  RE,NEXTEL           SEARCH FOR FX REP ELEMENT                    
         BNE   *+10                   NONE FOUND                                
         MVC   AGYFXREP,PAGFXREP   SAVE FX REP CODE                             
*                                                                               
         DROP  R5                                                               
*                                                                               
CANX     DS    0H                                                               
*                                                                               
         OC    4(2,RA),4(RA)       TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    6(2,RA),6(RA)       TEST ANY LIMIT ACCESS                        
         BZ    MEDVALX             NO LIMIT ACCESS                              
*                                                                               
         MVI   TRAGYSW,0           CLEAR TRAFFIC AGENCY INDICATOR               
         BRAS  RE,CKTRAFID         TRAFFIC ID ?                                 
         BNE   *+8                                                              
         MVI   TRAGYSW,C'Y'        SET TRAFFIC AGENCY INDICATOR                 
*                                                                               
MEDVALX  DS    0H                                                               
*                                                                               
         TITLE 'T40301 - PAY PROGRAM HEADLINE EDIT - OPTVAL'                    
***********************************************************************         
*                                                                     *         
*        OPTIONS VALIDATION                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VALOPT   DS    0H                                                               
*                                                                               
         LA    R2,PAYOPH           POINT TO OPTIONS FIELD                       
*                                                                               
         BRAS  RE,OPTVAL           VALIDATE OPTION FIELD                        
         BNE   EXIT                ERRORS                                       
*                                                                               
VALOPTX  DS    0H                                                               
*                                                                               
         EJECT                                                                  
*EDIT CLIENT                                                                    
CLT      LA    R2,PAYCLH                                                        
         LA    R3,CLERR                                                         
         STH   R3,ADBERNUM           FOR USE IF EDIT ERROR                      
         LHI   RF,D#CLTCOD           OCCURS WHILE DOING AN                      
         STH   RF,ADBERFLD           ADBUYER CALL                               
         TM    4(R2),X'20'                                                      
*NOP*    BO    PRD                                                              
         BO    CLTX                                                             
         BAS   RE,CLRCL                                                         
         BAS   RE,ANY                                                           
         CLI   5(R2),2                                                          
         BL    ERROR                                                            
         CLI   5(R2),3                                                          
         BH    ERROR                                                            
         BE    CLT2                                                             
         OC    PAYCL,BLANKS                                                     
         FOUT  (R2)                                                             
*READ CLT REC                                                                   
CLT2     XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),PAYMD                                                   
         MVI   KEY+3,2                                                          
         MVC   KEY+4(3),PAYCL                                                   
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
         L     R9,AREC                                                          
         USING PCLTREC,R9                                                       
*                                                                               
         OC    4(2,RA),4(RA)       TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    6(2,RA),6(RA)       TEST ANY LIMIT ACCESS                        
         BZ    CLT10               NO LIMIT ACCESS                              
*                                                                               
         LAY   R4,PUBIO                                                         
         LR    R0,R4            USE PUBIO FOR 1024 BYTE "SECRET BLOCK"          
         LHI   R1,1024                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
*******  CALL TO FASECRET TO BUILD A SECURITY AUTHORIZATION TABLE               
*                                                                               
*  INITIALIZE SECURITY BLOCK       R4 POINTING TO PUBIO                         
         L     RF,VCOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',(R4)),0                                    
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
         MVC   BYTE3,PCLTOFF     SAVE PCLTOFF FOR LIMIT ACCESS TESTING          
*                                                                               
         CLI   TRAGYSW,C'Y'       TRAFFIC ID ?                                  
         BNE   VCLT20             NO                                            
*                                 SEE IF TRAFFIC OFFICE EXISTS                  
         LA    R5,PCLTREC+33      POINT TO CLIENT REC ELEMS                     
         MVI   ELCODE,X'50'       CLIENT TRAFFIC OFFICE ELEM CODE               
         BAS   RE,NEXTEL                                                        
         BNE   VCLT20             NO TRAFFIC OFFICE FOUND                       
         MVC   BYTE3,2(R5)        REPLACE PCLTOFF SAVED IN BYTE                 
*                                 WITH CLIENT TRAFFIC OFFICE CODE               
*                                                                               
VCLT20   DS    0H               *****  LIMIT ACCESS TESTING   *****             
         XC    WORK,WORK          WORK MUST BE AT LEAST 48 BYTES                
         LA    R1,WORK            (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAUTH,6(RA)                                                    
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,BYTE3       CLT OR CLT TRAFFIC OFFICE CODE                
         MVC   OFCCLT,PCLTKCLT                                                  
         OC    OFCCLT,=3C' '                                                    
         MVC   OFCPMED,PCLTKMED                                                 
         MVC   OFCLMT(4),6(RA)                                                  
         ST    R4,OFCSECD         A("SECRET BLOCK" - USING PUBIO)               
         DROP  R1                                                               
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),255                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORK),VCOMFACS    ACCESS AWARE CALL              
         CLI   0(R1),0                                                          
         BE    CLT10                                                            
*                                                                               
         LA    R3,207                                                           
         STH   R3,ADBERNUM           FOR USE IN ADBUYER CALL                    
         B     ERROR                                                            
*                                                                               
**************                                                                  
CLT10    DS    0H                                                               
*                                                                               
         LH    RF,=Y(QCLT-GENOLD)                                               
         LA    RF,GENOLD(RF)                                                    
         MVC   0(L'QCLT,RF),PCLTKCLT                                            
         MVC   SVCLPROF,PCLTPROF                                                
**NEW 12/7/89                                                                   
         MVC   SVCLOFF(2),=C'  '    SET TO SPACES                               
         MVI   SVPYOFF,C' '         FOR ADDRESS/REP LOOK-UP                     
*        WAS   MVC  SVCLOFF,PCLTOFF                                             
         MVC   SVCLOFF(1),PCLTOFF                                               
         MVC   SVPYOFF(1),PCLTOFF   FOR ADDRESS/REP LOOK-UP                     
         OC    PCLTAOFC,PCLTAOFC    CHECK FOR ACC OFFICE CODE                   
         BZ    *+10                                                             
         MVC   SVCLOFF(2),PCLTAOFC   USE ACC OFFICE                             
         OC    SVCLOFF(2),=C'  '     JUST IN CASE                               
**NEW 12/7/89                                                                   
*                                  TRY FOR FX  PROFILE                          
         XC    PROGPRO2,PROGPRO2        CLEAR EXTENSION PROFILE                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'P0FX'                                                 
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),PAYMD                                                  
         MVC   WORK+7(3),PCLTKCLT                                               
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'90',WORK),PROGPRO2,VDATAMGR                         
*                                                                               
         NI    FXSW,X'FF'-X'80'    TURN OFF SWITCH                              
*                                                                               
         CLI   PROGPRO2,C'Y'       IF USING FX FEATURE                          
         BNE   *+8                                                              
         OI    FXSW,X'80'             SET INDICATOR                             
*                                                                               
*                                  TRY FOR FX PROFILE                           
         XC    PROGPRO2,PROGPRO2        CLEAR EXTENSION PROFILE                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'PA0A'                                                 
         NI    WORK,X'BF'               MAKE SYSTEM LOWER CASE                  
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),PAYMD                                                  
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'C0',WORK),PROGPRO2,VDATAMGR                         
*                                                                               
         MVC   WORK(4),=C'P0AB'                                                 
         GOTO1 (RF),DMCB,(X'C0',WORK),WORK+20,VDATAMGR                          
         CLI   WORK+20+8,C'Y'      PAY ACCESS IS ONLY ALLOWED VIA AB?           
         BNE   CLT10M                                                           
         CLI   ADBSW,C'Y'          ADBUYER CALL?                                
         BE    CLT10M                                                           
         L     RF,VCOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         OI    6(R2),X'40'         INSERT CURSOR                                
         LA    R3,289              ACCESS DENIED ERROR MSG                      
         MVI   ERRAREA,X'FF'                                                    
         GOTO1 (RF),DMCB+12,(R3),0,(C'E',DMCB),0,0,0                            
         B     EXXMOD                                                           
*                                                                               
CLT10M   MVC   ADBPF11,PROGPRO2+11   SAVE PART PMT PROFILE FOR ADBUYER          
         MVC   ADBPF12,PROGPRO2+12   SAVE SINGLY PROFILE FOR ADBUYER            
*                                    FOR POSSIBLE USE IN PAY02 SAVE             
         MVC   SVDYNCD,PROGPRO2+01    DAYS TO ADD TO INV DTE (NO CD)            
         MVC   SVDYCD,PROGPRO2+02     DAYS TO ADD TO INV DTE (CD)               
*                                                                               
         XC    WORK,WORK           TRY FOR A0 PROFILE                           
         MVC   WORK(4),=C'P0A0'                                                 
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),PAYMD                                                  
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'C0',WORK),PROGPROF,VDATAMGR                         
*                                                                               
         MVC   GNOPT,SVAGPROF      SET OPTIONS FROM MEDIA PROFILES              
         XC    SPOFF(5),SPOFF                                                   
*                                                                               
         OC    PROGPROF,PROGPROF                                                
         BZ    CLT44A                                                           
*                                  OR FROM A0 PROFILE                           
         CLI   ICLOPT,0            SEE IF OPTION OVERRIDE                       
         BNE   *+10                                                             
         MVC   ICLOPT,PROGPROF+2                                                
         MVC   GNOPT,PROGPROF                                                   
*                                                                               
* CONVERT PROFILE DATES TO Y2K FORMAT                                           
         OC    PROGPROF+5(2),PROGPROF+5 SPECIAL OFFICE START                    
         BZ    CLT11X                                                           
         MVC   DUB(2),PROGPROF+5                                                
         MVI   DUB+2,1                                                          
         GOTO1 VDATCON,DMCB,(3,DUB),WORK    MAKE EBCDIC                         
         GOTO1 (RF),(R1),WORK,(3,DUB)       MAKE BINARY                         
         MVC   PROGPROF+5(2),DUB                                                
*                                                                               
*                                                                               
CLT11X   DS    0H                                                               
**NEW 12/7/89    WAS (4)  CHANGED FOR SPOFF2                                    
         MVC   SPOFF(5),PROGPROF+4                                              
         CLI   SPOFF2,C'*'                                                      
         BH    *+8                                                              
         MVI   SPOFF2,C' '         SET TO SPACE FOR *                           
*                                                                               
CLT44A   DS    0H                                                               
         CLI   ICLOPT,0            IF STILL 0 SET TO YES                        
         BNE   *+8                                                              
         MVI   ICLOPT,C'Y'         SET DEFAULT                                  
*NOP*    CLI   SPOFF,C'*'                                                       
*NOP*    BH    *+10                                                             
         CLI   SPOFF,C'('                                                       
         BL    CLT44C             CLEAR                                         
         CLI   SPOFF,C'*'                                                       
         BNE   *+10                                                             
**NEW 12/7/89    WAS (4)  CHANGED FOR SPOFF2                                    
CLT44C   XC    SPOFF(5),SPOFF                                                   
         OC    SPOFFDTE,SPOFFDTE                                                
         BNZ   CLT47                                                            
*                                                                               
         LA    R3,BADOFCDT                                                      
         STH   R3,ADBERNUM           FOR USE IN ADBUYER CALL                    
         CLI   SPOFF,0                SEE IF I HAVE SPECIAL OFFICE              
         BE    CLT47                  NO                                        
         B     ERROR                  YES - THE REQUIRE EFFECTIVE DATE          
*                                                                               
*******  MVC   SPOFFDTE,=2X'FF'                                                 
*                                                                               
CLT47    FOUT  PAYCLNMH,PCLTNAME                                                
*                                                                               
*          DATA SET SPPAY01    AT LEVEL 053 AS OF 04/09/85                      
*                                                                               
CLT12    CLI   PROGPROF+10,C'C'       TEST CCUSA INTERFACE                      
         BNE   CLT40                                                            
         SPACE 1                                                                
* READ CONTROL FILE TO LOCATE CCUSA ACCTG FILE *                                
         SPACE 1                                                                
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'                                                        
         MVC   WORK+15(10),=CL10'CCUSA'                                         
         LAY   RF,PUBIO                                                         
         LR    R0,RF               SAVE ADDRESS                                 
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,(RF)                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RF,R0               RESTORE ADDRESS                              
         CLC   WORK(25),0(RF)                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,28(RF)                                                        
CLT14    CLI   0(R1),X'21'         TEST SYSTEM ELEMENT                          
         BNE   *+12                                                             
         CLI   2(R1),X'06'         TEST ACCOUNTING                              
         BE    CLT16                                                            
*                                                                               
         ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BNE   CLT14                                                            
         DC    H'0'                                                             
*                                                                               
CLT16    MVC   SVXFRACC,3(R1)      SAVE ACC SYSTEM NUMBER                       
         MVC   SVXFRCOM,4(R1)      AND COMPANY CODE                             
*                                                                               
         CLC   T403FFD+10(2),=AL2(CCUSAID)  TEST ID = CCUSA                     
         BNE   CLT18                                                            
         LA    R3,BADINTR                                                       
         STH   R3,ADBERNUM         FOR USE IN ADBUYER CALL                      
         CLI   PCLTNUM,C' '                                                     
         BNH   ERROR               MISSING INTERFACE CODE                       
         MVC   SVXFRAGN,PCLTNUM                                                 
         B     CLT40                                                            
         SPACE 1                                                                
* READ ID RECORD FROM CONTROL FILE *                                            
         SPACE 1                                                                
CLT18    XC    WORK,WORK                                                        
         MVI   WORK,C'I'                                                        
         MVC   WORK+23(2),T403FFD+10                                            
         LAY   RF,PUBIO                                                         
         LR    R0,RF               SAVE ADDRESS                                 
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,(RF)                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RF,R0               RESTORE ADDRESS                              
         CLC   WORK(25),0(RF)                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,28(RF)                                                        
CLT20    CLI   0(R1),X'02'         TEST DESC ELEMENT                            
         BE    CLT22                                                            
         ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BNE   CLT20                                                            
         DC    H'0'                                                             
*                                                                               
CLT22    LA    R3,BADINTR                                                       
         STH   R3,ADBERNUM         FOR USE IN ADBUYER CALL                      
         CLC   =C'CC',2(R1)        TEST ID LOOKS LIKE COKE                      
         BNE   CLT24                                                            
         MVC   SVXFRAGN,4(R1)      SAVE AGENCY NUMBER                           
         B     CLT40                                                            
*                                                                               
CLT24    CLC   =C'RM',2(R1)                                                     
         BNE   CLT25                                                            
         LA    RE,4(R1)            BE SURE 3 DIGITS FOLLOW                      
         LA    RF,3                                                             
CLT24A   CLI   0(RE),C'0'                                                       
         BL    CLT28ERR                                                         
         CLI   0(RE),C'9'                                                       
         BH    CLT28ERR                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,CLT24A                                                        
         MVC   SVXFRAGN,4(R1)      SAVE AGENCY NUMBER                           
         B     CLT40                                                            
*                                                                               
*                                                                               
CLT25    ZIC   RE,1(R1)            GET ELEMENT LENGTH                           
         LA    RE,0(RE,R1)         POINT TO NEXT ELEM                           
         BCTR  RE,0                BACK UP TO END OF THIS ELEM                  
         CLI   0(RE),C' '          TEST BLANK                                   
         BNH   *-6                                                              
         BCTR  RE,0                BACK UP 1 MORE TO POINT TO OFFICE            
*                                                                               
         LA    R4,CCAGYLST                                                      
*                                                                               
CLT26    CLC   0(2,R4),AGYALPHA                                                 
         BNE   CLT28                                                            
         CLC   2(2,R4),0(RE)       MATCH OFFICE                                 
         BNE   CLT28                                                            
         MVC   SVXFRAGN,4(R4)                                                   
         B     CLT40                                                            
*                                                                               
CLT28    LA    R4,7(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   CLT26                                                            
CLT28ERR LA    R3,BADINTR                                                       
         STH   R3,ADBERNUM           FOR USE IN ADBUYER CALL                    
         B     ERROR                                                            
*                                                                               
CLT40    DS    0H                                                               
         OI    4(R2),X'20'         SET VALIDATED                                
*                                                                               
CLTX     DS    0H                                                               
         CLI   PROGPRO2+13,C'Y'    DISALLOW CONTROL DATE OVERRIDE ?             
         BNE   CLTXL               NO                                           
         CLI   PROGPRO2+00,C'I'    CALCULATE CONTROL DATE ?                     
         BE    CLTXC               YES                                          
         CLI   PROGPRO2+00,C'P'    CALCULATE CONTROL DATE ?                     
         BNE   CLTXL               NO                                           
*                                                                               
CLTXC    DS    0H                                                               
         XC    CKDATE,CKDATE       CLEAR - WILL BE CALCULATED LATER             
         XC    PAYCK,PAYCK         CLEAR                                        
         MVI   PAYCKH+5,0                                                       
         NI    PAYCKH+4,X'FF'-X'20'   SET NOT VALIDATED                         
         OI    PAYCKH+6,X'80'         XMIT                                      
*                                                                               
CLTXL    DS    0H                                                               
         CLI   PROGPRO2+12,C'Y'    "SINGLY" REQUESTED BY PROFILE ?              
         BE    CLTXN               YES                                          
         CLI   PROGPRO2+12,C'N'    OVERRIDE "NO SINGLY" ALLOWED ?               
         BE    CLTXP               YES                                          
         LHI   RF,D#SINGLY           FOR USE IF DOING AN                        
         STH   RF,ADBERFLD           ADBUYER CALL                               
         CLI   MLTOPTSW,C'S'       OPTION "SINGLY" ENTERED ?                    
         BE    CLTXERR             YES - ERROR                                  
         CLI   MLTOPTSW,C'X'       OPTION "XSINGLY" ENTERED ?                   
         BE    CLTXERR             YES - ERROR                                  
         B     PRD                                                              
CLTXN    DS    0H                                                               
         MVI   MLTREQSW,C'Y'       PAY INDIVIDUALLY (SINGLY)                    
         CLI   MLTOPTSW,C'X'       OPTION "XSINGLY" ENTERED ?                   
         BNE   *+8                 NO                                           
         MVI   MLTREQSW,0          DON'T PAY INDIVIDUALLY (SINGLY)              
         B     PRD                                                              
CLTXP    DS    0H                                                               
         MVI   MLTREQSW,0          DON'T PAY INDIVIDUALLY (SINGLY)              
         CLI   MLTOPTSW,C'S'       OPTION "SINGLY" ENTERED ?                    
         BNE   *+8                 NO                                           
         MVI   MLTREQSW,C'Y'       PAY INDIVIDUALLY (SINGLY)                    
         B     PRD                                                              
CLTXERR  DS    0H                                                               
         LA    R2,PAYOPH                                                        
         J     OPTERR              OPTIONS ERROR                                
*                                                                               
*                                                                               
CCAGYLST DS    0CL7                AGYALPHA(2)/OFFICE(2)/CCAGY(3)               
         DC    CL7'APNY510'                                                     
         DC    CL7'BJMN725'                                                     
         DC    CL7'LIPA560'                                                     
         DC    CL7'MCNY981'       WAS 910 3/8/88                                
         DC    CL7'MCCH981'       ADDED 3/8/88                                  
         DC    CL7'MCAT915'                                                     
         DC    CL7'MCNO931'                                                     
         DC    CL7'MCHO933'                                                     
         DC    CL7'MCDA935'                                                     
         DC    CL7'MCBO955'                                                     
         DC    CL7'MCDE970'                                                     
         DC    CL7'MCCN977'                                                     
         DC    CL7'MCLO979'                                                     
         DC    CL7'MCLA981'                                                     
         DC    CL7'MCSE981'        WAS 982                                      
         DC    CL7'MCSF981'        WAS 983                                      
         DC    CL7'MCSD984'                                                     
         DC    CL7'PMWA515'                                                     
         DC    CL7'RRSS770'                                                     
         DC    CL7'SJJR123'        **** FOR TESTING ONLY                        
         DC    X'FF'               E-O-L FLAG                                   
         EJECT                                                                  
*EDIT PRD                                                                       
PRD      OC    PAYRQ,BLANKS                                                     
         LA    R2,PAYPRH                                                        
         LA    R3,PRERR                                                         
         STH   R3,ADBERNUM           FOR USE IF EDIT ERROR                      
         LHI   RF,D#PRDCOD           OCCURS WHILE DOING AN                      
         STH   RF,ADBERFLD           ADBUYER CALL                               
         TM    4(R2),X'20'                                                      
         BO    PUB                                                              
         BAS   RE,CLRPR                                                         
         BAS   RE,ANY                                                           
         CLI   PAYPR,C'*'          OTHER AGY PRD-NOT PAYABLE                    
         BE    ERROR                                                            
         CLI   5(R2),2                                                          
         BL    ERROR                                                            
*                                                                               
         CLI   PROGPROF+10,C'C'    SEE IF CCUSA INTERFACE ACT                   
         BNE   PRD1                                                             
         CLC   PAYPR(3),=C'ZZZ'    NO ZZZ FOR CCUSA                             
         BE    ERROR                                                            
*                                                                               
PRD1     XC    BEST,BEST                                                        
         MVC   SAVPR,=C'   '                                                    
         MVC   SAVPR(2),PAYPR                                                   
         LA    R4,PAYPR+3                                                       
         CLI   PAYPR+2,C'A'                                                     
         BL    *+14                                                             
         MVC   SAVPR+2(1),PAYPR+2                                               
         LA    R4,PAYPR+4                                                       
         LH    RF,=Y(QPRD-GENOLD)                                               
         LA    RF,GENOLD(RF)                                                    
         MVC   0(L'QPRD,RF),SAVPR                                               
         CLI   5(R2),3                                                          
         BNH   PRD2                                                             
         LA    R3,ESTERR                                                        
         STH   R3,ADBERNUM           FOR USE IF EDIT ERROR                      
         LHI   RF,D#ESTNUM           OCCURS WHILE DOING AN                      
         STH   RF,ADBERFLD           ADBUYER CALL                               
         BAS   RE,FINDNUM                                                       
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BNP   ERROR                                                            
         CLC   =C'ALL',SAVPR                                                    
         BE    ERROR                                                            
         STH   R0,BEST                                                          
PRD2     DS    0H                                                               
*                                  RJ MUST PAY BY EST                           
         CLC   AGYALPHA,=C'RJ'                                                  
         BNE   PRD3                                                             
         OC    BEST,BEST                                                        
         BNZ   PRD3                                                             
         LA    R3,ESTERR                                                        
         STH   R3,ADBERNUM           FOR USE IF EDIT ERROR                      
         LHI   RF,D#ESTNUM           OCCURS WHILE DOING AN                      
         STH   RF,ADBERFLD           ADBUYER CALL                               
         B     ERROR                                                            
*READ PRD REC                                                                   
PRD3     CLC   =C'ALL',PAYPR                                                    
         BE    PRDX                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),PAYMD                                                   
         MVI   KEY+3,6                                                          
         MVC   KEY+4(3),PAYCL                                                   
         MVC   KEY+7(3),SAVPR                                                   
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
         L     R9,AREC                                                          
         USING PPRDREC,R9                                                       
         FOUT  PAYPRNMH,PPRDNAME                                                
         OC    BEST,BEST                                                        
         BZ    PRDX                                                             
         MVI   KEY+3,7                                                          
         MVC   KEY+10(2),BEST                                                   
         BAS   RE,HIGH                                                          
         LA    R3,ESTERR                                                        
         STH   R3,ADBERNUM           FOR USE IF EDIT ERROR                      
         LHI   RF,D#ESTNUM           OCCURS WHILE DOING AN                      
         STH   RF,ADBERFLD           ADBUYER CALL                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         BAS   RE,GETREC                                                        
         L     R9,AREC                                                          
         USING PESTREC,R9                                                       
         TM    PESTTEST,X'80'          SEE IF TEST EST                          
         BZ    PRD7                                                             
         LA    R3,ESTERR                                                        
         FOUT  PAYESNMH,=C'*THIS IS A TEST EST*',20                             
         B     ERROR                                                            
*                                                                               
PRD7     FOUT  PAYESNMH,PESTNAME                                                
PRDX     EQU   *                                                                
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
*EDIT PUB                                                                       
PUB      LA    R2,PAYPBH                                                        
         LA    R3,PBERR                                                         
         STH   R3,ADBERNUM           FOR USE IF EDIT ERROR                      
         LHI   RF,D#PUBCOD           OCCURS WHILE DOING AN                      
         STH   RF,ADBERFLD           ADBUYER CALL                               
         TM    4(R2),X'20'                                                      
         BZ    PE3                                                              
         TM    PAYEEH+4,X'20'                                                   
         BO    PYE                                                              
PE3      DS    0H                  RE-EDIT PUB ON CHANGE OF PAYEE               
         BAS   RE,CLRPB                                                         
         BAS   RE,ANY                                                           
***                                                                             
***      PUB NAME SEARCHING                                                     
***                                                                             
         SR    R2,RA               DISPLACEMENT TO PUB                          
         LA    R4,WORK                                                          
         USING DSPARM,R4                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
         MVC   DSMEDCOD,PAYMD                                                   
*                                                                               
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),VCOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'PUB'),0,RR=RELO01                       
         DROP  R4                                                               
*                                                                               
PE3X     LA    R2,PAYPBH                MUST RESET R2                           
         GOTO1 VPUBVAL,DMCB,(5(R2),PAYPB),BPUB                                  
         CLI   0(R1),X'FF'                                                      
         BNE   PE4                                                              
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         SH    R0,=H'4'                                                         
         LA    R4,8(R2)                                                         
         AR    R4,R0                                                            
         CLC   0(4,R4),=C',ALL'                                                 
         BNE   ERROR                                                            
         GOTO1 VPUBVAL,DMCB,((R0),PAYPB),BPUB                                   
*                                                                               
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         OC    BPUB+4(2),BPUB+4                                                 
         BNZ   ERROR                                                            
         MVC   BPUB+4(2),=X'FFFF'                                               
*                                                                               
PE4      DS    0H                                                               
*                                                                               
*READ PUB REC                                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAYMD                                                     
         MVC   KEY+1(6),BPUB                                                    
         LA    R5,6                COMPARE LEN - 1                              
         CLI   BPUB+4,X'FF'        TEST ALL ZONES/EDTS                          
         BNE   PE4C                                                             
PE4B     DS    0H                                                               
         LA    R5,4                COMPARE LEN - 1                              
         XC    KEY+5(2),KEY+5                                                   
PE4C     DS    0H                                                               
         MVC   KEY+7(2),AGYALPHA                                                
         MVI   KEY+9,X'81'                                                      
PE5      DS    0H                                                               
         LA    R3,PUBNFERR                                                      
         STH   R3,ADBERNUM           FOR USE IN ADBUYER CALL                    
         BAS   RE,HIGHPUB                                                       
PE5A     DS    0H                                                               
         EX    R5,*+12                                                          
         BNE   ERROR                                                            
         BE    *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         CLC   KEY+7(3),KEYSAVE+7                                               
         BE    PUB2                                                             
PE6      DS    0H                                                               
         CLI   SVAGPROF+16,C'0'    TEST STND FILE                               
         BE    PE6B                                                             
         CLC   KEY+7(2),=C'ZZ'                                                  
         BE    PUB2                                                             
PE6B     DS    0H                                                               
         BAS   RE,SEQPUB                                                        
         B     PE5A                                                             
*                                                                               
PUB2     DS    0H                                                               
         CH    R5,=H'4'                                                         
         BE    PUB2B                                                            
         CLI   SVAGPROF+13,C'0'    IF COMBINED PAYMENTS                         
         BNE   PE4B                GO BACK AND GET BASE PUB FOR ADDR            
PUB2B    DS    0H                                                               
         BAS   RE,GETPUB                                                        
         MVC   SVPUBDA,KEY+27                                                   
         MVC   SVNAME,=CL30'PAY PUB DIRECT'                                     
         LAY   R1,PUBIO            POINT TO PUB RECORD                          
         USING PUBREC,R1          ESTABLISH RECORD                              
         MVC   SVADD1,PUBLINE1                                                  
         MVC   SVADD2,PUBLINE2                                                  
*                                                                               
         NI    FXSW,X'FF'-X'40'    TURN OFF SWITCH                              
*                                                                               
         CLC   =C'90',PUBSTACD     IF NOT CANADIAN PUB                          
         BE    *+8                                                              
         OI    FXSW,X'40'   ASSUME AMERICAN PUB FOR FX                          
*                                                                               
         FOUT  PAYPBNMH,PUBNAME                                                 
         OI    4(R2),X'20'                                                      
*                                  NEWSPAPER ONLY                               
         CLI   PAYMD,C'N'                                                       
         BNE   PUB4                                                             
         LA    R4,PAYPBNM+20                                                    
         OI    0(R4),C' '                                                       
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R4,*-12                                                          
* PUB CITY AND STATE                                                            
         MVC   3(16,R4),PUBCITY                                                 
         LA    R4,19(R4)                                                        
         OI    0(R4),C' '                                                       
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R4,*-12                                                          
         MVC   2(2,R4),PUBSTATE                                                 
         SPACE 2                                                                
*TEST IF REP INPUT                                                              
PUB4     EQU   *                                                                
         LA    R2,PAYPBH                                                        
         MVC   RPAGY,AGYALPHA                                                   
*                                                                               
         CLC   PAYEE(5),=C'P0000'    OVERRIDE TO PAY DIRECT                     
         BE    PUB4DIR               TREAT LIKE REP NOT INPUT                   
*                                                                               
         CLI   PAYEEH+5,0          SEE IF REP INPUT                             
         BNE   PUBX                                                             
*                                                                               
PUB4DIR  DS    0H                                                               
         MVC   RPAGY,PUBKAGY                                                    
         FOUT  PAYEENMH,=C'PAY PUB DIRECT'                                      
         OI    PAYEEH+4,X'20'                                                   
         MVC   FULL(3),=3X'FF'                                                  
         LAY   R5,PUBIO            POINT TO PUB RECORD                          
P4A0     DS    0H                                                               
*                                                                               
         MVI   ADRTYP,C'P'         PAY ADDRESS                                  
*                                    FILL IN 7 BYTES OF CLTDATA                 
         MVC   CLTAGY,PUBKAGY        AGENCY                                     
         MVC   CLTMED,PUBKMED        MEDIA                                      
         MVC   CLTCODE,PAYCL         CLIENT CODE                                
         MVC   CLTOFF,SVPYOFF        CLIENT OFFICE                              
*                                                                               
         GOTO1 =V(PPGETADR),DMCB,(ADRTYP,CLTDATA),(R5),VDATAMGR,       X        
               RR=RELO01                                                        
*                                                                               
         CLI   0(R1),X'FF'         ERROR IN CALL ?                              
         BNE   *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
         CLI   0(R1),X'08'         PAY ADDRESS FOUND ?                          
         BNE   P4C                 NO - LOOK FOR REP                            
*                                  ADDRESS REC FOUND                            
         L     R5,4(R1)            A(ADDRESS INFO FROM CALL)                    
         USING PGETADRD,R5                                                      
         MVC   SVNAME,PGADNAME                                                  
         MVC   SVADD1,PGADLIN1                                                  
         MVC   SVADD2,PGADLIN2                                                  
         MVC   FULL(3),1(R1)       ADDRESS "LEVEL"                              
*                                                                               
         DROP  R1,R5                                                            
*                                                                               
         B     P4C                 LOOK FOR REP                                 
*                                                                               
ADRTYP   DS    CL1          TYPE OF ADDRESS REC - PAY, TRAFFIC, ETC.            
CLTDATA  DS    0CL7         USED TO PASS KEY INFO TO PPGETADR MODULE            
CLTAGY   DS    CL2                                                              
CLTMED   DS    CL1                                                              
CLTCODE  DS    CL3                                                              
CLTOFF   DS    CL1                                                              
*                                                                               
P4C      DS    0H                                                               
*                                                                               
         CLC   PAYEE(5),=C'P0000'    OVERRIDE TO PAY DIRECT                     
         BE    PUBX                  DON'T TRY FOR A REP                        
*                                                                               
         LAY   R5,PUBIO            POINT TO PUB RECORD                          
         LA    R5,33(R5)                                                        
         MVI   ELCODE,X'14'                                                     
         CLI   0(R5),X'14'                                                      
         BE    PUB4B                                                            
PUB4A    DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   PUBX                                                             
         USING PUBREPEL,R5                                                      
PUB4B    DS    0H                                                               
         CLC   PUBRPOFF,=3X'FF'                                                 
         BE    PUB4D                                                            
         CLC   PUBRPOFF,PAYCL                                                   
         BE    PUB4C                                                            
         CLI   PUBRPOFF,X'FF'          OFFICE                                   
         BNE   PUB4A                                                            
         CLC   PUBRPOFF+1(1),SVPYOFF                                            
**                  CHANGED FROM +1(2)  9/22/88B.A.P.                           
         BNE   PUB4A                                                            
*                                                                               
PUB4C    DS    0H                                                               
         OC    PUBPAREP(12),PUBPAREP     TEST ANY OVERIDES                      
         BZ    PUB4A               NO                                           
*                                                                               
PUB4D    DS    0H                                                               
         CLC   PUBPAREP,=4C'0'                                                  
         BE    PUBX                                                             
         OC    PUBPAREP,PUBPAREP                                                
         BZ    PUBX                                                             
         CLC   FULL(3),PUBRPOFF    TEST IF ADDR OR REP MORE SPECIFIC            
         BL    PUBX                ADDR                                         
*                                  REP                                          
         MVC   PAYEEH+4(2),=X'8005'                                             
         MVI   PAYEE,C'P'                                                       
*                                                                               
*  WAS 'S' - 'S' WILL BE USED FOR SPECIAL REP                                   
*                                                                               
         MVC   PAYEE+1(4),PUBPAREP                                              
         MVC   SVREP(4),PUBPAREP                                                
         MVI   SVREPTYP,C'P'                                                    
         FOUT  PAYEEH                                                           
*                                                                               
         DROP  R5                                                               
*                                                                               
PUBX     DS    0H                                                               
*                                                                               
*        FIND REP ELEMENT THAT HAS PAY IF CASH RECEIVED INFO                    
*                                                                               
         XC    SVREPELM,SVREPELM   INIT SAVEAREA                                
*                                                                               
         LAY   R4,PUBIO            POINT TO PUB RECORD                          
         USING PUBREC,R4           ESTABLISH PUB RECORD                         
*                                                                               
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         LA    R5,PUBREC+33        POINT TO FIRST ELEMENT IN RECORD             
*                                                                               
OPTCHKLP DS    0H                                                               
*                                                                               
         USING PUBREPD,R5          ESTABLISH AS REP ELEMENT                     
*                                                                               
         CLI   PUBREPEL,0          DONE IF END OF RECORD                        
         BE    OPTCHKDN                                                         
*                                                                               
         CLI   PUBREPEL,PUBREPQ    IF ELEMENT FOUND                             
         BNE   OPTCHKCN                                                         
*                                                                               
         CLC   PUBRPOFF,PAYCL         USE IF FOR THIS CLIENT                    
         BE    OPTCHKFD                                                         
*                                                                               
         CLI   PUBRPOFF,X'FF'         IF NOT ANOTHER CLIENT                     
         BNE   OPTCHKCN                                                         
*                                                                               
         CLC   SVPYOFF,PUBRPOFF+1        IF FOR CLIENT'S OFFICE                 
         BNE   *+10                                                             
         LR    R0,R5                        SAVE ADDRESS                        
         B     OPTCHKCN                                                         
*                                                                               
         CLC   PUBRPOFF,=3X'FF'          IF FOR AGENCY                          
         BNE   OPTCHKCN                                                         
*                                                                               
         LTR   R0,R0                     AND OFFICE ELM NOT FOUND               
         BNZ   *+6                                                              
         LR    R0,R5                         SAVE ELEMENT ADDRESS               
*                                                                               
OPTCHKCN DS    0H                                                               
*                                                                               
         IC    RF,PUBREPEL+1       BUMP TO NEXT ELEMENT                         
         AR    R5,RF                                                            
         B     OPTCHKLP                                                         
*                                                                               
OPTCHKDN DS    0H                                                               
*                                                                               
         LTR   R0,R0               IF NO ELEMENT FOUND                          
         BZ    OPTCHKOK               ASSUME BILL NOT NEEDED TO BE PAID         
*                                                                               
         LR    R5,R0               POINT TO APPROPRIATE ELEMENT                 
*                                                                               
OPTCHKFD DS    0H                                                               
*                                                                               
         MVC   SVREPELM,0(R5)      SAVE REP ELEMENT                             
*                                                                               
OPTCHKOK DS    0H                                                               
*                                                                               
         DROP  R4,R5                                                            
*                                                                               
         CLI   PROGPRO2+0,C'P'     BASE CONTROL DATE ON INVOICE DATE?           
         BNE   OPTCHKX             NO - AT LEAST NOT FROM PUB "DAYS"            
*     RESET SVDY... DAYS IN CASE PROGPRO2 NOT REREAD IN CLT VALIDATION          
         MVC   SVDYNCD,PROGPRO2+01    DAYS TO ADD TO INV DTE (NO CD)            
         MVC   SVDYCD,PROGPRO2+02     DAYS TO ADD TO INV DTE (CD)               
         BRAS  RE,CKPAYEL          LOOK FOR DAY ADJUSTS IN PUB                  
*                                                                               
OPTCHKX  DS    0H                                                               
*                                                                               
         OI    4(R2),X'20'                                                      
         FOUT  PAYEENMH,SVNAME                                                  
         FOUT  PAYADD1H,SVADD1                                                  
         FOUT  PAYADD2H,SVADD2                                                  
         EJECT                                                                  
*EDIT PAYEE                                                                     
PYE      LA    R2,PAYEEH                                                        
         LA    R3,EEERR                                                         
         STH   R3,ADBERNUM           FOR USE IF EDIT ERROR                      
         LHI   RF,D#PAYREP           OCCURS WHILE DOING AN                      
         STH   RF,ADBERFLD           ADBUYER CALL                               
         TM    4(R2),X'20'                                                      
         BO    PYEX                                                             
         CLI   5(R2),0                                                          
         BE    PYE4                                                             
         BAS   RE,CLREE                                                         
*  REP INPUT MUST BE PRECEDED BY P - PAYING REP OVERRIDE                        
*  WAS 'S' - 'S' WILL BE USED FOR SPECIAL REP                                   
*                                                                               
         CLI   8(R2),C'S'          FOR SPECIAL REP                              
         BE    PYE2                                                             
         CLI   8(R2),C'P'          PAYING REP OVERRIDE                          
         BNE   ERROR                                                            
PYE2     MVC   SVREPTYP,8(R2)      SAVE TYPE OF REP INPUT P OR S                
         MVC   8(18,R2),9(R2)                                                   
         MVI   26(R2),0                                                         
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         BCTR  R5,R0                                                            
         FOUT  (R2)                                                             
*READ                                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(2),RPAGY                                                     
         MVC   KEY+2(1),PAYMD                                                   
         MVI   KEY+3,X'11'                                                      
*                                                                               
         MVC   KEY+4(4),8(R2)                                                   
         CLC   KEY+4(4),=4C'0'                                                  
         BE    PYE6                                                             
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BE    PYE3                                                             
**NEW 3/15/91                                                                   
         CLI   SVAGPROF+16,C'0'    SEE IF USING STANDARD FILE                   
         BE    ERROR                                                            
**NEW 3/15/91                                                                   
         MVC   KEY,KEYSAVE         TRY FOR ZZ DEFAULT REP (OUTDOOR)             
         MVC   KEY(2),=C'ZZ'                                                    
**NEW 3/15/91                     WAS JUST BAL  RE,READ                         
         BAS   RE,HIGH                                                          
         CLC   KEY(9),KEYSAVE                                                   
         BNE   ERROR                                                            
**NEW 3/15/91                                                                   
PYE3     BAS   RE,GETREC                                                        
         L     R9,AREC                                                          
         USING PREPREC,R9                                                       
         FOUT  PAYEENMH,PREPNAME                                                
         FOUT  PAYADD1H,PREPLIN1                                                
         FOUT  PAYADD2H,PREPLIN2                                                
         MVC   SVREP(4),PREPREC+4                                               
         B     PYEX                                                             
PYE4     EQU   *                   PAYEE NOT INPUT                              
         TM    PAYPBH+4,X'80'      TEST PUB DONE THIS TIME                      
         BNZ   PYE6                YES                                          
*                                  NO - GET PAYEE FROM PUB                      
         MVC   KEY+27(4),SVPUBDA                                                
         BAS   RE,GETPUB                                                        
         B     PUB4                                                             
PYE6     FOUT  PAYEENMH,SVNAME                                                  
         FOUT  PAYADD1H,SVADD1                                                  
         FOUT  PAYADD2H,SVADD2                                                  
         XC    SVREP(4),SVREP                                                   
         MVI   SVREPTYP,0                                                       
PYEX     DS    0H                                                               
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
* EDIT DATE                                                                     
DTE      LA    R2,PAYDTH                                                        
         LA    R3,DTERR                                                         
         STH   R3,ADBERNUM           FOR USE IF EDIT ERROR                      
         LHI   RF,D#STEND            OCCURS WHILE DOING AN                      
         STH   RF,ADBERFLD           ADBUYER CALL                               
         TM    4(R2),X'20'                                                      
         BO    DTX                                                              
         MVI   BLIN,0                                                           
         BAS   RE,ANY                                                           
*              IF DATE GREATER THAN 6 POSITIONS GO TO DTE4                      
*                                  TO VALIDATE M/D/Y                            
         CLI   5(R2),6                                                          
         BH    DTE4                                                             
         GOTO1 VDATVAL,DMCB,(2,8(R2)),WORK                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    DTE4                                                             
         MVC   WKSTART(4),WORK                                                  
         MVC   WKSTART+4(2),=C'01'                                              
         MVC   WORK+4(2),=C'28'                                                 
DTE2     GOTO1 VADDAY,(R1),WORK,WORK+6,1                                        
         CLC   WORK+2(2),WORK+8                                                 
         BNE   DTE2B                                                            
         MVC   WORK(6),WORK+6                                                   
         B     DTE2                                                             
DTE2B    MVC   WKEND(6),WORK                                                    
         MVC   WKSTART+4(2),=C'00'     SET DAY BACK TO 00                       
         B     DTE10                                                            
         SPACE 2                                                                
*TEST FOR M/D/Y                                                                 
DTE4     GOTO1 VDATVAL,DMCB,(0,8(R2)),WORK                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    ERROR                                                            
         MVC   WKSTART(6),WORK                                                  
         LA    R4,8(R2)                                                         
         A     R4,0(R1)                                                         
         MVC   WKEND(6),WKSTART                                                 
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         C     R0,0(R1)                                                         
         BE    DTE10               ONLY 1 DATE                                  
*        IF M/D/Y END DATE MUST ALSO BE GIVEN M/D/Y                             
         CLI   0(R4),C'-'                                                       
         BNE   ERROR                                                            
         GOTO1 (RF),(R1),(0,1(R4))                                              
         OC    0(4,R1),0(R1)                                                    
         BZ    DTE8                                                             
         MVC   WKEND(6),WORK                                                    
         B     DTE10                                                            
         SPACE 2                                                                
DTE8     DS    0H                   EDIT LINE NUMBER                            
         MVI   WORK,0                                                           
         LA    R4,1(R4)                                                         
         CLI   0(R4),C'0'           SEE IF DIGIT                                
         BNL   DTE9                                                             
         CLI   0(R4),C'A'                                                       
         BL    ERROR                                                            
         CLI   0(R4),C'P'                                                       
         BH    ERROR                                                            
         BNE   DTE8C                                                            
         CLI   1(R4),C'5'                                                       
         BH    ERROR                                                            
DTE8C    MVC   WORK(1),0(R4)                                                    
         LA    R4,1(R4)                                                         
DTE9     BAS   RE,FINDNUM                                                       
         CVB   R0,DUB                                                           
         CLI   WORK,0                                                           
         BNE   DTE9B               TO ALLOW FOR A0, ETC                         
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         CH    R0,=H'99'                                                        
         BH    ERROR                                                            
DTE9B    CLI   5(R2),11                                                         
         BH    ERROR                                                            
         OC    BEST,BEST                                                        
         BZ    ERROR                                                            
         CLI   WORK,0                                                           
         BE    DTE9X                                                            
         CH    R0,=H'9'                                                         
         BH    ERROR                                                            
         LA    R1,ALPHTAB                                                       
         LH    RF,=H'100'                                                       
DTE9F    CLC   WORK(1),0(R1)                                                    
         BE    DTE9H                                                            
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'           SOMETHING VERY WRONG                              
         LA    R1,1(R1)                                                         
         AH    RF,=H'10'                                                        
         B     DTE9F                                                            
*                                                                               
DTE9H    AR    R0,RF                                                            
DTE9X    STC   R0,BLIN                                                          
         SPACE 2                                                                
*                                                                               
DTE10    DS    0H                                                               
         CLC   WKEND,WKSTART                                                    
         BL    ERROR    END DATE BEFORE START                                   
*                                                                               
         GOTO1 VDATCON,DMCB,WKSTART,(3,BSTART)                                  
         GOTO1 (RF),(R1),WKEND,(3,BEND)                                         
         OI    4(R2),X'20'                                                      
         B     DTX                                                              
*                                                                               
DTX      DS    0H                                                               
         CLI   PROGPROF+10,C'C'    TEST CCUSA INTERFACE ACTIVE                  
         BNE   DTXX                                                             
         MVI   ERRAREA,X'00'       CLEAR                                        
         BRAS  RE,CCUSARTN         SPECIAL CCUSA INTERFACE PROCESSING           
         BNE   ERROR               ERROR FOUND (R3 HAS ERROR NUMBER)            
         CLI   ERRAREA,X'FF'       ERROR (SET IN PAYMSG ALREADY)                
         BE    EXIT                                                             
DTXX     DS    0H                                                               
         OI    PAYDTH+4,X'20'      SET DATE VALID                               
         B     CKD                                                              
*                                                                               
*                                                                               
ALPHTAB  DC    C'ABCDEFGHIJKLMNOP',X'FF'                                        
*                                                                               
         EJECT                                                                  
*                                                                               
* EDIT CHECK CONTROL DATE                                                       
CKD      LA    R2,PAYCKH                                                        
         LA    R3,DTERR                                                         
         STH   R3,ADBERNUM           FOR USE IF EDIT ERROR                      
         LHI   RF,D#CTDTE            OCCURS WHILE DOING AN                      
         STH   RF,ADBERFLD           ADBUYER CALL                               
         TM    4(R2),X'20'                                                      
         BO    CKDX                                                             
         XC    CKDATE,CKDATE                                                    
         CLI   5(R2),0                                                          
         BNE   CKD2                                                             
         CLI   PROGPRO2+0,C'I'     IF USING INVOICE DATE (I OR P)               
         BE    CKDX                                                             
         CLI   PROGPRO2+0,C'P'                                                  
         BE    CKDX                LEAVE CKDATE AS X'000000'                    
*                                   DON'T USE TODAY                             
*                                                                               
*                                  SEE IF DOING CR/CK SWITCH                    
*                                  IF SO LEAVE CKDATE ALONE                     
         CLI   CRCKSW,0                                                         
         BNE   CKDX                                                             
*                                  NO INPUT - USE TODAY'S DATE                  
         MVC   WORK(6),SVDATE                                                   
         B     CKD3B                                                            
*                                                                               
CKD2     DS    0H                                                               
         CLC   =C'NO CHECK',8(R2)                                               
         BNE   CKD2B                                                            
         MVC   CKDATE(3),=3X'FF'   NO CHECK TO BE CREATED                       
         B     CKDX                                                             
*                                                                               
CKD2B    DS    0H                                                               
*                                  TRY M/D/Y                                    
         GOTO1 VDATVAL,DMCB,(0,8(R2)),WORK                                      
*                                                                               
         OC    0(4,R1),0(R1)                                                    
         BNZ   CKD4                                                             
*                                                                               
         CLI   5(R2),5            CHECK INPUT LENGTH                            
         BH    ERROR              IF HIGH THEN MORE THAN M/D WERE               
*                                 ENTERED                                       
*                                  NO GOOD - TRY M/D                            
         GOTO1 (RF),(R1),(1,8(R2)),WORK                                         
*                                                                               
         OC    0(4,R1),0(R1)                                                    
         BZ    ERROR                                                            
         MVC   WORK(2),SVDATE      THIS YEAR                                    
         CLC   WORK+2(4),SVDATE+2                                               
         BNL   CKD4                                                             
*                                                                               
*Y2K*    PACK  DUB,SVDATE(2)                                                    
*Y2K*    AP    DUB,=P'1'           NEXT YEAR                                    
*Y2K*    OI    DUB+7,X'0F'                                                      
*Y2K*    UNPK  WORK(2),DUB                                                      
*                                                                               
*  CANNOT ADD PACKED TO "FUNNY" DATE SO OUTPUT SVDATE AS "NON-FUNNY"            
*  FOR INCREMENTING AND THEN "RE-OUTPUT" BACK TO "FUNNY"                        
*                                                                               
         GOTO1 VDATCON,DMCB,(0,SVDATE),(X'20',WORK+40)                          
         PACK  DUB,WORK+40(2)                                                   
         AP    DUB,=P'1'           NEXT YEAR                                    
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+40(2),DUB                                                   
         GOTO1 VDATCON,DMCB,(0,WORK+40),(0,WORK+40)  INCREMENTED YEAR           
         MVC   WORK(2),WORK+40                       TO "FUNNY" FORMAT          
*                                  IF NOT SAME YEAR TRANSMIT DATE               
CKD3B    DS    0H                                                               
         GOTO1 VDATCON,DMCB,WORK,(5,8(R2))                                      
*                                                                               
         FOUT  (R2)                                                             
*                                                                               
CKD4     DS    0H                                                               
         GOTO1 VDATCON,DMCB,WORK,(3,CKDATE)                                     
*                                                                               
         LA    R3,CHKERR2                                                       
         STH   R3,ADBERNUM         FOR USE IN ADBUYER CALL                      
         CLC   WORK(6),SVDATE      NOT BEFOER TODAY                             
         BL    ERROR                                                            
         CLC   WORK(4),SVDATE      SAME MONTH                                   
         BE    CKDX                                                             
*                                                                               
*                                  TEST CHECK DATE VS TODAY                     
         GOTO1 VADDAY,DMCB,SVDATE,WORK+6,61                                     
*                                                                               
         CLC   WORK(6),WORK+6                                                   
         BNH   CKDX                                                             
         LA    R3,CHKERR                                                        
         STH   R3,ADBERNUM           FOR USE IN ADBUYER CALL                    
         B     ERROR                                                            
*                                                                               
CKDX     DS    0H                                                               
         OI    4(R2),X'20'                                                      
         SPACE 2                                                                
* EDIT INVOICE DATE                                                             
INVD     DS    0H                                                               
         LA    R2,PAYINVDH                                                      
*                                                                               
*        CLC   =C'AUTP',PAYMD+1               FROM PRINT AUTOPAY?               
*        BNE   INVDA                                                            
*        GOTO1 VDATCON,DMCB,(5,0),(8,8(R2))   YES                               
*        OI    6(R2),X'80'                    INVOICE DATE = TODAY              
*        MVI   5(R2),8                                                          
*                                                                               
INVDA    CLI   PROGPRO2+0,C'I'          IGNORE IF NO AOA PROFILE                
         BE    INVDC                                                            
         CLI   PROGPRO2+0,C'P'          IGNORE IF NO AOA PROFILE                
         BNE   INVD1                                                            
INVDC    CLI   PAYCKH+5,0               SEE IF CHK DATE INPUT                   
         BNE   *+10                                                             
         XC    CKDATE,CKDATE            CLEAR CHECK DATE                        
*                                       IT WILL BE CALCULATED LATER             
INVD1    LA    R3,DTERR                                                         
         STH   R3,ADBERNUM           FOR USE IF EDIT ERROR                      
         LHI   RF,D#INVDAT           OCCURS WHILE DOING AN                      
         STH   RF,ADBERFLD           ADBUYER CALL                               
         XC    INVDATE,INVDATE                                                  
         CLI   5(R2),0                                                          
         BNE   INVD2                                                            
******   CLC   PAYMD+1(4),=C'TEST'    DON'T REQUIRE FOR TEST                    
******   BE    INVDX                                                            
         OC    CKDATE,CKDATE       SEE IF CHECK DATE ENTERED                    
         BNZ   INVDX         YES - DON'T REQUIRE INVOICE DATE                   
         CLI   PROGPRO2+0,C'I'     SEE IF REQUIRED                              
         BE    INVDF                                                            
         CLI   PROGPRO2+0,C'P'     SEE IF REQUIRED                              
         BNE   INVDX                                                            
INVDF    LA    R3,MISSERR          INVOICE DATE REQUIRED ERROR                  
         STH   R3,ADBERNUM           FOR USE IN ADBUYER CALL                    
         B     ERROR                                                            
*                                                                               
INVD2    DS    0H                                                               
         CLC   =C'NO CHECK',8(R2)                                               
         BNE   INVD2B                                                           
         MVC   INVDATE(3),=3X'FF'  NO CHECK TO BE CREATED                       
         MVC   CKDATE(3),=3X'FF'   ALSO SET CKDATE                              
         B     INVDX                                                            
*                                                                               
INVD2B   DS    0H                                                               
*                                  TRY M/D/Y                                    
         GOTO1 VDATVAL,DMCB,(0,8(R2)),WORK                                      
*                                                                               
         OC    0(4,R1),0(R1)                                                    
         BNZ   INVD4                                                            
*                                                                               
         CLI   5(R2),5            CHECK INPUT LENGTH                            
         BH    ERROR              IF HIGH THEN MORE THAN M/D WERE               
*                                 ENTERED                                       
*                                  NO GOOD - TRY M/D                            
         GOTO1 (RF),(R1),(1,8(R2)),WORK                                         
*                                                                               
         OC    0(4,R1),0(R1)                                                    
         BZ    ERROR                                                            
         MVC   WORK(2),SVDATE      THIS YEAR                                    
         CLC   WORK+2(4),SVDATE+2                                               
         BNL   INVD4                                                            
*                                                                               
*Y2K*    PACK  DUB,SVDATE(2)                                                    
*Y2K*    AP    DUB,=P'1'           NEXT YEAR                                    
*Y2K*    OI    DUB+7,X'0F'                                                      
*Y2K*    UNPK  WORK(2),DUB                                                      
*                                                                               
*  CANNOT ADD PACKED TO "FUNNY" DATE SO OUTPUT SVDATE AS "NON-FUNNY"            
*  FOR INCREMENTING AND THEN "RE-OUTPUT" BACK TO "FUNNY"                        
*                                                                               
         GOTO1 VDATCON,DMCB,(0,SVDATE),(X'20',WORK+40)                          
         PACK  DUB,WORK+40(2)                                                   
         AP    DUB,=P'1'           NEXT YEAR                                    
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+40(2),DUB                                                   
         GOTO1 VDATCON,DMCB,(0,WORK+40),(0,WORK+40)  INCREMENTED YEAR           
         MVC   WORK(2),WORK+40                       TO "FUNNY" FORMAT          
*                                  IF NOT SAME YEAR TRANSMIT DATE               
INVD3B   DS    0H                                                               
         GOTO1 VDATCON,DMCB,WORK,(5,8(R2))                                      
*                                                                               
         FOUT  (R2)                                                             
*                                                                               
INVD4    DS    0H                                                               
         GOTO1 VDATCON,DMCB,WORK,(3,INVDATE)                                    
*                                                                               
*                                                                               
INVDX    DS    0H                                                               
         OI    4(R2),X'20'     SET PREV VALIDATED                               
         FOUT  PAYMSGH,=C'PLEASE ENTER INVOICES AND AMOUNTS'                    
*                                                                               
         LA    R2,PAYINV1H                                                      
         B     EXIT                                                             
         EJECT                                                                  
*CLEAR ALL                                                                      
CLRMD    NI    PAYMDH+4,X'DF'                                                   
         XC    PAYMDNM,PAYMDNM                                                  
         FOUT  PAYMDNMH                                                         
CLROP    NI    PAYOPH+4,X'DF'                                                   
CLRCL    NI    PAYCLH+4,X'DF'                                                   
         XC    PAYCLNM,PAYCLNM                                                  
         FOUT  PAYCLNMH                                                         
CLRPR    NI    PAYPRH+4,X'DF'                                                   
         XC    PAYPRNM,PAYPRNM                                                  
         FOUT  PAYPRNMH                                                         
         XC    PAYESNM,PAYESNM                                                  
         FOUT  PAYESNMH                                                         
CLRPB    NI    PAYPBH+4,X'DF'                                                   
         XC    PAYPBNM,PAYPBNM                                                  
         FOUT  PAYPBNMH                                                         
         XC    PAYADD1,PAYADD1                                                  
         FOUT  PAYADD1H                                                         
         XC    PAYADD2,PAYADD2                                                  
         FOUT  PAYADD2H                                                         
CLREE    NI    PAYEEH+4,X'DF'                                                   
         XC    PAYEENM,PAYEENM                                                  
         FOUT  PAYEENMH                                                         
         XC    PAYXLIN,PAYXLIN                                                  
         FOUT  PAYXLINH                                                         
         XC    SVREP,SVREP                                                      
         MVI   SVREPTYP,0                                                       
*                                                                               
         NI    PAYCKH+4,X'DF'       UNVALIDATE CHECK CONTROL DATE               
         NI    PAYINVDH+4,X'DF'     UNVALIDATE INVOICE DATE                     
*                                                                               
CLRINV   CLI   HALF,C'C'                                                        
         BER   RE                                                               
         MVI   HALF,C'C'                                                        
         CLI   PROGPROF+1,C'N'     DONT FORCE CLEAR                             
         BER   RE                                                               
         CLI   INVMATSW,C'Y'       SKIP CLEAR IF WE CAME FROM MATCH             
         BER   RE                                                               
         LA    R4,PAYINV1H                                                      
         SR    R5,R5                                                            
CLRINV2  IC    R5,0(R4)                                                         
         SH    R5,=H'9'                                                         
         EX    R5,CLROC                                                         
         BZ    CLRINV3                                                          
         EX    R5,CLRXC                                                         
         FOUT  (R4)                                                             
CLRINV3  LA    R4,9(R4,R5)                                                      
         CLI   0(R4),0                                                          
         BNE   CLRINV2                                                          
         BR    RE                                                               
CLROC    OC    8(0,R4),8(R4)                                                    
CLRXC    XC    8(0,R4),8(R4)                                                    
*                                                                               
         EJECT                                                                  
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         BCR   8,RE                                                             
         CLI   0(R5),0                                                          
         BNE   *-18                                                             
         LTR   R5,R5                                                            
         BR    RE                                                               
         SPACE 2                                                                
FINDNUM  EQU   *                                                                
         SPACE 1                                                                
         LR    R5,R4                                                            
FNUM2    CLI   0(R4),C'0'                                                       
         BL    FNUM4                                                            
         LA    R4,1(R4)                                                         
         B     FNUM2                                                            
FNUM4    EQU   *                                                                
         SR    R4,R5                                                            
         BNP   ERROR                                                            
         BCTR  R4,R0                                                            
         EX    R4,*+6                                                           
         BR    RE                                                               
         PACK  DUB,0(0,R5)                                                      
         SPACE 2                                                                
PATCH    DC    6D'0'                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITL    LR    R4,RC               SET UP TO CLEAR WORK SPACE                   
         LR    R5,RD                                                            
         SR    R5,R4                                                            
         LR    R0,RE                                                            
         BAS   RE,CLEARWRK                                                      
         LM    R2,R4,0(R1)                                                      
         PACK  AGYNUM,0(1,R1)      AGENCY NUMBER                                
         LH    R5,0(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,FRSTFLD          A(FIRST INPUT FIELD HEADER)                  
         LH    R5,2(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,LASTFLD          A(LAST INPUT FIELD)                          
         MVC   NUMFLD,4(R2)        NUMBER OF FIELDS                             
         ST    R3,VTWA             A(TWA)                                       
         MVC   VDATAMGR(36),0(R4)  FACILITY LIST                                
         LR    RA,R3                                                            
         MVC   TERMNAL,0(RA)       TERMINAL NUMBER                              
         MVC   AGYALPHA,14(RA)     ALPHA AGENCY CODE                            
         LA    R3,64(R3)                                                        
         ST    R3,ERRAREA          PRESET ERRAREA TO A(FIRST HEADER)            
         MVI   DMINBTS,X'C0'       PRESET DATAMGR CONTROL BITS                  
         MVI   DMOUTBTS,X'FD'      PRESET DATAMGR ERROR CHECK BITS              
         MVC   AREC,AIOAREA                                                     
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
CLEARWRK LTR   R5,R5               CLEAR STORAGE TO ZEROS                       
         BCR   8,RE                                                             
         CHI   R5,250                                                           
         BNH   CLEAREST                                                         
         XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         AHI   R5,-250                                                          
         B     CLEARWRK                                                         
*                                                                               
CLEAREST BCTR  R5,R0                                                            
         EX    R5,VARCLEAR                                                      
         BR    RE                                                               
*                                                                               
VARCLEAR XC    0(0,R4),0(R4)                                                    
         EJECT                                                                  
*                  FARMABLE CODE                                                
         SPACE 3                                                                
ANY      CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         LA    R3,1                                                             
         B     ERROR                                                            
         SPACE 2                                                                
ANY2     TM    4(R2),X'10' .       IS IT VALID NUMERIC                          
         BCR   8,RE .              IF APPLICABLE                                
         LA    R3,3                                                             
         B     ERROR                                                            
         SPACE 2                                                                
PACK     SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         TM    4(R2),X'08'                                                      
         BCR   8,RE                        OR NON NUMERIC                       
         BCTR  R1,R0                                                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         BR    RE                                                               
         SPACE 2                                                                
VARPACK  PACK  DUB,8(0,R2)                                                      
         SPACE 2                                                                
MOVE     MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         BCTR  R1,R0                                                            
         EX    R1,VARMOVE                                                       
         BR    RE                                                               
         SPACE 2                                                                
VARMOVE  MVC   WORK(0),8(R2)                                                    
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (DIRECTORY)                  
         SPACE 3                                                                
READ     MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
ADD      MVC   COMMAND,=C'DMADD '                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
DIRCTRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
         SPACE 3                                                                
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
FILE     NTR                                                                    
         LA    R2,KEY+27                                                        
         CLC   COMMAND(5),=C'DMDEL'                                             
         BE    *+12                                                             
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),AREC,(TERMNAL,DMWORK)                                       
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (PUBDIR)                     
         SPACE 3                                                                
READPUB  MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIRY                                                          
         SPACE 2                                                                
SEQPUB   MVC   COMMAND,=C'DMRSEQ'                                               
         B     PUBDIRY                                                          
         SPACE 2                                                                
HIGHPUB  MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIRY                                                          
         SPACE 2                                                                
WRITEPUB MVC   COMMAND,=C'DMWRT'                                                
         B     PUBDIRY                                                          
         SPACE 2                                                                
ADDPUBD  MVC   COMMAND,=C'DMADD '                                               
         B     PUBDIRY                                                          
         SPACE 2                                                                
PUBDIRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (PUBFILE)                    
         SPACE 3                                                                
GETPUB   MVC   COMMAND,=C'GETREC'                                               
         B     PUBFILE                                                          
         SPACE 2                                                                
PUTPUB   MVC   COMMAND,=C'PUTREC'                                               
         B     PUBFILE                                                          
         SPACE 2                                                                
ADDPUB   MVC   COMMAND,=C'ADDREC'                                               
         B     PUBFILE                                                          
         SPACE 2                                                                
PUBFILE  NTR                                                                    
         LA    R2,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBFILE',            X        
               (R2),PUBIO,(TERMNAL,DMWORK)                                      
         B     DMCHECK                                                          
         EJECT                                                                  
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT                                                                    
         SPACE 2                                                                
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                  EXITS FROM PROGRAM                                           
         SPACE 3                                                                
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
         SPACE 2                                                                
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
         SPACE 2                                                                
EXIT     OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        OPTIONS VALIDATION                                           *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
OPTVAL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    ICLOPT,ICLOPT       INIT OPTION SAVEAREAS                        
         XC    GSTINDET,GSTINDET                                                
         XC    SVPSTEL,SVPSTEL     INIT PST ELEMENT SAVEAREA                    
         MVI   SVGST,0             INIT GST OVERRIDE SAVEAREA                   
         MVI   SVPSTOVR,0          INIT PST MASTER OVERRIDE SAVEAREA            
         MVI   SVPSTSW,0           INIT PST SWITCHES                            
         XC    NCDSW,NCDSW                                                      
*                                                                               
         MVI   MLTREQSW,0                                                       
         MVI   MLTOPTSW,0                                                       
*                                                                               
         MVI   CRCKSW,0                                                         
         XC    CRCKDAT,CRCKDAT                                                  
         MVI   CRCKSEQ,0                                                        
         MVI   CDPAY,C'N'                                                       
*                              CDPAY WILL BE SET TO 'Y' IF                      
*                              ANY ITEM BEING PAID HAS A PAYABLE CD             
*                                                                               
         XC    SVID,SVID                                                        
         MVI   SVXFROPT,0                                                       
*                                                                               
         LA    R2,PAYOPH           SKIP IF NO OPTIONS ENTERED                   
         CLI   5(R2),0                                                          
         BE    OPTVALX                                                          
*                                                                               
OPT1     DS    0H                                                               
*                                                                               
         LAY   R4,PUBIO                                                         
         XC    0(256,R4),0(R4)     INIT SCANNER AREA                            
*                                                                               
*********************************************************                       
** NOTE -- SCANNER ENTRIES ARE NON-STANDARD *34* BYTES **                       
*********************************************************                       
*                                                                               
         GOTO1 VSCANNER,DMCB,(12,(R2)),(5,(R4))                                 
*                                                                               
OPTLOOP  DS    0H                                                               
*                                                                               
         CLI   0(R4),0             DONE AT END OF SCANNER ENTRIES               
         BE    OPTDONE                                                          
*                                                                               
         TITLE 'T40301 - PAY PROGRAM HEADLINE EDIT - OPTXFR'                    
***********************************************************************         
*                                                                     *         
*        XFR - OPTION                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OPTXFR   DS    0H                                                               
*                                                                               
         CLC   =C'XFR',12(R4)      SKIP IF NOT XFR OPTION                       
         BNE   OPTXFRN                                                          
*                                                                               
         CLC   =C'NO',22(R4)       'NO' ONLY MEANINGFUL CASE                    
         BNE   *+8                                                              
         MVI   SVXFROPT,C'N'          SAVE XFR=NO OPTION                        
*                                                                               
OPTXFRX  DS    0H                                                               
         B     OPTCONT             NEXT OPTION                                  
*                                                                               
OPTXFRN  DS    0H                                                               
*                                                                               
         TITLE 'T40301 - PAY PROGRAM HEADLINE EDIT - OPTID'                     
***********************************************************************         
*                                                                     *         
*        ID - OPTION - BYPASSED                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OPTID    B     OPTIDN              SKIP ID FOR NOW                              
*                                                                               
         CLC   =C'ID',12(R4)       SKIP IF NOT ID OPTION                        
         BNE   OPTIDN                                                           
*                                                                               
         CLI   1(R4),0             MUST HAVE A VALUE                            
         BE    OPTERR                                                           
*                                                                               
         MVC   SVID,22(R4)         SAVE ID                                      
         OC    SVID,BLANKS         FORCE UPPERCASE                              
*                                                                               
OPTIDX   DS    0H                                                               
         B     OPTCONT                                                          
*                                                                               
OPTIDN   DS    0H                                                               
*                                                                               
         TITLE 'T40301 - PAY PROGRAM HEADLINE EDIT - OPTICL'                    
***********************************************************************         
*                                                                     *         
*        INVOICE CHECKING LIST REPORT OPTION                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OPTICL   DS    0H                                                               
*                                                                               
         CLC   =C'ICL',12(R4)      SKIP IF NOT ICL OPTION                       
         BNE   OPTICLN                                                          
*                                                                               
         CLC   =C'NO',22(R4)       IF OPTION IS 'NO'                            
         BE    *+10                                                             
         CLC   =C'YES',22(R4)      OR OPTION IS 'YES'                           
         BNE   OPTICL1E                                                         
*                                                                               
         MVC   ICLOPT,22(R4)          SAVE OPTION                               
*                                                                               
OPTICLX  DS    0H                                                               
         B     OPTCONT                                                          
*                                                                               
OPTICLN  DS    0H                                                               
*                                                                               
         TITLE 'T40301 - PAY PROGRAM HEADLINE EDIT - OPTCD'                     
***********************************************************************         
*                                                                     *         
*        PAYING WITH CASH DISCOUNT OPTION - Y/N                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OPTCD    DS    0H                                                               
*                                                                               
         CLC   =C'CD',12(R4)       SEE IF I DON'T WANT TO PAY CD                
         BNE   OPTCDN              NCDSW CAN ALSO BE SET IN T40302              
*                                  BY PUTTING 'NCD' IN A COMMENT                
         CLC   =C'NO',22(R4)       SKIP IF NOT PAYING W/O CD                    
         BNE   OPTCDNON                                                         
*                                                                               
         MVI   NCDSW,C'N'             SAVE OPTION                               
*                                                                               
         B     OPTCDX                                                           
*                                                                               
OPTCDNON DS    0H                                                               
*                                                                               
         CLC   =C'YES',22(R4)      IF PAYING WITH CD                            
         BNE   OPTCDYN                                                          
*                                                                               
         MVI   NCDSW,0                SAVE OPTION                               
*                                                                               
         B     OPTCDX                                                           
*                                                                               
OPTCDYN  CLC   =C'LOST',22(R4)     IF AGENCY LOST CD                            
         BNE   OPTCD1E                                                          
**SJR                                                                           
         CLC   AGYALPHA,=C'SJ'      ONLY FOR SJR NOW                            
         BNE   OPTCD1E                                                          
**SJR                                                                           
         MVI   NCDSW,C'L'             SAVE OPTION                               
*                                                                               
OPTCDX   DS    0H                                                               
         B     OPTCONT                                                          
*                                                                               
OPTCDN   DS    0H                                                               
*                                                                               
         TITLE 'T40301 - PAY PROGRAM HEADLINE EDIT - OPTCRCK'                   
***********************************************************************         
*                                                                     *         
*        REVERSING CR AND CK'S - REV=MMMDD/YY=NNN                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OPTCRCK  DS    0H                                                               
*                                                                               
         CLC   =C'REV',12(R4)      SKIP IF NOT REVERSING OPTION                 
         BNE   OPTCRCKN                                                         
*                                                                               
         CLI   CRCKSW,0            MUST NOT HAVE BEEN ENTERED BEFORE            
         BNE   OPTCCK1E                                                         
*                                                                               
         MVI   CRCKSW,C'R'        INDICATE SWITCHING CR'S AND CK'S              
*                                                                               
         CLI   1(R4),0            OPTION VALUE MUST BE GIVEN                    
         BE    OPTCCK2E                                                         
*                                                                               
         XC    WORK(20),WORK      INIT WORKAREA                                 
*                                                                               
*        SPLIT FIELD INTO DATE AND SEQUENCE NUMBER                              
*              C'-' SEPARATES THE TWO                                           
*                                                                               
         LA    R3,22(R4)           POINT TO INPUT - DATE & SQN                  
*                                                                               
         LA    R0,C'-'             SEARCHING FOR DASH                           
         LLC   RF,1(R4)            LENGTH OF OPTION VALUE                       
         LA    RE,0(RF,R3)         END OF DATA                                  
*                                                                               
         SRST  RE,R3               SEARCH FOR C'-'                              
*                                                                               
         LR    R0,RE               SAVE POINTER TO POSSIBLE DASH                
*                                                                               
         SR    RE,R3               LENGTH OF DATE                               
         BZ    OPTCCK4E               MUST BE THERE                             
*                                                                               
         CH    RE,=H'8'                                                         
         BH    OPTCCK4E            DATE IS MAX OF 8 CHARS                       
*                                                                               
         BCTR  RE,0                DECREMENT FOR EXECUTE                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R3)       MOVE DATE TO WORKAREA                        
*                                                                               
         GOTO1 VDATVAL,DMCB,(0,WORK),WORK+10                                    
         OC    DMCB(4),DMCB                                                     
         BZ    OPTCCK4E               MUST BE VALID DATE                        
*                                                                               
*                                  SAVE DATE IN BINARY                          
         GOTO1 VDATCON,DMCB,(0,WORK+10),(3,CRCKDAT)                             
*                                                                               
*        CAPTURE SEQUENCE NUMBER                                                
*                                                                               
         LR    RE,R0               RESTORE POINTER TO POSSIBLE DASH             
*                                                                               
         CLI   0(RE),C'-'          SKIP IF NO DASH                              
         BNE   OPTCRCKX                                                         
*                                                                               
         LLC   RF,1(R4)            GET TOTAL OPTION VALUE LENGTH                
         SR    RE,R3               LENGTH SO FAR                                
         AHI   RE,1                PLUS ONE FOR DASH                            
*                                                                               
         SR    RF,RE               LENGTH OF SQN                                
         BNP   OPTCCK2E                                                         
*                                                                               
         CHI   RF,3                MAX 3 DIGITS                                 
         BH    OPTCCK3E                                                         
*                                                                               
         LR    RE,R0               RESTORE POINTER TO POSSIBLE DASH             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),1(RE)        COPY INPUT                                   
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVN   DUB(0),=3X'F0'     KILL NUMERICS                                 
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   DUB(0),=3X'F0'     NUMERIC SQN IF EQUAL                          
         BNE   OPTCCK3E                                                         
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,1(0,RE)        PACK NUMERIC SQN                              
*                                                                               
         CVB   RF,DUB                                                           
*                                                                               
         CHI   RF,0                                                             
         BNH   OPTCCK3E            MUST BE BETWEEN 1 AND 255                    
*                                                                               
         CHI   RF,255                                                           
         BH    OPTCCK3E                                                         
*                                                                               
         STC   RF,CRCKSEQ          SAVE SEQUENCE NUMBER                         
*                                                                               
OPTCRCKX DS    0H                                                               
*                                                                               
         CLI   CRCKSEQ,0           IF NO SQN GIVEN                              
         BNE   *+8                                                              
         MVI   CRCKSEQ,1              SET DEFAULT TO 1                          
*                                                                               
         B     OPTCONT                                                          
*                                                                               
OPTCRCKN DS    0H                                                               
*                                                                               
         TITLE 'T40301 - PAY PROGRAM HEADLINE EDIT - OPTGST'                    
***********************************************************************         
*                                                                     *         
*        GST OVERRIDE                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OPTGST   DS    0H                                                               
*                                                                               
         CLC   =C'GST',12(R4)      SKIP IF NOT GST OVERRIDE                     
         BNE   OPTGSTN                                                          
*                                                                               
         CLI   1(R4),0             IF NO OTHER DATA ENTERED                     
         BNE   *+12                                                             
         MVI   GSTINDET,C'Y'          SET TO INCLUDE GST IN DETAILS             
         B     OPTGSTX                                                          
*                                                                               
         CLI   1(R4),1             OVERRIDE IS 1 POSITION                       
         BNE   OPTGST1E                                                         
*                                                                               
         CLI   SVGST,0             OKAY IF NO PREVIOUS OVERRIDE                 
         BE    *+10                                                             
         CLC   SVGST,22(R4)        OKAY IF SAME AS PREVIOUS OVERRIDE            
         BNE   OPTGST2E             ELSE ERROR                                  
*                                                                               
         CLI   22(R4),C'X'         MAKE SURE ITS A VALID CODE                   
         BE    *+8                                                              
         CLI   22(R4),C'Z'         MAKE SURE ITS A VALID CODE                   
         BE    *+8                                                              
         CLI   22(R4),C'S'         MAKE SURE ITS A VALID CODE                   
         BE    *+8                                                              
         B     OPTGST1E                                                         
*                                                                               
         MVC   SVGST,22(R4)        SAVE GST OVERRIDE                            
*                                                                               
OPTGSTX  DS    0H                                                               
         B     OPTCONT                                                          
*                                                                               
OPTGSTN  DS    0H                                                               
*                                                                               
         TITLE 'T40301 - PAY PROGRAM HEADLINE EDIT - OPTPST'                    
***********************************************************************         
*                                                                     *         
*        PST OVERRIDE                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OPTPST   DS    0H                                                               
*                                                                               
         B     OPTPST10            PST=X DEACTIVATED                            
*                                                                               
         CLC   =C'PST',12(R4)      IF PST OVERALL OVERRIDE                      
         BNE   OPTPST10                                                         
*                                                                               
         CLI   1(R4),1             VALUE MUST BE 1 LONG                         
         BNE   OPTPST1E                                                         
         CLI   22(R4),C'X'         AND EQUAL 'X'                                
         BNE   OPTPST3E                                                         
*                                                                               
         TM    SVPSTSW,X'80'       ERROR IF PROVINCIAL OVERRIDE EXISTS          
         BO    OPTPST4E                                                         
*                                                                               
         OI    SVPSTSW,X'40'       INDICATE PST MASTER OVERRIDE                 
*                                                                               
         MVC   SVPSTOVR,22(R4)     SAVE OVERRIDE                                
*                                                                               
         MVI   SVPSTEL,X'84'       SET ELEMENT CODE                             
         MVI   SVPSTEL+1,12        SET ELEMENT LENGTH                           
*                                                                               
*        OVERRIDE ALL PROVINCIAL CODES                                          
*                                                                               
         LA    R0,10               TEN PROVINCES                                
         LA    RE,SVPSTEL+2        START OF PROVINCIAL PST CODES                
*                                                                               
         MVC   0(1,RE),SVPSTOVR    OVERRIDE PROVINCIAL PST CODES                
         LA    RE,1(RE)            NEXT PROVINCE                                
         BCT   R0,*-10                                                          
*                                                                               
         B     OPTPSTX                                                          
*                                                                               
OPTPST10 DS    0H                                                               
*                                                                               
         CLI   0(R4),2             SKIP IF KEYWORD IS NOT 2 LONG                
         BNE   OPTPSTN                                                          
*                                                                               
*        ASSUME KEYWORD IS A PROVINCIAL CODE                                    
*                                                                               
*        BUILD A DUMMY FLDHDR FOR CALL TO PSTVAL                                
*                                                                               
         CLI   1(R4),1             TEST LENGTH OF PST CODE = 1                  
         BNE   OPTPST1E                                                         
*                                                                               
         TM    SVPSTSW,X'80'+X'40' ERROR IF PST OVERRIDE                        
         BM    OPTPST4E               ALREADY                                   
*                                                                               
         OI    SVPSTSW,X'80'       INDICATE PROVINCIAL PST OVERRIDE             
*                                                                               
         XC    WORK,WORK           INIT DUMMY FLDHDR                            
         MVI   WORK,48             SET FLDHDR LENGTH                            
         MVI   WORK+5,4            SET INPUT LENGTH                             
*                                                                               
         LA    R1,WORK+8           POINT TO FIELD INPUT AREA                    
*                                                                               
         MVC   0(2,R1),12(R4)      MOVE PROVINCE CODE                           
*                                                                               
         MVI   2(R1),C'='          SET DELIMITER                                
*                                                                               
         MVC   3(1,R1),22(R4)      MOVE PST VALUE                               
*                                                                               
*        BUILD PSTBLK                                                           
*                                                                               
         XC    WPSTBLK,WPSTBLK     INIT CONTROL BLOCK                           
*                                                                               
         LA    R5,WPSTBLK          ESTABLISH PSTVAL CONTROL BLOCK               
         USING PSTBLKD,R5                                                       
*                                                                               
         LA    R1,WORK             PASS A(INPUT)                                
         ST    R1,PSTADIN                                                       
*                                                                               
         XC    ELEM,ELEM           INIT WORKAREA                                
*                                                                               
         LA    R1,ELEM             GET OUTPUT TO TEMP AREA                      
         ST    R1,PSTADOUT                                                      
*                                                                               
         MVI   PSTACT,PSTVALQ      SET TO VALIDATE PST ENTRY                    
*                                                                               
         MVC   PSTACOM,VCOMFACS    A(COMFACS)                                   
*                                                                               
         XC    DMCB,DMCB           GET A(PSTVAL)                                
         GOTO1 VCALLOV,DMCB,,X'D9000A6B'    PSTVAL                              
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                MUST FIND IT                                 
*                                                                               
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(R5)      VALIDATE PST VALUE                           
*                                                                               
         CLI   PSTERR,0            OKAY IF NO ERROR                             
         BE    OPTPST15                                                         
*                                                                               
         CLI   PSTERR,2                                                         
         BE    OPTPST1E            BAD PST CODE                                 
         B     OPTPST2E            ELSE BAD PROVINCIAL CODE                     
*                                                                               
OPTPST15 DS    0H                                                               
*                                                                               
*        VALID PST CODE                                                         
*                                                                               
         LA    R0,10               10 PROVINCES                                 
         LA    RE,SVPSTEL+2                                                     
         LA    RF,ELEM                                                          
*                                                                               
*        SAVE PST CODE                                                          
*                                                                               
OPTPSTLP DS    0H                                                               
*                                                                               
         CLI   0(RF),0             TEST OVERRIDE IN NEW                         
         BE    OPTPSTCN            NO - OK                                      
*                                                                               
         CLI   0(RE),0             TEST OVERRIDE SET ALREADY                    
         BE    OPTPSTL1            NO - DOESN'T MATTER                          
*                                                                               
         CLI   0(RE),C'X'          OKAY IF PST CANCELLED                        
         BE    OPTPSTL1                                                         
*                                                                               
         CLC   0(1,RE),0(RF)       ERROR IF DIFFERENT OVERRIDE                  
         BNE   OPTPST4E                                                         
*                                                                               
OPTPSTL1 DS    0H                                                               
*                                                                               
         MVC   0(1,RE),0(RF)       UPDATE PST CODE                              
*                                                                               
         B     OPTPSTDN            DONE SINCE ONLY 1 PROV HAS                   
*                                     OVERRIDE                                  
*                                                                               
OPTPSTCN LA    RE,1(RE)            BUMP POINTERS                                
         LA    RF,1(RF)                                                         
*                                                                               
         BCT   R0,OPTPSTLP                                                      
*                                                                               
OPTPSTDN DS    0H                                                               
*                                                                               
         MVI   SVPSTEL,X'84'       SET ELEMENT CODE                             
         MVI   SVPSTEL+1,12        SET ELEMENT LENGTH                           
*                                                                               
OPTPSTX  DS    0H                                                               
         B     OPTCONT                                                          
*                                                                               
         DROP  R5                                                               
*                                                                               
OPTPSTN  DS    0H                                                               
*                                                                               
         TITLE 'T40301 - PAY PROGRAM HEADLINE EDIT - OPTSNG'                    
***********************************************************************         
*                                                                     *         
*        SINGLY OPTION                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OPTSNG   DS    0H                                                               
*                                                                               
         CLC   =C'SING',12(R4)     ONE PAY REQUEST PER BUY ?                    
         BNE   OPT35                                                            
         LA    R3,MULTERR2         "SINGLY" MUST BE ONLY OPTION                 
         CLI   5(R2),6                                                          
         BH    OPTXERR             "SINGLY" MUST BE ONLY OPTION                 
*                                                                               
         LA    RE,PAYINV2H         ONLY ALLOW ONE INVOICE, ONE AMOUNT           
         LA    RF,PAYTSTH          AND MULTIPLE COMMENTS TO BE ENTERED          
         LA    R3,MULTERR1         ONLY 1 INVOICE AND AMOUNT ALLOWED            
*                                                                               
OPT30L   DS    0H                                                               
         CLI   5(RE),0             ANYTHING THERE ?  (INVOICE)                  
         BNE   OPT30X5             YES - ERROR                                  
         ZIC   R0,0(RE)                                                         
         AR    RE,R0               BUMP TO NEXT HEADER                          
         CLI   5(RE),0             ANYTHING THERE ?  (AMOUNT)                   
         BNE   OPT30X5             YES - ERROR                                  
         ZIC   R0,0(RE)                                                         
         AR    RE,R0               BUMP TO NEXT HEADER   (COMMENT)              
         ZIC   R0,0(RE)                                                         
         AR    RE,R0               BUMP TO NEXT HEADER                          
         CR    RE,RF               END OF INPUT FIELDS ?                        
         BL    OPT30L              NO - TEST NEXT                               
*                                                                               
         MVI   MLTOPTSW,C'S'       SINGLY ENTERED AS OPTION                     
         B     OPTCONT                                                          
*                                                                               
OPT30X5  DS    0H                                                               
         LR    R2,RE               SET CURSOR                                   
         B     OPTXERR                                                          
*                                                                               
*                                                                               
OPT35    DS    0H                                                               
         CLC   =C'XSIN',12(R4)     "OVERRIDE SINGLY" ?                          
         BNE   OPTSINX            NO                                            
*                                                                               
         MVI   MLTOPTSW,C'X'       OVERRIDE SINGLY ENTERED AS OPTION            
*                                                                               
OPTXSINX DS    0H                                                               
         B     OPTCONT                                                          
*                                                                               
OPTSINX  DS    0H                                                               
*                                                                               
         B     OPTERR              UNKNOWN OPTION                               
*                                                                               
OPTCONT  LA    R4,34(R4)           ** NOTE 34 BYTE SCANNER ENTRIES **           
         B     OPTLOOP                                                          
*                                                                               
OPTDONE  DS    0H                                                               
*                                                                               
OPTVALX  DS    0H                                                               
         OI    4(R2),X'20'         FLAG FIELD AS VALIDATED                      
         CR    RB,RB               SET EQ CC                                    
         XIT1                                                                   
*                                                                               
         TITLE 'T40301 - PAY PROGRAM HEADLINE EDIT - OPTERR'                    
***********************************************************************         
*                                                                     *         
*        OPTION ERROPR ROUTINES                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OPTERR   DS    0H                                                               
         LA    R3,INVERR                                                        
         J     ERROR                                                            
*                                                                               
OPTICL1E DS    0H                                                               
         LA    R3,PPEICLNV         ICL OPTION IS YES/NO                         
         B     OPTXERR                                                          
*                                                                               
OPTCD1E  DS    0H                                                               
         LA    R3,PPECDNV          INVALID CD OPTION                            
         B     OPTXERR                                                          
*                                                                               
OPTCCK1E DS    0H                                                               
         LA    R3,PPECCK1          ONLY ONE CR/CK REVERSAL ALLOWED              
         B     OPTXERR                                                          
*                                                                               
OPTCCK2E DS    0H                                                               
         LA    R3,PPECCKMS         DATA MISSING FROM REV OPTION                 
         B     OPTXERR                                                          
*                                                                               
OPTCCK3E DS    0H                                                               
         LA    R3,PPECCKSQ         SQN 1 - 255                                  
         B     OPTXERR                                                          
*                                                                               
OPTCCK4E DS    0H                                                               
         LA    R3,PPECCKDN         DATE INVALID                                 
         B     OPTXERR                                                          
*                                                                               
OPTPST1E DS    0H                                                               
         LA    R3,PPEPSTNV         INVALID PST VALUE                            
         B     OPTXERR                                                          
*                                                                               
OPTGST1E DS    0H                                                               
         LA    R3,PPEGSTNV         INVALID GST VALUE                            
         B     OPTXERR                                                          
*                                                                               
OPTGST2E DS    0H                                                               
         LA    R3,PPEGST1          INCOMPATABLE GST OVERRIDES                   
         B     OPTXERR                                                          
*                                                                               
OPTPST2E DS    0H                                                               
         LA    R3,PPEPPCNV         INVALID PROVINCIAL CODE                      
         B     OPTXERR                                                          
*                                                                               
OPTPST3E DS    0H                                                               
         LA    R3,PPEMSPNV         INVALID PST MASTER OVERRIDE VALUE            
         B     OPTXERR                                                          
*                                                                               
OPTPST4E DS    0H                                                               
         LA    R3,PPEMSPPR         NO PROV OVR IF THERE IS A MASTER OVR         
         B     OPTXERR                                                          
*                                                                               
*                                                                               
OPTXERR  DS    0H                                                               
         L     RF,VCOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         MVI   ERRAREA,X'FF'                                                    
         GOTO1 (RF),DMCB+12,(R3),0,(C'E',DMCB),0,0,0                            
         LTR   RB,RB               SET NEQ CC                                   
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPPAY01 - LOOK FOR CONTROL DATE ADJUSTMENTS'                    
*                                                                               
         DS    0D                                                               
CKPAYEL  NTR1  BASE=*,LABEL=*      CHECKING FOR PUB PAY ELEMENTS                
*                                                                               
*        FIND PUB PAY ELEMENT THAT HAS DAYS TO ADD TO INVOICE DATE              
*                                                                               
         LAY   R4,PUBIO            POINT TO PUB RECORD                          
         USING PUBREC,R4           ESTABLISH PUB RECORD                         
*                                                                               
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         LA    R5,PUBREC+33        POINT TO FIRST ELEMENT IN RECORD             
*                                                                               
PAYCHKLP DS    0H                                                               
*                                                                               
         USING PUBPAYLD,R5         ESTABLISH AS PUB PAY ELEMENT                 
*                                                                               
         CLI   PUBPAYEL,0          DONE IF END OF RECORD                        
         BE    PAYCHKDN                                                         
*                                                                               
         CLI   PUBPAYEL,PUBPYELQ   IF ELEMENT FOUND                             
         BNE   PAYCHKCN                                                         
*                                                                               
         CLC   PUBPYNCD(2),=X'FFFF'  DAYS TO ADD TO INVCE DATE ENTERED?         
         BE    PAYCHKCN               NO - NEXT ELEMENT                         
*                                                                               
*SMY*    CLI   PUBPYNCD,X'FF'    DAYS TO ADD TO NCD INVCE DATE ENTERED?         
*SMY*    BE    PAYCHKCN               NO - NEXT ELEMENT                         
*SMY*    CLI   PUBPY_CD,X'FF'    DAYS TO ADD TO CD  INVCE DATE ENTERED?         
*SMY*    BE    PAYCHKCN               NO - NEXT ELEMENT                         
*                                                                               
         CLC   PUBPYOFF,PAYCL         USE IF FOR THIS CLIENT                    
         BE    PAYCHKFD                                                         
*                                                                               
         CLI   PUBPYOFF,X'FF'         IF NOT ANOTHER CLIENT                     
         BNE   PAYCHKCN                                                         
*                                                                               
         CLC   SVPYOFF,PUBPYOFF+1        IF FOR CLIENT'S OFFICE                 
         BNE   *+10                                                             
         LR    R0,R5                        SAVE ADDRESS                        
         B     PAYCHKCN                                                         
*                                                                               
         CLC   PUBPYOFF,=3X'FF'          IF FOR AGENCY                          
         BNE   PAYCHKCN                                                         
*                                                                               
         LTR   R0,R0                     AND OFFICE ELM NOT FOUND               
         BNZ   *+6                                                              
         LR    R0,R5                         SAVE ELEMENT ADDRESS               
*                                                                               
PAYCHKCN DS    0H                                                               
*                                                                               
         ZIC   RF,PUBPAYEL+1       BUMP TO NEXT ELEMENT                         
         AR    R5,RF                                                            
         B     PAYCHKLP                                                         
*                                                                               
PAYCHKDN DS    0H                                                               
*                                                                               
         LTR   R0,R0               IF NO ELEMENT FOUND                          
         BZ    PAYCHKOK               DO NOT CHANGE SVDYCD OR SVDYNCD           
*                                                                               
         LR    R5,R0               POINT TO APPROPRIATE ELEMENT                 
*                                                                               
PAYCHKFD DS    0H                                                               
*                                  "REPLACE" SVDY... DAYS SET FROM              
*                                  PROGPRO2+1 AND PROGPRO2+2                    
         MVC   SVDYNCD,PUBPYNCD    DAYS TO ADD TO INVOICE DATE, NO CD           
         MVC   SVDYCD,PUBPY_CD     DAYS TO ADD TO INVOICE DATE, CD              
*                                                                               
PAYCHKOK DS    0H                                                               
*                                                                               
PAYCHKX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,R5                                                            
*                                                                               
         TITLE 'PPPAY01 - CHECKING FOR TRAFFIC ID SIGN-ON'                      
*                                                                               
         DS    0D                                                               
CKTRAFID NTR1  BASE=*,LABEL=*      CHECKING FOR TRAFFIC ID SIGN-ON              
*                                                                               
         LAY   R4,PUBIO                                                         
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,10(RA)      ID NUMBER                                    
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(R4),(R4)                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,CTIDATA                                                       
CKTRA10  CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                ELEM MUST BE PRESENT                         
         CLI   0(RE),CTAGYELQ      AGENCY ALPHA ID ELEMENT (X'06')              
         BE    CKTRA20                                                          
         SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     CKTRA10                                                          
*                                                                               
CKTRA20  DS    0H                                                               
         USING CTAGYD,RE                                                        
         CLI   CTAGYIDT,CTAGYTTQ   TRAFFIC ID (C'T')?                           
         BNE   CKTRIDER                                                         
*                                                                               
CKTRIDX  DS    0H                                                               
         CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CKTRIDER LTR   RB,RB               NOT EQUAL (SIGN-ON IS NOT TRAFFIC)           
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,RE                                                            
*                                                                               
         TITLE 'PPPAY01 - SPECIAL PROCESSING FOR CCUSA INTERFACE'               
*                                                                               
         DS    0D                                                               
CCUSARTN NTR1  BASE=*,LABEL=*      CCUSA INTERFACE PROCESSING                   
*                                                                               
         CLC   BSTART(2),BEND      MUST BE SAME MONTH                           
         BNE   CCXERR                                                           
*                                                                               
         MVC   SVXFREST,BEST+1     SET LOW EST                                  
         MVC   SVXFRESX,BEST+1     AND HI EST                                   
         MVI   SVXFRSW,C'Y'        DEFAULT = XFR                                
*                                                                               
         OC    BEST,BEST           TEST EST ENTERED                             
         BZ    CC2                 NO                                           
*                                                                               
         CLC   SVXFREST,PROGPROF+11 COMPARE TO HIGH XFR EST                     
         BNH   *+8                                                              
         MVI   SVXFRSW,C'N'                                                     
         CLI   SVXFROPT,0          TEST OPTION ENTERED                          
         BE    CC10                NO                                           
         CLC   SVXFROPT,SVXFRSW    ELSE OPTION SHOULD AGREE                     
         BE    CC10                                                             
         LA    R2,PAYOPH                                                        
         NI    4(R2),X'DF'         SET INVALID                                  
         LA    R3,BADXFROP                                                      
         B     CCXERR                                                           
*                                                                               
CC2      MVI   SVXFREST,1          SET LOW EST VALUE                            
         MVC   SVXFRESX,PROGPROF+11 AND HIGH EST VALUE                          
         CLI   SVXFROPT,C'N'       TEST XFR OPTION= NO                          
         BNE   CC10                NO                                           
         MVI   SVXFRSW,C'N'        SET TO SUPPRESS XFR                          
         IC    RE,SVXFRESX         GET LAST EST FOR XFR                         
         LA    RE,1(RE)                                                         
         STC   RE,SVXFREST         AND SET AS LOW EST                           
         MVI   SVXFRESX,255        SET 255 AS HI EST                            
*                                                                               
******   MVC   BEST+1(1),SVXFREST SET LOW EST IN KEY                            
CC10     CLI   SVXFRSW,C'N'        TEST SUPPRESS XFR                            
         BE    CCX                 YES- EXIT                                    
*                                                                               
         XC    SVXFRDTA,SVXFRDTA   CLEAR ACC INTERFACE AREA                     
         LA    R4,SVXFRDTA                                                      
         USING PAYXFRD,R4                                                       
*                                                                               
         MVC   XFRCOM,SVXFRCOM     SET COMPANY                                  
****************  FOR TESTING                                                   
         CLC   T403FFD+10(2),=X'0011'  TEST ID = SJR                            
         BNE   *+8                                                              
         MVI   XFRCOM,C'9'         ******* FORCE COMPANY CODE=SJR               
****************  FOR TESTING                                                   
         MVI   XFRMD,C'P'                                                       
         MVC   XFRMD+1(1),PAYMD                                                 
         MVC   XFRCLT,PAYCL                                                     
         MVC   XFRPRD,SAVPR        MUST USE SAVED PRD                           
         GOTO1 VPUBEDIT,DMCB,BPUB,(C'Q',WORK)                                   
         CLC   AGYALPHA,=C'MC'     SPECIAL CODE FOR MC OUTDOOR                  
         BE    CC10D                                                            
*** JUST FOR TEST                                                               
         CLC   AGYALPHA,=C'SJ'     SPECIAL CODE FOR SJ OUTDOOR                  
         BNE   CC11                                                             
***                                                                             
CC10D    CLI   PAYMD,C'O'                                                       
         BNE   CC11                                                             
         MVC   WORK(8),WORK+2      DROP FIRST 2 ZEROS                           
*                                  AND PICKUP ZONE                              
         CLI   WORK+6,C' '                                                      
         BH    *+10                                                             
         MVC   WORK+6(2),=C'00'    SET ZONE TO 00 IF NOT THERE                  
*                                                                               
CC11     MVC   XFRVEH(8),WORK      ONLY BASE NUMBER                             
*                                                                               
CC12     MVC   XFRAGYNO,SVXFRAGN                                                
         MVC   XFRACN,SVID                                                      
*                                                                               
         GOTO1 VDATCON,DMCB,WKSTART,(1,XFRSTART)                                
         GOTO1 (RF),(R1),WKEND,(1,XFREND)                                       
         CLI   XFRSTART+2,0                                                     
         BNE   *+8                                                              
         MVI   XFRSTART+2,X'01'    SET DAY TO 01                                
*                                                                               
         OC    SVID,SVID           TEST ACN NUMBER ENTERED                      
         BZ    CC14                NO                                           
* MAKE SURE ACN IS 5 NUMERICS *                                                 
         LA    R0,5                                                             
         LA    R1,SVID                                                          
CC13A    CLI   0(R1),C'0'                                                       
         BL    CC13ERR                                                          
         CLI   0(R1),C'9'                                                       
         BH    CC13ERR                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,CC13A                                                         
         CLI   0(R1),C' '                                                       
         BE    CC14                                                             
CC13ERR  LA    R3,INVACN                                                        
         LA    R2,PAYOPH                                                        
         NI    4(R2),X'DF'         SET OPTIONS FIELD INVALID                    
         B     CCXERR                                                           
         SPACE 1                                                                
* CALL FASWITCH TO SWITCH TO ACC *                                              
         SPACE 1                                                                
CC14     L     RF,VCOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SVXFRACC                                                 
**************** FOR TESTING                                                    
         CLC   T403FFD+10(2),=X'0011' TEST ID=SJR                               
         BNE   *+8                                                              
         MVI   DMCB,6                FORCE ACC SYS NUMBER                       
**************** FOR TESTING                                                    
         GOTO1 (RF),DMCB                                                        
*                                                                               
         CLI   4(R1),2             TEST SYSTEM NOT OP                           
         BE    CCERR2                                                           
         CLI   4(R1),0             ALL OTHER ERRORS ARE FATAL                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VACCPAY,DMCB,=C'GET',SVXFRDTA,PAYMSGH,VCOMFACS                   
         CLI   8(R1),0             TEST ERROR                                   
         BE    CC20                                                             
         B     CCERR4                                                           
*                                                                               
CCERR2   MVC   PAYMSG(26),=C'ACC SYSTEM NOT OPERATIONAL'                        
*                                                                               
CCERR4   MVI   ERRAREA,X'FF'       INDICATE MESSAGE PRESENT                     
         LA    R2,PAYMDH           CURSOR TO MEDIA                              
         NI    PAYOPH+4,X'DF'      SET TO RE-EDIT HEADLINES                     
         B     CCX                                                              
         SPACE 1                                                                
* SWITCH BACK TO PRINT                                                          
         SPACE 1                                                                
CC20     L     RF,VCOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CCX      DS    0H                                                               
         CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CCXERR   LTR   RB,RB               NOT EQUAL (ERROR DETECTED)                   
         XIT1  REGS=(R2,R3)                                                     
*                                                                               
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE FLDIND                                                         
*                                                                               
           EJECT                                                                
       ++INCLUDE PPPAYWRK                                                       
*                                                                               
           EJECT                                                                
         PRINT OFF                                                              
GENOLD   DSECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE PAGYREC                                                        
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE PCLTREC                                                        
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE PPRDREC                                                        
         EJECT                                                                  
         ORG   PUBIO                                                            
         ORG   IOAREA                                                           
       ++INCLUDE PESTREC                                                        
         EJECT                                                                  
         ORG   PUBIO                                                            
       ++INCLUDE PUBREC                                                         
       ++INCLUDE PUBNAMEL                                                       
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE PREPREC                                                        
           EJECT                                                                
         PRINT ON                                                               
       ++INCLUDE PPPAYFFD                                                       
           EJECT                                                                
       ++INCLUDE PPPAYTWA                                                       
         PRINT OFF                                                              
*                                                                               
PUBREPD  DSECT                     PUB REP ELEMENT                              
PUBREPQ  EQU   X'14'               PUB REP ELEMENT ID                           
       ++INCLUDE PUBREPEL                                                       
PUBPAYLD DSECT                     PUB PAY ELEMENT                              
       ++INCLUDE PPGENPUBPY                                                     
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
*****PUBADREL DSECT                                                             
*****       **INCLUDE PUBAOVEL                                                  
*                                                                               
PGETADRD DSECT                                                                  
       ++INCLUDE PPGETADRD                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDOFFICED                                                      
*                                                                               
       ++INCLUDE FASECRETD                                                      
*                                                                               
       ++INCLUDE PPSRCHPARM                                                     
*                                                                               
         EJECT                                                                  
       ++INCLUDE PBYPSTEL                                                       
         EJECT                                                                  
PBILLRCD DSECT                     BILL RECORD                                  
PBILKIDQ EQU   X'08'               BILL RECORD ID                               
       ++INCLUDE PBILLREC                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE PPERREQUS                                                      
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056PPPAY01   10/14/20'                                      
         END                                                                    
