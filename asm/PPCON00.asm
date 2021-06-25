*          DATA SET PPCON00    AT LEVEL 040 AS OF 04/19/10                      
*PHASE T40D00A                                                                  
*INCLUDE PUBVAL                                                                 
*INCLUDE PPRTLOOK                                                               
*INCLUDE GETADVC                                                                
*INCLUDE SRCHCALL                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPCON00 - PRINTPAK CONTRACT BASE'                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 04/15/10 ALLOW OPEN RATE FOR ALPHA DM                                    
*                                                                               
* SMYE 11/06/03 ADD ADDRESS FOR MOVED AND ENLARGED CLTLST                       
*                                                                               
* SMYE  11/02   ADD SOME ADDRESSES FOR GLOBBER FUNCTION                         
*                                                                               
* SMYE 04/18/02 NEW LIMIT ACCESS CHANGES                                        
*                                                                               
* KWAN 10/15/01 STORE SYSRD FOR BROWSE FUNCTION                                 
*                                                                               
* KWAN 09/20/01 REORGANIZED PPCONWRK AND SAVCLTOF (CLT OFFICE CODE)             
*                                                                               
* YKAP  3/28/01  BUG IN NUMBER FIELD(KBANUM) IF NUMERIC THE LENGTH              
*                OF THE FIELD SHOULD BE LESS THAN 10                            
*                                                                               
* BPLA  5/00     NMOD EXPANDED FOR BIGGER CONTRACT AREA                         
*                WAS 1905 - CHANGED TO 2005                                     
*                CONTRAC AREA IN PPCONWRK EXPANDED                              
*                FROM 2001 TO 4001 BYTES                                        
*                                                                               
* BPLA  5/98     CHANGES FOR STORING FINACIAL OPEN RATES                        
*                                                                               
* SMYE 1/98      ADD CLIENT GROUP SECURITY                                      
*                                                                               
* SMYE 10/08/97  GETINS MADE CORE-RESIDENT                                      
*                                                                               
* BPLA 9/97      IF MASTER CLIENT SKIP READ OF                                  
*                PRODUCT FOR PRODUCT CONTRACTS                                  
*                                                                               
* BPLA 7/96      CHANGE FOR SHORTER KBAPUBN (WAS 52 NOW 51)                     
*                                                                               
* BPLA 1/96      CHANGES FOR COPYING CONTRACTS                                  
*                                                                               
* BPLA 11/22/94  DISABLE OLD AOR (DUPONT)                                       
*                                                                               
* BPLA 7/13/93   ADVCATB EXPANDED TO 2000 BYTES (100 CLTS)                      
*                                                                               
* BPLA 1/21/93   ADD FUNCTION LIMIT ACCESS                                      
*                T40DFFD+12  X'01' - NO ACCESS TO ADD/CHA                       
*                            X'02' - ACCESS TO LOCK/UNLOCK                      
*                                                                               
* BPLA 11/16/92  IF AOR CONTROL IS SET FOR NO BUYING AGY CONS                   
*                AND NO PUB LINK REQUIRED THEN READ AOR PUB                     
*                IN VALPUB                                                      
*                                                                               
* BPLA 10/14/92  NEW AOR CONTROLS TO ALLOW LOCK/UNLOCK                          
*                AND AOR CONTRACT DISPLAY (NO BUYING AGENCY                     
*                CONTRACTS ALLOWED)                                             
*                                                                               
* BPLA 12/23/91 ADVERTISER LOGIC TO READ ACROSS AGENCIES                        
*                                                                               
* BPLA 8/23/91 ADD PUB NAME SEARCHING LOGIC                                     
*                                                                   L03         
* ROSA 2/8/90 ADD CLIENT SECURITY                                   L03         
*                                                                   L03         
* SWON 1/30/90 CHECK IF CONTRACT RECORD DISPLAYED BEFORE AN ADDR,   L02         
*              ADDH,ADDL IS ALLOWED                                 L02         
*      ******* NOTE: R8 ADDED AS 2ND BASE REGISTER *******          L02         
*                                                                               
* ROSA5/16/88  ALLOW USER TO ENTER HIGHER AND LOWER LEVELS IN       L01         
*         THE CONTRACT                                              L01         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         SPACE 3                                                                
T40D00   CSECT                                                                  
         PRINT GEN                                                              
         NMOD1 WRKEND-GENOLD,T40D00,R8                                          
         PRINT NOGEN                                                            
         LA    R7,1(RC)                                                         
         LA    R7,4095(R7)                                                      
         USING GENOLD,RC,R7                                                     
         USING T40DFFD,RA                                                       
         BAS   RE,INITL                                                         
*                                                                               
         MVC   ACOMFACS,16(R1)                                                  
         MVC   ATIOB,0(R1)         A(TIOB)                                      
*                                                                               
         L     RF,ACOMFACS                                                      
         MVC   GOGLOB,CGLOBBER-COMFACSD(RF)    GET GLOBBER ADDR                 
*                                                                               
         ST    R1,SYSPARMS         SAVE PARAMETER LIST POINTER                  
*                                                                               
         ST    RB,BASERB                                                        
         ST    R8,BASER8                                                        
         ST    RD,SYSRD                                                         
*                                                                               
         RELOC RELO00                                                           
*                                                                               
*                                                                               
ACCERR   EQU   55                                                               
AACCERR  EQU   96       ACCESS TO THE FUNCTION NOT ALLOWED                      
PUBCLTER EQU   33                                                               
PUBNFERR EQU   18                                                               
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(3,TODAY)     TODAY'S DATE                    
*              STORE ADDRESS                                                    
         LA    RE,GETEL OF COMMON ROUTINES                                      
         ST    RE,VGETEL                                                        
         LA    RE,FOUTBLK                                                       
         ST    RE,VFOUTBLK                                                      
         LA    RE,MOVEREC                                                       
         ST    RE,VMOVEREC                                                      
         LA    RE,ADDELEM                                                       
         ST    RE,VADDELEM                                                      
         LA    RE,DELELEM                                                       
         ST    RE,VDELELEM                                                      
         LH    RE,=Y(PBYOWRK-GENOLD)                                            
         AR    RE,RC                                                            
         ST    RE,APBYOWRK                                                      
         LH    RE,=Y(RTLKWRK-GENOLD)                                            
         AR    RE,RC                                                            
         ST    RE,ARTLKWRK                                                      
         LH    RE,=Y(CLTLST-GENOLD)                                             
         AR    RE,RC                                                            
         ST    RE,ACLTLST                                                       
         LH    RE,=Y(PUBIO-GENOLD)                                              
         AR    RE,RC                                                            
         ST    RE,APUBIO                                                        
*                                                                               
         LH    RE,=Y(SECBLK-GENOLD)                                             
         AR    RE,RC                                                            
         ST    RE,ASECBLK                                                       
*                        GET AND STORE GETINS CORE-RESIDENT ADDRESS             
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AAB'      GET GETINS ADDRESS                   
         GOTO1 VCALLOV,DMCB                                                     
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VGETINS,DMCB                STORE GETINS ADDRESS                 
**ADV                                                                           
         LA    RE,ADVCTAB                                                       
         ST    RE,AADVCTAB                                                      
         ST    RE,ANXTACLT                                                      
**ADV                                                                           
         L     RE,=V(RATELOOK)                                                  
         A     RE,RELO00                                                        
         ST    RE,VRTLOOK                                                       
*                                                                               
*******  CALL TO FASECRET TO BUILD A SECURITY AUTHORIZATION TABLE               
*                                                                               
*  ASECBLK POINTS TO 1024 BYTES OF SAVED STORAGE                                
         L     R0,ASECBLK                                                       
         LHI   R1,1024                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR 1024 BYTE SECBLK                       
*                                                                               
         OC    4(2,RA),4(RA)       TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    6(2,RA),6(RA)       TEST ANY LIMIT ACCESS                        
         BZ    NOSECRET                                                         
*  INITIALIZE SECURITY BLOCK                                                    
         L     RF,ACOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',ASECBLK),0                                 
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
NOSECRET DS    0H                                                               
*                                                                               
         EJECT                                                                  
*              VALIDATE MEDIA                                                   
         LA    R2,KBAMEDH                                                       
         LA    R3,13               MEDIA ERROR                                  
         TM    4(R2),X'20'         PREV VALID?                                  
         BO    VALCLT                                                           
         XC    SAVKKEY,SAVKKEY                                                  
         XC    SAVCLT,SAVCLT                                                    
*              CLEAR FIELDS BELOW                                               
         NI    KBACLTH+4,X'DF'                                                  
         NI    KBAPUBH+4,X'DF'                                                  
         FOUT  KBAMEDNH,SPACES,10                                               
         FOUT  KBACLTNH,SPACES,20                                               
         FOUT  KBAPUBNH,SPACES,51                                               
*              BUILD AGY-MEDIA KEY                                              
         XC    IOAREA(32),IOAREA                                                
         MVC   PAGYKAGY,AGYALPHA   AGENCY                                       
         MVC   PAGYKMED,KBAMED     MEDIA                                        
         MVI   PAGYKRCD,1          REC CODE                                     
         MVC   KEY,IOAREA                                                       
*                                                                               
         BAS   RE,HIGH             PRTDIR-AGY                                   
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         BAS   RE,GETREC                                                        
*                                                                               
*                                                                               
         FOUT  KBAMEDNH,PAGYMED,10                                              
         MVC   SAVAGYPR,PAGYPROF                                                
         OI    4(R2),X'20'         VALID MEDIA                                  
         MVC   SAVKKEY(3),PAGYKAGY SAVE AGY-MEDIA                               
         MVI   SAVKKEY+3,X'10'     REC CODE                                     
         EJECT                                                                  
*              VALIDATE CLIENT                                                  
VALCLT   LA    R2,KBACLTH                                                       
         LA    R3,14               CLT ERROR                                    
         TM    4(R2),X'20'         VALID CLIENT?                                
         BO    VALPUB                                                           
*                                                                               
         FOUT  KBACLTNH,SPACES,20                                               
         FOUT  KBAPUBNH,SPACES,51                                               
         NI    KBAPUBH+4,X'DF'                                                  
         MVC   SAVKKEY(2),AGYALPHA  MUST RESET SAVKKEY FOR NEW CLIENT           
         MVC   SAVKKEY+2(1),KBAMED                                              
         MVI   SAVKKEY+3,X'10'      SINCE AOR/DUPONT CLOBBERS IT                
         XC    SAVKKEY+4(28),SAVKKEY+4                                          
         XC    IOAREA(32),IOAREA                                                
*                                                                               
         MVC   PCLTKAGY,AGYALPHA                                                
         MVC   PCLTKMED,KBAMED                                                  
         MVI   PCLTKRCD,2          REC CODE                                     
*                                                                               
         BAS   RE,MOVE                                                          
         MVC   PCLTKCLT,WORK                                                    
         XC    SAVPRD,SAVPRD                                                    
         CLI   5(R2),4             ONLY CLT INPUT                               
         BL    VC2                                                              
         CLI   5(R2),4                                                          
         BNH   ERROR                                                            
         MVC   SAVPRD(3),12(R2)                                                 
         OC    SAVPRD,=3C' '                                                    
         CLI   11(R2),C'/'         SEE IF 3 CHAR CLT                            
         BE    VC2                 YES                                          
         MVC   SAVPRD(3),11(R2)                                                 
         OC    SAVPRD,=3C' '                                                    
         CLI   10(R2),C'/'         SEE IF 2 CHAR CLT                            
         BNE   ERROR                                                            
         MVI   PCLTKCLT+2,C' '                                                  
*                                                                               
VC2      DS    0H                                                               
         MVC   KEY,IOAREA                                                       
         BAS   RE,HIGH             PRTDIR-CLIENT                                
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
*                                                                               
         BAS   RE,GETREC           PRTFILE                                      
*                                                                               
         MVC   SAVCLTOF,PCLTOFF    SAVE CLIENT OFFICE CODE                      
*                                                                               
         OC    4(2,RA),4(RA)       TEST AGENCY FOR NEW SECURITY                 
         BNZ   VC6                                                              
         OC    6(2,RA),6(RA)       TEST ANY LIMIT ACCESS                        
         BZ    VC8                 NO RESTRICTIONS                              
*                                                                   L03         
VC6      DS    0H                                                               
         BAS   RE,PPCLIVER                                                      
         BE    VC8                 OK                                           
         LA    R3,ACCERR           SECURITY LOCKOUT                             
         B     ERROR                                                            
VC8      DS    0H                                                               
*                                                                               
         FOUT  KBACLTNH,PCLTNAME,20                                             
         MVC   SAVCLT,PCLTKCLT          SAVE ENTERED CLIENT CODE                
         MVC   SAVFIN,PCLTFIN           SAVE FINANCIAL STATUS                   
*                                                                               
         MVC   SAVKKEY+4(3),PCLTKCLT    SAVE CLIENT CODE                        
         MVC   SAVCLTPR,PCLTPROF                                                
         XC    SADVDATA,SADVDATA                                                
******                                                                          
******   CLTAGYR AND AGYAGYR USED FOR DUPONT SCHEME                             
******                                                                          
         XC    CLTAGYR(3),CLTAGYR                                               
         XC    AGYAGYR(3),AGYAGYR                                               
*                                                                               
*        OLD AOR (DUPONT) DISABLED                                              
*                                                                               
****     LA    RF,CLTAGYR          SET FOR 'SLAVE' AGY                          
****     TM    PCLTACTL,X'08'                                                   
****     BZ    *+8                                                              
****     LA    RF,AGYAGYR          SET FOR ADVERTISER                           
****     MVC   0(3,RF),PCLTAGYR                                                 
****                                                                            
****     CLI   0(RF),0                                                          
****     BE    *+10                                                             
****     MVC   SAVKAGY,0(RF)       USE AGY OF REC                               
****                                                                            
         LA    R3,135                                                           
         CLI   PCLTPROF+5,C'2'     SLAVE CLIENT                                 
         BE    ERROR                                                            
*                                                                               
**ADV                                                                           
         L     R1,ANXTACLT                                                      
         MVC   0(2,R1),=X'FFFF'                                                 
         MVI   ADVSW,0                                                          
*                                                                               
         LA    R4,PCLTREC+33                                                    
VC10     CLI   0(R4),X'15'         SEE IF ADV CLIENT                            
         BE    VC15                                                             
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             END OF REC                                   
         BE    VC50                                                             
         B     VC10                                                             
*                                                                               
VC15     DS    0H                                                               
         MVC   SADVDATA,2(R4)     SAVE ADV CLIENT DATA                          
*                                                                               
         USING PCLTADVE,R4                                                      
         CLC   PCLTAOR,AGYALPHA   SEE IF I AM THE AOR OR ADV                    
         BNE   VC50                                                             
         MVI   ADVSW,1            SET ADVSW                                     
*                                                                               
*        MUST SWITCH TO CONTROL                                                 
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVI   DMCB,X'0A'               CONTROL SYSTEM                          
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    VC20                                                             
         MVC   KBAMSG,=CL60'** CONTROL SYSTEM NOT ACTIVE **'                    
         LA    R2,KBAMEDH                                                       
         NI    KBAMEDH+4,X'DF'      UNVALIDATE MEDIA                            
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
*                                                                               
VC20     XC    WORK(20),WORK                                                    
         MVI   WORK,C'P'                                                        
         MVC   WORK+1(1),PCLTKMED                                               
         MVC   WORK+2(2),PCLTAOR                                                
         MVC   WORK+4(3),PCLTADV                                                
         GOTO1 =V(GETADVC),DMCB,WORK,AADVCTAB,VDATAMGR,RR=RELO00                
*                                                                               
         DROP  R4                                                               
*        MUST SWITCH BACK                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                                                               
VC50     DS    0H                                                               
**ADV                                                                           
         CLI   SAVPRD,0            SEE IF PRD INPUT                             
         BE    VCX                 NO                                           
*                                                                               
         CLI   SAVCLTPR+5,C'1'     SEE IF MASTER CLIENT                         
         BE    VCX                 SKIP PRODUCT READ                            
*                                                                               
VC55     DS    0H                                                               
         MVC   KEY,IOAREA          READ PRD                                     
         MVC   KEY+7(3),SAVPRD                                                  
         MVI   KEY+3,X'06'                                                      
         BAS   RE,HIGH                                                          
         LA    R3,41               PRD NOT ON FILE                              
         CLC   KEYSAVE(25),KEY                                                  
         BNE   ERROR                                                            
*                                                                               
VCX      DS    0H                                                               
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
*              VALIDATE PUBLICATION                                             
VALPUB   LA    R2,KBAPUBH                                                       
         LA    R3,18                                                            
         TM    4(R2),X'20'         PREVIOUSLY VALID PUB?                        
         BO    VALACT                                                           
*                                                                               
         XC    SPUBKILL,SPUBKILL     CLEAR PUB KILL DATE                        
*                                                                               
         NI    TWAKIND,X'3F'                                                    
*                                                                               
         FOUT  KBAPUBNH,SPACES,51                                               
**                                                                              
**       NAME SEARCH CALL                                                       
**                                                                              
         SR    R2,RA               GET DISPLACEMENT INTO TWA OF PUB             
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
         MVC   DSMEDCOD,KBAMED                                                  
*                                                                               
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'PUB'),0,RR=RELO00                       
         DROP  R3                                                               
         LA    R3,18          RESET ERROR CODE                                  
         LA    R2,KBAPUBH                                                       
*                                                                               
         L     R5,APUBIO                                                        
         USING PUBRECD,R5                                                       
         XC    PUBREC(32),PUBREC                                                
         XC    SAVKKEY+7(25),SAVKKEY+7                                          
         GOTO1 =V(PUBVAL),DMCB,(5(R2),KBAPUB),PUBKPUB,RR=RELO00                 
*                                                                               
         CLI   DMCB,X'FF'          ERROR?                                       
         BE    ERROR                                                            
*                                                                               
         GOTO1 (RF),(R1),(5(R2),KBAPUB),SAVPUB                                  
*                                                                               
         MVC   PUBKMED,KBAMED      MEDIA                                        
         MVC   PUBKAGY,AGYALPHA                                                 
         CLI   AGYAGYR,0           IF ADVERTISER                                
         BE    *+10                                                             
         MVC   PUBKAGY,AGYAGYR     USE AGY OF REC                               
         MVI   PUBKCOD,X'81'       PUB REC CODE                                 
         MVC   KEY,PUBREC                                                       
*                                                                               
         OC    SADVDATA(18),SADVDATA   SEE IF AOR SITUATION                     
         BZ    PUB50                                                            
         TM    SADVDATA+15,X'20'   NO BUYING AGENCY CONS                        
         BNO   PUB50                                                            
         TM    SADVDATA+15,X'01'   PUB LINK REQUIRED                            
         BO    PUB50                                                            
         MVC   KEY+7(2),SADVDATA   USE AOR                                      
********                                                                        
******** MUST SWITCH TO THE AOR TO READ THE PUB                                 
*******                                                                         
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SADVDATA+14         AOR SE NUMBER                        
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    PUB50                                                            
         MVC   KBAMSG,=CL60'** AOR SYSTEM NOT ACTIVE **'                        
         LA    R2,KBAMEDH                                                       
         NI    KBAMEDH+4,X'DF'      UNVALIDATE MEDIA                            
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
*                                                                               
PUB50    DS    0H                                                               
         BAS   RE,HIGHPUB          PUBDIR                                       
         CLC   KEY(25),KEYSAVE                                                  
         BE    PUB100                                                           
*                                                                               
*              TRY ZZ PUBREC                                                    
         CLI   SAVAGYPR+16,C'0'    TEST IF SRDS DEFAULT ALLOWED                 
         BE    PUB50ER                                                          
         MVC   PUBKAGY,=C'ZZ'                                                   
         MVC   KEY,PUBREC                                                       
         BAS   RE,HIGHPUB          PUBDIR                                       
         CLC   KEY(25),KEYSAVE                                                  
         BNE   PUB50ER                                                          
         B     PUB100                                                           
*                                                                               
PUB50ER  DS    0H                     PUB NOT FOUND                             
*                                                                               
         OC    SADVDATA(18),SADVDATA   SEE IF AOR SITUATION                     
         BZ    PUB50E5                                                          
         TM    SADVDATA+15,X'20'   NO BUYING AGENCY CONS                        
         BNO   PUB50E5                                                          
         TM    SADVDATA+15,X'01'   PUB LINK REQUIRED                            
         BO    PUB50E5                                                          
*                                                                               
*        MUST SWITCH BACK                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PUB50E5  B     ERROR                                                            
*                                                                               
PUB100   BAS   RE,GETPUB                                                        
*                                                                               
         MVC   SPUBKILL,PUBKILL       SAVE PUB KILL DATE                        
*                                                                               
         OC    SADVDATA(18),SADVDATA   SEE IF AOR SITUATION                     
         BZ    PUB105                                                           
         TM    SADVDATA+15,X'20'   NO BUYING AGENCY CONS                        
         BNO   PUB105                                                           
         TM    SADVDATA+15,X'01'   PUB LINK REQUIRED                            
         BO    PUB105                                                           
*                                                                               
*        MUST SWITCH BACK                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PUB105   DS    0H                                                               
         FOUT  KBAPUBH                                                          
         MVC   WORK2(L'SPACES),SPACES                                           
         LA    R2,WORK2                                                         
         CLI   KBAMED,C'N'                                                      
         BE    *+12                                                             
         LA    R2,WORK2+12                                                      
         B     PUB200                                                           
         MVC   WORK2(16),PUBCITY   CITY                                         
         OC    PUBCITY,SPACES                                                   
         CLC   PUBCITY,SPACES                                                   
         BE    PUB150                                                           
*                                                                               
         GOTO1 FLOAT,DMCB,WORK2+27  FIND LAST CHAR                              
*                                                                               
         L     R2,DMCB                                                          
         MVI   0(R2),C','                                                       
PUB150   MVC   2(2,R2),PUBSTATE    STATE                                        
         LA    R2,6(R2)                                                         
PUB200   MVC   0(20,R2),PUBNAME                                                 
*                                                                               
         OC    PUBNAME,SPACES                                                   
         CLC   PUBNAME,SPACES                                                   
         BE    PUB250                                                           
         GOTO1 FLOAT,DMCB,21(R2)    PUB NAME                                    
*                                                                               
         L     R2,DMCB                                                          
PUB250   MVC   2(20,R2),PUBZNAME   PUB ZONE                                     
         CLI   KBAMED,C'N'         NEWSPAPERS?                                  
         BNE   PUB300                                                           
* MOVE NEWSPAPER NAME RIGHT 12 BYTES IF ROOM                                    
         CLC   WORK2+40(12),SPACES                                              
         BNE   PUB300                                                           
         MVC   WORK2+52(40),WORK2                                               
         MVC   WORK2(52),WORK2+40                                               
PUB300   DS    0H                                                               
         FOUT  KBAPUBNH,WORK2,51                                                
         MVC   SAVKKEY+7(6),KEY+1     SAVE PUB FIELD                            
         MVC   SAVPUBA,KEY+27      SAVE PUB DISK ADDR                           
*                                                                               
         CLI   CLTAGYR,0           IF 'SLAVE' AGY                               
         BNZ   PUB350                                                           
         TM    CLTACTL,X'20'       OR PUB/CLT VERIFICATION NEEDED               
         BZ    PUB400                                                           
*                                  LOOK FOR CLIENT ELEM                         
PUB350   DS    0H                                                               
         LA    R4,PUBREC+33                                                     
         SR    R0,R0                                                            
         LA    R2,KBAPUBH                                                       
PUB352   DS    0H                                                               
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    PUB354                                                           
         CLI   0(R4),X'14'                                                      
         BNE   PUB352                                                           
         USING PUBREPEL,R4                                                      
         CLC   PUBRPOFF,SAVKKEY+4                                               
         BNE   PUB352                                                           
*                                                                               
         CLI   CLTAGYR,0                                                        
         BE    PUB400                                                           
         MVC   PUBKPUB(6),PUBCVEN                                               
         B     PUB356                                                           
*                                                                               
PUB354   DS    0H                                                               
         TM    CLTACTL,X'20'                                                    
         BZ    PUB356                                                           
         LA    R3,PUBCLTER          PUBB NOT VALID FOR CLIENT                   
         B     ERROR                                                            
*                                                                               
PUB356   DS    0H                                                               
         MVC   PUBKAGY,CLTAGYR                                                  
         LA    R3,PUBNFERR                                                      
         MVC   KEY,PUBKEY                                                       
         BAS   RE,HIGHPUB                                                       
         CLC   KEY(25),KEYSAVE                                                  
         BE    PUB358                                                           
*                                  TRY ZZ REC                                   
         TM    CLTACTL,X'10'       TEST ALLOWED                                 
         BZ    ERROR                                                            
         MVC   PUBKAGY,=C'ZZ'                                                   
         MVC   KEY,PUBREC                                                       
         BAS   RE,HIGHPUB                                                       
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
*                                                                               
PUB358   DS    0H                                                               
         MVC   SAVKKEY+7(6),KEY+1       SAVE NEW PUB CODE                       
         MVC   SAVPUBA,KEY+27           SAVE NEW PUB DA                         
         MVI   PUBREC,0                                                         
         B     PUB450                                                           
*                                                                               
*                                                                               
PUB400   DS    0H                                                               
         OC    SADVDATA(18),SADVDATA    SEE IF AOR SITUATION                    
         BZ    PUB450                                                           
         CLC   SADVDATA(2),AGYALPHA     SEE IF I AM THE AOR                     
         BE    PUB450                                                           
         TM    SADVDATA+15,X'20'   SEE IF AOR DISPLAY/NO AGY CONTRACTS          
         BNO   PUB450         SO THEY CAN SET UP THEIR OWN CONTRACTS            
*                                                                               
PUB410   DS    0H                                                               
         LA    R2,KBAPUBH               MUST RESET FOR CURSOR                   
         MVC   SAVKAGY,SADVDATA         SET AOR IN CONTRACT KEY                 
         MVC   SAVKCLT,SADVDATA+2       USE ADV                                 
*                                                                               
         TM    SADVDATA+15,X'01'        SEE IF PUB LINK REQUIRED                
         BZ    PUB420                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'FE'                                                        
         MVC   KEY+1(1),KBAMED       MEDIA                                      
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(3),SADVDATA+2    ADV                                       
         MVC   KEY+7(2),SADVDATA      AOR                                       
         MVC   KEY+9(6),SAVKKEY+7     AGY PUB                                   
         LA    R3,PUBNFERR                                                      
         GOTO1 HIGHPUB                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BNE   ERROR                                                            
         MVC   SAVKKEY+7(6),KEY+15       AOR PUB                                
*                                                                               
PUB420   DS    0H                                                               
********                                                                        
******** MUST SWITCH TO THE AOR TO READ THE PUB                                 
*******                                                                         
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SADVDATA+14         AOR SE NUMBER                        
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    PUB425                                                           
         MVC   KBAMSG,=CL60'** AOR SYSTEM NOT ACTIVE **'                        
         LA    R2,KBAMEDH                                                       
         NI    KBAMEDH+4,X'DF'      UNVALIDATE MEDIA                            
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
*                                                                               
PUB425   DS    0H                                                               
         MVC   PUBKAGY,SADVDATA       AOR                                       
         MVC   PUBKPUB(6),SAVKKEY+7                                             
         LA    R3,PUBNFERR                                                      
         MVC   KEY,PUBKEY                                                       
         BAS   RE,HIGHPUB                                                       
*                                                                               
*        MUST SWITCH BACK                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         MVC   SAVPUBA,KEY+27         SAVE AOR PUB ADDR                         
*                                                                               
PUB450   DS    0H                                                               
         OI    KBAPUBH+4,X'20'                                                  
         DROP  R5                                                               
         EJECT                                                                  
*              ANALYZE ACTION                                                   
VALACT   LA    R2,KBAACTH                                                       
         LA    R3,12                                                            
         CLI   KBAACT,C'D'                                                      
         BE    ACT125                                                           
         LA    R3,AACCERR          ACTION LIMIT ACCESS ERROR                    
         TM    T40DFFD+12,X'01'    NO ACCESS TO ADD/CHA                         
         BNO   VALACT5                                                          
         TM    T40DFFD+12,X'02'    SEE IF ACCESS TO LOCK/UNLOCK                 
         BO    ACT250                                                           
         B     ERROR                                                            
*                                                                               
VALACT5  CLI   AGYAGYR,0           IF ADVERTISER                                
         BE    ACT090                                                           
         TM    AGYACTL,X'40'       IS CHANGE/ADD OK                             
         BNZ   ACT092              YES                                          
         B     ACT250              NO - STILL ALLOW LOCK OR UNLOCK              
*                                                                               
ACT090   DS    0H                                                               
         CLI   CLTAGYR,0           IF SLAVE AGY                                 
         BE    ACT092                                                           
         TM    CLTACTL,X'40'       IS CHANGE/ADD OK                             
         BZ    ERROR               NO                                           
ACT092   DS    0H                                                               
*                                                                               
         OC    SADVDATA(18),SADVDATA    SEE IF AOR SITUATION                    
         BZ    ACT092X                                                          
         TM    SADVDATA+15,X'20'   AOR DISPLAY/NO AGY CONS                      
         BNO   ACT092X                                                          
*                                                                               
         CLI   KBAACT,C'D'         SEE IF DISPLAY ACTIONS                       
         BE    ACT125                                                           
         TM    SADVDATA+15,X'40'    SEE IF LOCK/UNLOCK ALLOWED                  
         BNZ   ACT250                                                           
         TM    SADVDATA+15,X'20'    DISALLOW ADD/CHANGES                        
         BNZ   ERROR                                                            
*                                                                               
ACT092X  DS    0H                                                               
         CLC   KBAACT(4),=C'ADDR'  ADD EXTRA RATES                              
         BE    CALLRATB                                             L01         
         CLC   KBAACT(4),=C'ADDH'  ADD EXTRA HIGHER LEVELS          L01         
         BE    CALLRATH                                             L01         
         CLC   KBAACT(4),=C'ADDL'  ADD EXTRA LOWER LEVELS           L01         
         BE    CALLRATL                                             L01         
         CLC   KBAACT(4),=C'ADDO'  ADD OPEN RATES                   L01         
         BE    CALLRATO                                             L01         
         CLC   KBAACT(4),=C'COPY'  SPECIAL FOR TESTING COPYING                  
         BE    CALLCOPY                                                         
*                                                                               
         CLC   KBAACT(3),=C'ADD'                                                
         BNE   ACT100                                                           
         LA    R2,KBANUMH          CHK FOR INPUT IN NUMBER                      
         CLI   5(R2),0                                                          
         BE    ACT094              IF MISSING - NEXT AVAILABLE IS USED          
         LA    R3,2                INVALID INPUT                                
         BAS   RE,PACK                                                          
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         CH    R0,=H'999'          MAX NUMBER IS 999                            
         BH    ERROR                                                            
*                                  BUILD K KEY                                  
         LA    R3,52               DUPLICATE KEY ON ADD                         
         STH   R0,HALF                                                          
         MVC   PCONKEY(13),SAVKKEY                                              
         MVC   PCONNUM,HALF                                                     
         MVC   KEY,PCONKEY                                                      
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
*                                                                               
         BAS   RE,HIGH                                                          
*                                                                               
         NI    DMINBTS,X'F7'       SET OFF PASSING DELETES                      
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BE    ERROR                                                            
ACT094   NI    TWAKIND,X'3F'       NO VALID CONTRACT                            
         CLI   TWASTAT,X'FE'                                                    
         BE    CALLK                                                            
         GOTO1 VCALLOV,DMCB,KBALAST,X'D9040DFE'   CALL K SCREEN                 
         CLI   DMCB+4,X'FF'        ERROR?                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   KBAMSG(L'KBAMSG),SPACES                                          
         MVC   KBAMSG(26),=C'ENTER CONTRACT INFORMATION'                        
         MVI   TWASTAT,X'FE'                                                    
         LA    R2,CONSDTH                                                       
         B     EXIT                                                             
*              CALL CONTRACT ADD/CHA OVERLAY                                    
CALLK    GOTO1 VCALLOV,DMCB,(X'10',0),(RA)                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC)      EDIT K FIELDS                                
         CLI   ERRAREA,X'FF'                                                    
         BE    EXXMOD                                                           
         B     CALLDISP            GO TO DISPLAY                                
*                                                                               
ACT100   DS    0H                                                               
*                                                                               
ACT125   CLC   KBAACT(4),=C'DISR'          DISPLAY RATES                        
         BE    CALLRATB                                                         
         CLC   KBAACT(4),=C'DISH'          DISPLAY HIGHER LEVELS    L01         
         BE    CALLRATH                                             L01         
         CLC   KBAACT(4),=C'DISL'          DISPLAY HIGHER LEVELS    L01         
         BE    CALLRATL                                             L01         
         CLC   KBAACT(4),=C'DISO'          DISPLAY OPEN   LEVELS    L01         
         BE    CALLRATO                                             L01         
*                                                                               
ACT150   CLC   KBAACT(3),=C'CHA'                                                
         BNE   ACT200                                                           
         LA    R3,142                                                           
         TM    TWAKIND,X'80'       K DISPLAYED                                  
         BO    CALLK                                                            
         TM    TWAKIND,X'40'       RATES DISPLAYED                              
         BO    CALLRATA                                                         
         B     ERROR                                                            
*                                                                               
ACT200   CLC   KBAACT(3),=C'DIS'                                                
         BNE   ACT250                                                           
         CLI   KBAACT+3,C'B'       BUY DISPLAY?                                 
         BE    CALLMUL                                                          
         CLI   KBAACT+3,C'C'       MULTIPLE CONTRACT DISPLAY?                   
         BE    CALLMUL                                                          
*                                                                               
         B     CALLDISP                                                         
*                                                                               
ACT250   CLC   KBAACT(4),=C'LOCK'                                               
         BNE   ACT300                                                           
         LA    R3,142              DISP MUST PRECEED LOCK                       
         TM    TWAKIND,X'80'       K DISP MUST PRECEED LOCK                     
         BZ    ERROR                                                            
         TM    T40DFFD+12,X'02'    ACCESS TO LOCK/UNLOCK                        
         BO    CALLK                                                            
         TM    AGYACTL,X'08'       ONLY ADV CAN LOCK                            
         BO    CALLK                                                            
         TM    SADVDATA+15,X'40'   AOR CONTROL FOR LOCK/UNLOCK                  
         BO    CALLK                                                            
ACT220   LA    R3,12                                                            
         B     ERROR                                                            
*                                                                               
ACT300   CLC   KBAACT(6),=C'UNLOCK'                                             
         BNE   ACT220                                                           
         LA    R3,142              DISP MUST PRECEED UNLOCK                     
         TM    TWAKIND,X'80'       K DISP MUST PRECEED UNLOCK                   
         BZ    ERROR                                                            
         TM    T40DFFD+12,X'02'    ACCESS TO LOCK/UNLOCK                        
         BO    CALLK                                                            
         TM    AGYACTL,X'08'       ONLY ADV CAN UNLOCK                          
         BO    CALLK                                                            
         TM    SADVDATA+15,X'40'   AOR CONTROL FOR LOCK/UNLOCK                  
         BO    CALLK                                                            
         B     ACT220                                                           
*                                                                               
CALLDISP GOTO1 VCALLOV,DMCB,(X'20',0),(RA)   CALL K DISPLAY                     
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC)      DISPLAY K                                    
         B     EXXMOD                                                           
*                                                                               
*              GET BUY DISPLAY                                                  
CALLMUL  GOTO1 VCALLOV,DMCB,(X'30',0),(RA)                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC)                                                   
         B     EXXMOD                                                           
*             CALL RATES DISPLAY/CHANGE                                         
*                                                                               
CALLRAT  GOTO1 VCALLOV,DMCB,(X'45',0),(RA)                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC)                                                   
         B     EXXMOD                                                           
*                                                                  L01          
CALLCOPY DS    0H                                                               
         CLI   TWASTAT,X'FA'                                                    
         BE    CALLC2                                                           
         GOTO1 VCALLOV,DMCB,KBALAST,X'D9040DFA'   CALL K COPY SCREEN            
         CLI   DMCB+4,X'FF'        ERROR?                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   KBAMSG(L'KBAMSG),SPACES                                          
         MVC   KBAMSG(27),=C'ENTER CONTRACT TO BE COPIED'                       
         MVI   TWASTAT,X'FA'                                                    
         B     CALLC8                                                           
*                                                                               
CALLC2   DS    0H                                                               
         TM    TWAKIND,X'80'     MUST DISPLAY CONTRACT BEFORE COPY              
         BO    CALLC5                                                           
         B     CALLC8                                                           
*                                                                               
CALLC5   GOTO1 VCALLOV,DMCB,(X'50',0),(RA)   CALL K COPY                        
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC)                                                   
*                                                                               
*                             NOTE THAT PPCON50 WILL HAVE ADDED                 
*                             THE CONTRACT AND PUT THE NEW                      
*                             CONTRACT NUMBER IN KBANUM                         
         CLI   ERRAREA,X'FF'  CHECK FOR ERROR                                   
         BE    EXXMOD                                                           
         XC    KBAACT,KBAACT                                                    
         FOUT  KBAACTH                                                          
         XC    KBAMSG,KBAMSG                                                    
         MVC   KBAMSG(14),=C'CONTRACT ADDED'                                    
         FOUT  KBAMSGH                                                          
         LA    R2,KBAACTH            CURSOR TO ACTION                           
         B     EXIT                                                             
*                                                                               
CALLC8   GOTO1 VCALLOV,DMCB,(X'60',0),(RA)   CALL K COPY DISPLAY                
CALLC10  CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC)                                                   
         B     EXXMOD                                                           
*                                                                               
CALLRATB CLC   KBAACT(4),=C'ADDR'  IS IT AN ADD?                                
         BNE   RATB                                                             
         TM    TWAKIND,X'40'   MUST BE DISPLAYED BEFORE ADDING                  
         BO    RATB                                                             
         MVC   KBAMSG(L'KBAMSG),SPACES                                          
         MVC   KBAMSG(37),=C'RATES MUST BE DISPLAYED BEFORE ADDING'             
         B     INCONSIS                                                         
RATB     NI    TWAKIND,X'F8'   TURN OFF BITS 02 /01/ 04            L01          
         B     CALLRAT         ELEMENT 20                          L01          
*                                                                  L01          
CALLRATH CLC   KBAACT(4),=C'ADDH'  IS IT AN ADD?                   L02          
         BNE   RATH                                                L02          
         TM    TWAKIND,X'40'   MUST BE DISPLAYED BEFORE ADDING     L02          
         BO    RATH                                                L02          
         MVC   KBAMSG(L'KBAMSG),SPACES                             L02          
         MVC   KBAMSG(37),=C'RATES MUST BE DISPLAYED BEFORE ADDING'             
         B     INCONSIS                                            L02          
RATH     NI    TWAKIND,X'F8'   TURN OFF BITS 02 /01/ 04            L01          
         OI    TWAKIND,2        ELEMENT 22 (NEXT HIGHER)           L01          
         B     CALLRAT                                             L01          
*                                                                  L01          
CALLRATL CLC   KBAACT(4),=C'ADDL'  IS IT AN ADD?                   L02          
         BNE   RATL                                                L02          
         TM    TWAKIND,X'40'   MUST BE DISPLAYED BEFORE ADDING     L02          
         BO    RATL                                                L02          
         MVC   KBAMSG(L'KBAMSG),SPACES                             L02          
         MVC   KBAMSG(37),=C'RATES MUST BE DISPLAYED BEFORE ADDING'             
         B     INCONSIS                                            L02          
RATL     NI    TWAKIND,X'F8'   TURN OFF BITS 02 /01 / 04           L01          
         OI    TWAKIND,1        ELEMENT 21 (NEXT LOWER)            L01          
         B     CALLRAT                                             L01          
*                                                                  L01          
CALLRATO CLC   KBAACT(4),=C'ADDO'  IS IT AN ADD?                   L02          
         BNE   RATO                                                L02          
         TM    TWAKIND,X'40'   MUST BE DISPLAYED BEFORE ADDING     L02          
         BO    RATO                                                L02          
         MVC   KBAMSG(L'KBAMSG),SPACES                             L02          
         MVC   KBAMSG(37),=C'RATES MUST BE DISPLAYED BEFORE ADDING'             
         B     INCONSIS                                            L02          
*                                                                               
RATO     DS    0H                                                               
                                                                                
         CLC   AGYALPHA,=C'SJ'     ALLOWED FOR SJ                               
         BE    *+10                                                             
         CLC   AGYALPHA,=C'DM'     ALLOWED FOR DM                               
         BNE   ACT220              INVAILD ACTION                               
                                                                                
         CLI   SAVFIN,C'Y'    MUST BE FINANCIAL CLIENT                          
         BNE   ACT220                                                           
         NI    TWAKIND,X'F8'   TURN OFF BITS 02 /01 / 04           L01          
         OI    TWAKIND,4        ELEMENT 24 (OPEN RATES)            L01          
         B     CALLRAT                                             L01          
*                                                                  L01          
* THIS IS A CHANGE FUNCTION // ENSURE PRIOR DISPLAY WAS FOR        L01          
*   CORRECT ELEMENT (LOWER, HIGHER, OR CURRENT)                    L01          
*                                                                  L01          
CALLRATA CLI  KBAACT+3,C'L'    LOWER RATES TO BE CHANGED           L01          
         BNE  CHK4H                                                L01          
         TM   TWAKIND,1        WAS LOWER DISPLAYED                 L01          
         BO   CALLRAT                                              L01          
*                                                                  L01          
         MVC   KBAMSG(L'KBAMSG),SPACES                             L01          
         MVC   KBAMSG(30),=C'LOWER LEVELS MUST BE DISPLAYED'       L01          
INCONSIS DS    0H                                                  L01          
         OI    KBAACTH+6,OI1T                                      L01          
         FOUT  KBAACTH                                                          
         FOUT  KBAMSGH                                                          
         LA    R2,KBAACTH        CURSOR TO ACTION                  L01          
         MVI   ERRAREA,X'FF'                                       L01          
         B     EXIT                                                L01          
*                                                                  L01          
CHK4H    CLI  KBAACT+3,C'H'   HIGHER LEVEL TO BE CHANGED           L01          
         BNE  CHK4J                                                L01          
         TM   TWAKIND,2     WERE HIGHER  DISPLAYED                 L01          
         BO   CALLRAT                                              L01          
*                                                                  L01          
         MVC   KBAMSG(L'KBAMSG),SPACES                             L01          
         MVC   KBAMSG(31),=C'HIGHER LEVELS MUST BE DISPLAYED'                   
         B     INCONSIS                                            L01          
*                                                                  L01          
CHK4J    CLI  KBAACT+3,C'O'   OPEN RATES TO BE CHANGED             L01          
         BNE  CALLRAT        NOT H OR L OR O                       L01          
                                                                                
         CLC   AGYALPHA,=C'SJ'     ALLOWED FOR SJ                               
         BE    *+10                                                             
         CLC   AGYALPHA,=C'DM'     ALLOWED FOR DM                               
         BNE   ACT220              INVAILD ACTION                               
                                                                                
         CLI   SAVFIN,C'Y'    MUST BE FINANCIAL CLIENT                          
         BNE   ACT220                                                           
         TM   TWAKIND,4     WERE OPEN RATES DISPLAYED              L01          
         BO   CALLRAT                                              L01          
*                                                                  L01          
         MVC   KBAMSG(L'KBAMSG),SPACES                             L01          
         MVC   KBAMSG(28),=C'OPEN RATES MUST BE DISPLAYED'                      
         B     INCONSIS                                            L01          
*                                                                  L01          
*                                                                  L01          
         EJECT                                                                  
*              FIND LAST NON-BLANK CHARACTER                                    
*              P1 = A(POINT TO START SEARCH)                                    
*                                                                               
FLOAT    NTR1  BASE=BASERB                                                      
         L     R8,BASER8                                                        
         L     R2,0(R1)                                                         
         OI    0(R2),X'40'                                                      
         CLI   0(R2),X'40'                                                      
         BNE   *+8                                                              
         BCT   R2,*-12                                                          
         LA    R2,1(R2)                                                         
         ST    R2,0(R1)                                                         
         XIT1                                                                   
         TITLE 'DELETE ELEMENT ROUTINE'                                         
*              THIS ROUTINE DELETES ALL ELEMENTS WITH GIVEN CODE                
*              PARAMETER 1 =       BYTE  0   = ELEMENT CODE TO DELETE           
*                                  BYTES 1-3 = A(RECORD)                        
DELELEM  NTR1  BASE=BASERB                                                      
*                                                                               
         L     R8,BASER8                                                        
         L     R2,0(R1)            A(RECORD)                                    
         MVC   BYTE2,0(R1)         ELEM CODE                                    
*                                                                               
DEL100   MVC   HALF,25(R2)         REC LEN                                      
         LH    R5,HALF                                                          
         LA    R5,0(R5,R2)         REC END                                      
         BCTR  R5,R0               FOR BXLE                                     
         SR    R4,R4                                                            
*                                                                               
         LA    R3,33(R2)           FIRST ELEM                                   
         CLC   0(1,R3),BYTE2       ELEM CODE MATCH?                             
         BE    DELETEL                                                          
         IC    R4,1(R3)            ELEM LEN                                     
*                                                                               
         BXLE  R3,R4,*-14          LOOP THRU ELEMENTS                           
DELX     XIT1                                                                   
*                                                                               
DELETEL  GOTO1 VRECUP,DMCB+12,(1,(R2)),(R3),(R3)  DELETE ELEM                   
         B     DEL100                                                           
         TITLE 'ADD ELEMENT ROUTINE'                                            
*              PARAMETER 1 =       A(RECORD)                                    
*              PARAMETER 2 =       A(ELEMENT TO BE INSERTED)                    
*              ELEMENT IS ADDED IMMEDIATELY BEFORE HIGHER ELEM OR END           
ADDELEM  NTR1  BASE=BASERB                                                      
         L     R8,BASER8                                                        
         L     R2,0(R1)            A(RECORD)                                    
         L     R6,4(R1)            A(ELEMENT)                                   
         MVC   HALF,25(R2)         REC LEN                                      
         LH    R5,HALF                                                          
*                                                                               
         ZIC   R0,1(R6)            ELEM LENGTH                                  
         AR    R0,R5                                                            
         CH    R0,=H'2976'                                                      
         BNH   *+12                                                             
         MVI   0(R1),X'FF'         SET OVERFLOW ERROR                           
         B     ADDLX                                                            
         LA    R5,0(R5,R2)         REC END                                      
         BCTR  R5,R0               FOR BXLE                                     
         SR    R4,R4                                                            
*                                                                               
         LA    R3,33(R2)           FIRST ELEM                                   
*                                                                               
         CLC   0(1,R6),0(R3)       NEW ELEM CODE V REC ELEM CODE                
         BL    *+12                                                             
         IC    R4,1(R3)            ELEM LEN                                     
*                                                                               
         BXLE  R3,R4,*-14          LOOP THRU ELEMENTS                           
*                                                                               
         GOTO1 VRECUP,DMWORK+84,(1,(R2)),(R6),(R3)   ADD ELEMENT                
ADDLX    DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
       ++INCLUDE PPACCTEST                                                      
         EJECT                                                                  
*NOP*  **INCLUDE PPGRPTEST                                                      
         TITLE 'ROUTINE TO MOVE RECORD FROM 1 AREA TO ANOTHER'                  
*              PARAMETER 1 =       A(FROM RECORD AREA)                          
*              PARAMETER 2 =       A(TO RECORD AREA)                            
MOVEREC  NTR1  BASE=BASERB                                                      
*                                                                               
         L     R8,BASER8                                                        
         L     R2,0(R1)            FROM REC                                     
         L     R3,4(R1)            TO REC                                       
*                                                                               
         MVC   HALF,25(R2)         REC LEN                                      
         LH    R5,HALF                                                          
MOVE100  LTR   R5,R5                                                            
         BZ    MOVEXIT                                                          
*                                                                               
         CH    R5,=H'250'                                                       
         BNH   MOVEREST                                                         
         MVC   0(250,R3),0(R2)                                                  
         LA    R2,250(R2)                                                       
         LA    R3,250(R3)                                                       
         SH    R5,=H'250'                                                       
         B     MOVE100                                                          
MOVEREST BCTR  R5,R0                                                            
         EX    R5,MOVEVAR                                                       
MOVEXIT  L     R6,4(R1)                                                         
         MVC   HALF,25(R6)                                                      
         LH    R5,HALF                                                          
         LA    R6,0(R5,R6)                                                      
         MVI   0(R6),0                                                          
         XIT1                                                                   
MOVEVAR  MVC   0(0,R3),0(R2)                                                    
         TITLE 'ELEMENT GET ROUTINE'                                            
*        P1=A(RECORD)    BYTE 0 = ELEM CODE SOUGHT BY USER (1ST ELEM            
*                                 ONLY RETURNED) SET TO X'FF' IF NONE           
*        P2=A(3 FULL WORD AREA FOR REGISTERS 3-5 FOR BXLE IN USER)              
GETEL    NTR1  BASE=BASERB                                                      
         L     R8,BASER8                                                        
         L     R2,0(R1)            A(RECORD)                                    
         L     R6,4(R1)            A(STORE AREA)                                
*                                                                               
         SR    R4,R4                                                            
         MVC   HALF,25(R2)         REC LEN                                      
         LH    R5,HALF                                                          
         LA    R5,0(R2,R5)         REC END                                      
         BCTR  R5,R0               BXLE                                         
*                                                                               
         LA    R3,33(R2)           FIRST ELEM                                   
*                                                                               
GETEL100 CLC   0(1,R1),0(R3)       ELEM CODE?                                   
         BNE   GETEL200                                                         
         STM   R3,R5,0(R6)         BXLE FOR USER                                
GETELXIT XIT1                                                                   
GETEL200 IC    R4,1(R3)            ELEM LEN                                     
         BXLE  R3,R4,GETEL100                                                   
         MVI   0(R1),X'FF'         NOT FOUND INDICATOR                          
         B     GETELXIT                                                         
         TITLE 'FOUT BLANKS TO UNPROTECTED FIELDS'                              
*              PARAMETER 1 =       A(FIRST FIELD)                               
*                        2 =       A(END-ADDR) EX=BUYLAST                       
*                                                                               
FOUTBLK  NTR1  BASE=BASERB                                                      
         L     R8,BASER8                                                        
         L     R2,0(R1)            1ST FIELD                                    
         L     R3,4(R1)            LAST FIELD                                   
*                                                                               
         SR    RE,RE                                                            
         MVI   WORK2,C' '                                                       
         MVC   WORK2+1(L'WORK2-1),WORK2                                         
         LA    R6,CONSTDH          STANDARD COMMENT                             
         LA    RF,RATLI1H                                                       
         CR    R2,RF               SEE IF CLEARING RATE SCREEN                  
         BNE   *+8                                                              
         LA    R6,RATLI1H          SET R6 TO RATLI1H                            
FOUT100  IC    RE,0(R2)            LEN                                          
         TM    1(R2),X'20'         PROTECTED?                                   
         BO    FOUT200                                                          
*                                                                               
         LR    RF,RE                                                            
         SH    RF,=H'8'            DATA LENGTH                                  
         CR    R2,R6               STANDARD COMMENT YET?                        
         BL    FOUT150                                                          
         LR    R5,RF                                                            
         BCTR  R5,R0                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),WORK2       ALREADY SPACES                               
         BE    FOUT200                                                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         OC    8(0,R2),8(R2)                                                    
         BZ    FOUT200                                                          
FOUT150  DS    0H                                                               
         FOUT (R2),WORK2,(RF)                                                   
*                                                                               
FOUT200  LA    R2,0(RE,R2)         NEXT FIELD                                   
         CR    R2,R3               LAST?                                        
         BL    FOUT100                                                          
         XIT1                                                                   
*                                                                               
       ++INCLUDE PPGENEROL         IN-LINE CODES                                
*                                                                               
         LTORG                                                                  
SPACES   DC    CL70' '                                                          
PATCH    DS    CL30                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
       ++INCLUDE PPCONWRK                                                       
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE PPSRCHPARM                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE PGENGRP                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040PPCON00   04/19/10'                                      
         END                                                                    
