*          DATA SET ACREQ00    AT LEVEL 094 AS OF 02/17/21                      
*PHASE T60400B                                                                  
*INCLUDE TWABLD                                                                 
*INCLUDE BMONVAL                                                                
*&&UK                                                                           
*INCLUDE FASECURE                                                               
*&&                                                                             
*----------------------------------------------------------------------         
* VGUP 089 31JUL19 SPEC-37489 Added new VE report                               
* GHOA 090 02AUG19 SPEC-30973 SUPPORT DOLLAR TOLERANCE                          
*                  SPEC-20441 NOT WORKING IN CANADA (SPOT & PRINT)              
* JSHA 091 02AUG19 SPEC-41535 Check Delivery Flag                               
* GHOA 092 22OCT20 SPEC-51025 1099 new requirements for 2020                    
* GHOA 093 14JAN21 SPEC-51025 Allow T7,D for =REQ                               
* GHOA 094 21JAN21 SPEC-53267 User dumps requesting T7                          
*                  SPEC-41535 Check Flag description change                     
*----------------------------------------------------------------------         
         TITLE 'ACREQ00 - REQUEST - ROOT CONTROLLER'                            
T60400   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 GWSX-GWS,T60400,RR=R5,CLEAR=YES                                  
         LR    R9,RC                                                            
         USING GWS,R9              R9=A(GLOBAL WORKING STORAGE)                 
         LA    R8,RCARDS                                                        
         USING ACQD,R8             R8=A(REQUEST CARD BUFFER)                    
         ST    R5,RELOC                                                         
*                                                                               
         USING TWAD,R3             R3=A(TWA/SAVED STORAGE)                      
         L     R3,4(R1)                                                         
         ST    R1,APARM            SAVE A(SYSTEM PARAMETERS)                    
         ST    R3,ASAVE            A(TWA)                                       
         ST    RB,ABASE            ROOT BASE REGISTER                           
         ST    R9,ATEMP            A(TEMP. WORKING STORAGE)                     
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         L     R5,0(R1)                                                         
         LR    R6,R3               STORE A(LAST INPUT FLD)                      
         AH    R6,2(R5)                                                         
         ST    R6,ALASTF                                                        
         MVC   COUNTF,4(R5)        STORE INPUT FLD COUNT                        
         SR    R0,R0                                                            
         IC    R0,TIOBAID-TIOBD(R5)                                             
         CLI   TIOBAID-TIOBD(R5),12      FORCE RANGE 1-12                       
         BNH   *+8                                                              
         SH    R0,=H'12'                                                        
         STC   R0,PFKEY            SAVE PFKEY NUMBER                            
*                                                                               
         MVI   DDS,0                                                            
         CLI   TWAOFFC,C'*'        test DDS TERMINAL                            
         BNE   *+8                                                              
         MVI   DDS,1                                                            
         MVC   CMPY,0(R1)          STORE COMPANY                                
         L     RF,20(R1)           A(EXTRA INFO BLOCK)                          
         MVC   AGYCTRY,1(RF)       AGENCY COUNTRY                               
         CLI   AGYCTRY,0           DEFAULT COUNTRY                              
         BNE   MAIN09                                                           
         MVI   AGYCTRY,CTRYUSA                                                  
*                                                                               
MAIN09   MVC   USRID,10(R3)        STORE USER ID NUMBER                         
         MVI   USRIDF,0                                                         
*                                                                               
         USING COMFACSD,R5                                                      
         L     R5,16(R1)                                                        
         ST    R5,ACOMFACS                                                      
         MVC   FACLIST,0(R5)       STORE COMMON FACILITY LIST                   
         GOTO1 GETFACT,PLIST,0                                                  
         MVC   SECRET,CSECRET      SAVE ADDRESS TO SECRET                       
         DROP  R5                                                               
*                                                                               
         L     R1,0(R1)                                                         
         MVC   TODAY(13),4(R1)                                                  
         GOTO1 DATVAL,PLIST,TODAY,TEMP                                          
         MVC   TODAY(6),TEMP       CONVERT TO YYMMDD                            
         GOTO1 CALLOV,DMCB,0,X'E3000A62',0                                      
         MVC   OFFAL,0(R1)                                                      
*                                                                               
         GOTO1 (RF),(R1),0,X'D9000AFA'                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   GETIDS,0(R1)        GET GETIDS ADDRESS - CORERES                 
*                                                                               
         LA    RF,BVRNUMH          FIND 1ST MODIFIED FIELD                      
         ST    RF,AFIRSTF                                                       
MAIN10   SR    R1,R1               0 = END OF SCREEN                            
         ICM   R1,1,0(RF)                                                       
         BZ    MAIN20                                                           
         TM    4(RF),X'80'         IF FIELD WAS INPUT THIS TIME THEN            
         BO    *+10                SAVE ADDRESS OF FIELD                        
         AR    RF,R1                                                            
         B     MAIN10                                                           
         ST    RF,AFIRSTF                                                       
*                                                                               
MAIN20   LA    RF,OLAY             RELOCATE LOCAL ROUTINES                      
         ST    RF,AOLAY                                                         
         LA    RF,IOREAD                                                        
         ST    RF,AIOREAD                                                       
         LA    RF,INITV                                                         
         ST    RF,AINITV                                                        
         LA    RF,RJN                                                           
         ST    RF,ARJN                                                          
         LA    RF,RFPINIT                                                       
         ST    RF,ARFP                                                          
         LA    RF,SETREQ                                                        
         ST    RF,ASETREQ                                                       
         LA    RF,GRPVAL                                                        
         ST    RF,AGRPVAL                                                       
         LA    RF,CHKACC           CHECK USER ACCESS TO UPDATE THE FILE         
         ST    RF,ACHKACC                                                       
         LA    RF,SECBLK           SECRET BLOCK                                 
         ST    RF,ASECBLK                                                       
*                                  RELOCATE TABLES                              
         LA    R1,ADCONS                                                        
         LA    RF,ATYPES                                                        
         LA    R0,NADCONS                                                       
MAIN30   L     RE,0(R1)                                                         
         A     RE,RELOC                                                         
         ST    RE,0(RF)                                                         
         LA    R1,4(R1)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,MAIN30                                                        
*                                                                               
         LA    R1,OFFBLK                                                        
         ST    R1,AOFFBLK                                                       
         LA    R1,REQOFFL                                                       
         ST    R1,AREQOFFL                                                      
         ST    RB,LOLAYNUM         INIT LAST OLAY NUM & ADR                     
         LA    R6,ACCWORK                                                       
         ST    R6,DMCB+16                                                       
         MVC   DMCB+16(1),0(R3)    SET TERMINAL NUM IN DMCB                     
         OI    BVRSRVH+1,X'01'     MODIFIED                                     
         OI    BVRSRVH+6,X'80'                                                  
*                                                                               
         LA    RF,IO1                                                           
         ST    RF,AIO1             SET A(IO AREA #1)                            
         AH    RF,=Y(L'IO1)                                                     
         ST    RF,AIO2             SET A(IO AREA #2)                            
         AH    RF,=Y(L'IO2)                                                     
         ST    RF,AIORFP           SET A(RFP IO AREA)                           
         AH    RF,=Y(L'IORFP)                                                   
         ST    RF,AIOMINIO         SET A(MINIO IO AREA)                         
         AH    RF,=Y(L'IOMINIO)                                                 
         ST    RF,ARFPTAB          SET A(RFP TABLE AREA)                        
*                                                                               
* SEE IF SECURITY IS NEEDED ON PRG-IF NOT IN TABLE-NO SECURITY                  
*                                                                               
         USING PRGTABD,R4                                                       
         L     R4,APRGTAB          PROGRAM/ACTION TABLE                         
         LA    R0,PRGTBNUM                                                      
MAIN40   CLC   PRGID,BVRNUM        FIND PRG ID IN TABLE                         
         BE    MAIN50                                                           
         CLC   PRGCDE,BVRNUM       FIND PRG CODE IN TABLE                       
         BE    MAIN50                                                           
         LA    R4,PRGTBLNQ(R4)                                                  
         BCT   R0,MAIN40                                                        
         B     MAIN70              NOT IN TABLE-NO SECURITY                     
*                                                                               
MAIN50   XC    DMCB(24),DMCB       INIT SECRET                                  
         GOTO1 SECRET,DMCB,('SECPINIT',ASECBLK)                                 
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
         LA    R5,PRGANUM          R5=A(ACTION)                                 
         ICM   R5,8,PRGRNUM        PASS RECORD NUMBER IN HIGH BIT               
         GOTO1 SECRET,DMCB,('SECPRACT',ASECBLK),(R5)                            
         CLI   DMCB,SECPYES                                                     
         BNE   MAIN60                                                           
*                                                                               
         TM    PRGRSTAT,PRGNOACC   CHECK THEIR ACCESS                           
         BNO   MAIN55                                                           
         MVC   FERN,=AL2(FF)       SET ERROR FLAG AS OK                         
         GOTO1 ACHKACC             CHECK IF THEY HAVE ACCESS                    
         CLC   FERN,=AL2(FF)                                                    
         BNE   OERRMSG                                                          
*                                                                               
MAIN55   TM    PRGRSTAT,PRGSECL    SHOULD WE LOOK INTO FIELD SEC                
         BNO   MAIN70                                                           
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVI   BYTE,SALARYQ                CHECK ACCESS TO VIEW SALARY          
         GOTO1 SECRET,DMCB,('SECPFLDP',ASECBLK),BYTE                            
         CLI   DMCB,SECPNO                                                      
         BNE   MAIN70                                                           
*                                                                               
MAIN60   LA    RF,BVRNAMEH                                                      
         ST    RF,FADR                                                          
         MVC   FERN,=AL2(SECLOCK)                                               
         B     OERRMSG                                                          
         DROP  R4                                                               
*                                                                               
MAIN70   CLI   PFKEY,1             MENU PFKEY PRESSED?                          
         BNE   RFP                                                              
         MVC   BVRNAME,SPACES                                                   
         MVC   BVRNAME(4),=C'MENU'                                              
         OI    BVRNAMEH+1,X'01'                                                 
         MVI   BVRNAMEH+5,4                                                     
         OI    BVRNAMEH+6,X'80'                                                 
         MVC   BVRNUM,SPACES                                                    
         OI    BVRNUMH+6,X'80'                                                  
         MVI   STATUS,0                                                         
         MVI   FIND,0                                                           
         EJECT                                                                  
***********************************************************************         
*              INTERFACE TO $RFP                                      *         
***********************************************************************         
*                                                                               
RFP      DS    0H                                                               
         MVI   RFPSTAT,0           DESTINATION MUST BE FILE                     
         ZIC   R1,BVRDESTH+5                                                    
         SH    R1,=H'1'                                                         
         BM    RFPX                                                             
         EXCLC R1,BVRDEST,=CL15'FILE'                                           
         BNE   RFPX                                                             
*                                                                               
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPINIT   INITIALIZE $RFP INTERFACE                    
         GOTO1 ARFP,DMCB                                                        
*                                                                               
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         ZIC   R1,BVROUTH+5                                                     
         SH    R1,=H'1'                                                         
         BM    RFPX                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   QRFPWORK(0),BVROUT  VALIDATE GROUP NAME                          
         OC    QRFPWORK,SPACES                                                  
         MVI   QRFPMODE,QRFPGVAL                                                
         MVI   RFPSTAT,RFPINUSE                                                 
         GOTO1 ARFP,DMCB                                                        
         CLI   QRFPMODE,QRFPOK                                                  
         BE    RFPX                                                             
         MVC   FERN,=AL2(IGROUPNM)                                              
         LA    R7,BVROUTH                                                       
         ST    R7,FADR                                                          
         B     OERRMSG                                                          
*                                                                               
RFPX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*   SECURITY CHECK                                                              
***********************************************************************         
*&&UK                                                                           
SEC      CLI   BVRSECH+5,0                                                      
         BE    SECX                                                             
         LA    R2,BVRSECH                                                       
         ST    R2,FADR                                                          
         GOTO1 ASECURE,DMCB,(R2),ASECBLK,ACOMFACS,RTNTYP                        
         OC    PIDN,PIDN                                                        
         BNZ   SECX                                                             
         LA    RF,BVRSECH                                                       
         ST    RF,FADR                                                          
         MVC   FERN,=AL2(INVINPT)  INVALID INPUT                                
         B     OERRMSG                                                          
*                                                                               
SECX     DS    0H                                                               
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
*              STACKED REQUESTS                                       *         
***********************************************************************         
*                                                                               
VALCHK   DS    0H                                                               
         MVI   OUTSTAT,0           CLEAR OUTPUT STATUS BYTE                     
         CLC   BVRNUM(2),=C'55'    STACKED/55 SOON ONLY VALID FOR CKS           
         BNE   VALCHKX                                                          
         TM    RFPSTAT,RFPINUSE                                                 
         BO    VALCHKER            CANT RFP CHECKS                              
*                                                                               
         CLI   BVROUTH+5,0                                                      
         BE    VALCHKX                                                          
         MVC   TEMP(80),SPACES                                                  
         ZIC   R1,BVROUTH+5                                                     
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMP(0),BVROUT      CHECK OUTPUT TYPE                            
         OC    TEMP(80),SPACES                                                  
*                                                                               
         CLC   TEMP(6),=C'STACK '  STACK REQUEST                                
         BNE   VALCHK10                                                         
         OI    OUTSTAT,STCKQ                                                    
         MVC   TEMP(80),SPACES                                                  
         ZIC   R1,BVRNUMH+5                                                     
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMP(0),BVRNUM                                                   
         CLC   TEMP(5),=C'55,D '                                                
         BNE   VALCHKX                                                          
         MVI   OUTSTAT,DISPQ       DISPLAY REQUESTS                             
         MVI   OLAYNUM,5                                                        
         GOTO1 AOLAY                                                            
         XC    PREQNDX1,PREQNDX1                                                
         B     OERRMSG                                                          
*                                                                               
VALCHK10 DS    0H                                                               
         CLC   BVROUT(4),=C'RUN,'  RUN THE STACKED REQUESTS                     
         BNE   VALCHK15                                                         
         OI    OUTSTAT,RUNQ                                                     
         MVI   DISPMODE,0                                                       
         XC    PREQNDX1,PREQNDX1                                                
         B     VALCHK20                                                         
*                                                                               
VALCHK15 CLC   BVROUT(5),=C'SOON,' SOON REQUESTS                                
         BNE   VALCHKX                                                          
         OI    OUTSTAT,SOONQ                                                    
         MVI   DISPMODE,0                                                       
         XC    PREQNDX1,PREQNDX1                                                
*                                                                               
VALCHK20 CLI   BVRNUMH+5,2                                                      
         BE    VALCHKX                                                          
VALCHKER MVC   FERN,=AL2(INVINPT)  INVALID INPUT                                
         LA    R7,BVRNUMH                                                       
         ST    R7,FADR                                                          
         B     OERRMSG                                                          
*                                                                               
VALCHKX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
CONTROL  CLI   STATUS,0                                                         
         BE    STATUS0                                                          
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVC   OFFACOMF,ACOMFACS                                                
         MVC   OFFAALPH,TWAAGY                                                  
         MVC   OFFACPY,CMPY                                                     
         MVC   OFFACST1(OFFAOPOS-OFFACST1),COMPSTA1                             
         MVC   OFFALIMA,TWAACCS                                                 
         MVC   OFFAAUTH,TWAAUTH                                                 
         MVI   OFFAINDS,OFFAIOFF                                                
         MVI   OFFAACT,OFFAINI                                                  
         OC    OFFASAV(OFFASAVL),SAVEOFFA                                       
         BZ    *+8                                                              
         MVI   OFFAACT,OFFARES                                                  
         GOTO1 OFFAL                                                            
         BE    CONTROL1                                                         
         LA    RF,BVRNAMEH                                                      
         ST    RF,FADR                                                          
         MVC   FERN,=AL2(SECLOCK)                                               
         B     OERRMSG                                                          
*                                                                               
CONTROL1 MVC   SAVEOFFA,OFFASAV                                                 
         DROP  R1                                                               
*                                                                               
         CLI   STATUS,1                                                         
         BE    STATUS1                                                          
         CLI   STATUS,2                                                         
         BE    STATUS2                                                          
         CLI   STATUS,3                                                         
         BE    STATUS3                                                          
         CLI   STATUS,4                                                         
         BE    STATUS4                                                          
         DC    H'0'                                                             
*                                                                               
STATUS0  DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,TODAY2),(2,TODAY2)                                
         MVI   OLAYNUM,01          INPUT IS REQ DEFN                            
         GOTO1 AOLAY               VALIDATE & BUILD SCREEN                      
         CLC   FERN,=AL2(FF)                                                    
         BL    OERRMSG                                                          
         B     CONTROL                                                          
*                                                                               
STATUS1  CLI   REQACTN,C'D'                                                     
         BE    STATUS1A                                                         
         XC    BVRHDR,BVRHDR                                                    
         OC    OUTSTAT,OUTSTAT                                                  
         BNZ   STATUS1X                                                         
         CLI   REQACTN,C'A'                                                     
         BNE   *+14                                                             
         MVC   BVRHDR(23),=C'Enter request amendment'                           
         B     STATUS1A                                                         
STATUS1X CLI   REQACTN,C'N'                                                     
         BNE   *+14                                                             
         MVC   BVRHDR(18),=C'Enter request data'                                
         B     STATUS1A                                                         
*                                                                               
STATUS1A MVI   STATUS,2                                                         
         LA    R5,BVRFRSTH         POSN CURSOR                                  
         SR    R6,R6                                                            
         TM    1(R5),X'20'                                                      
         BZ    *+14                                                             
         IC    R6,0(R5)                                                         
         AR    R5,R6                                                            
         B     *-14                                                             
         ST    R5,FADR                                                          
         B     OHDR                                                             
*                                                                               
STATUS2  DS    0H                                                               
         MVC   REQNUM(16),LREQNUM  INPUT IS REQUEST DATA                        
         MVC   KEY,LKEY                                                         
*                                                                               
         LA    R0,LREQREC                                                       
         SR    R1,R1                                                            
         ICM   R1,3,=Y(L'REQREC)                                                
         LR    RF,R1                                                            
         LA    RE,REQREC                                                        
         MVCL  RE,R0                                                            
*                                                                               
         LA    R6,BVRFRSTH                                                      
         C     R6,AFIRSTF          WAS A REQ DEFN FLD INPUT                     
         BL    *+12                NO                                           
         MVI   STATUS,0            YES BACK TO STATUS 0                         
         B     CONTROL                                                          
         CLI   REQACTN,C'D'                                                     
         BE    UPDATE                                                           
         B     VALIDATE                                                         
*                                                                               
STATUS3  B     UPDATE              DISPLAY/UPDATE DATA                          
*                                                                               
STATUS4  MVC   BVRHDR(22),=C'Request menu displayed'                            
         B     UPDATE1                                                          
         EJECT                                                                  
***********************************************************************         
*              VALIDATE REQUEST DATA                                  *         
***********************************************************************         
*                                                                               
         USING RQTBD,R4                                                         
VALIDATE L     R4,AREQTBL                                                       
         AH    R4,REQNDX1           R4=A(REQ TBL FLD ENTRY)                     
         AHI   R4,(HDRFLDS-HDRQLEN) R4=A(FIRST FIELD ENTRY)                     
         SHI   R4,RQTBLNQ           backup length of one entry                  
         LA    RA,LREQMAP-3         RA=A(REQ MAP TBL ENTRY)                     
         B     VAL110                                                           
*                                                                               
VAL10    CLI   RQFLD,0             END OF REQ TBL                               
         BE    VALX                YES                                          
         CLI   RQFLD,LREQMAPX                                                   
         BE    VALX                                                             
*                                                                               
         TM    RQSTAT1,FDDSO       DDS-ONLY FIELD                               
         BZ    *+12                                                             
         CLI   DDS,1               SKIP IF THIS IS NOT DDS                      
         BNE   VAL120                                                           
*                                                                               
         TM    RQSTAT2,FCANO       DISPLAY ON CANADIAN SCREENS ONLY?            
         BZ    *+12                                                             
         CLI   AGYCTRY,CTRYCAN                                                  
         BNE   VAL120                                                           
*                                                                               
         TM    RQSTAT3,FUSAO       DISPLAY ON USA SCREENS ONLY?                 
         BZ    *+12                                                             
         CLI   AGYCTRY,CTRYUSA                                                  
         BNE   VAL120                                                           
*                                                                               
         TM    RQSTAT2,FAPGS       DISPLAY IF APG SECURITY ACTIVE?              
         BZ    *+12                                                             
         TM    COMPSTA5,CPYAPGS                                                 
         BZ    VAL120                                                           
*                                                                               
         TM    RQSTAT2,FNBLO       DISPLAY FOR NEW BILLING                      
         BZ    *+12                                                             
         TM    COMPSTAA,CPYSADBL                                                
         BZ    VAL120                                                           
*                                                                               
* Per PSHA - do not show the SALARY(Y/N) field until further notice             
* Field Control will control whether Salary is viewed or not and not            
* the SALARY(Y/N) field.  Everything from begin temp to end temp can            
* go after the field is valid again.                                            
*                                                                               
         TM    RQSTAT2,FNSEC       CHECK IF NEW SECURITY                        
         BZ    VAL20                                                            
*                                                                               
* BEGIN TEMPORARY AREA                                                          
*                                                                               
         L     R6,AFLDNUMT         FIND REQ FLD NUM IN TBL                      
         USING FLDD,R6                                                          
VAL11    CLI   0(R6),0             R6=A(FLD NUM TBL ENTRY)                      
         BNE   *+6                                                              
         DC    H'0'                DUMP IF REQ NUM NOT IN TBL                   
         CLC   FLDNUM,RQFLD                                                     
         BE    VAL12                                                            
         LA    R6,FLDLNQ(R6)                                                    
         B     VAL11                                                            
*                                                                               
VAL12    MVC   OLAYNUM,FLDVOVR     SET OVERLAY REQD                             
         MVC   ROUTNUM,FLDVNUM     SET VALIDATION ROUT REQD                     
         ST    R5,FLDHADR          SET INPUT FLD HDR ADR                        
         GOTO1 AOLAY               PASS CONTROL TO OVERLAY                      
         B     VAL120              SKIP FOR NOW                                 
*                                                                               
* END TEMPORARY AREA                                                            
*                                                                               
         TM    CMPSTAT,CMPNSWT+CMPNSRD  IS EITHER READ/WRITE ON?                
         BZ    VAL120                   NO - SKIP IT                            
*                                                                               
VAL20    TM    RQSTAT2,FNACO       DISPLAY ONLY IF ON NEW OFFICES AND           
         BZ    *+12                ON AN EMULATED ACCFILE                       
         TM    CMPSTAT,CMPNOFF+CMPEMUL                                          
         BNO   VAL120                                                           
*                                                                               
         TM    RQSTAT1,FDPSL       DPS W/O LIMIT ACCESS                         
         BZ    VAL30                                                            
         CLC   CMPY,DPS            SKIP IF THIS IS NOT DPS                      
         BNE   VAL120                                                           
         CLI   TWAACCS,C'T'        OR IF COMMERCIAL UNIT PRESENT                
         BE    VAL120                                                           
*                                                                               
VAL30    CLC   0(1,RA),RQFLD                                                    
         BE    *+6                                                              
         DC    H'0'                DUMP IF TBLS OUT OF STEP                     
*                                                                               
         L     R6,AFLDNUMT         FIND REQ FLD NUM IN TBL                      
VAL40    CLI   0(R6),0             R6=A(FLD NUM TBL ENTRY)                      
         BNE   *+6                                                              
         DC    H'0'                DUMP IF REQ NUM NOT IN TBL                   
         CLC   FLDNUM,RQFLD                                                     
         BE    VAL50                                                            
         LA    R6,FLDLNQ(R6)                                                    
         B     VAL40                                                            
*                                                                               
VAL50    MVC   OLAYNUM,FLDVOVR     SET OVERLAY REQD                             
         MVC   ROUTNUM,FLDVNUM     SET VALIDATION ROUT REQD                     
         ST    R5,FLDHADR          SET INPUT FLD HDR ADR                        
         GOTO1 AOLAY               PASS CONTROL TO OVERLAY                      
         DROP  R6                                                               
*                                                                               
         TM    COMPSTA5,CPYAPGS    USING APG $ SECURITY                         
         BZ    VAL70                                                            
         TM    RQSTAT2,FAPGS                                                    
         BZ    VAL70                                                            
         TM    TWAAUTH,X'20'       IF AUTHORIZED THEN FIELD IS REQUIRED         
         BZ    VAL60                                                            
         TM    FIND,FIINP                                                       
         BO    VAL70                                                            
         MVC   FERN,=AL2(FLDMIS)                                                
         B     OERRMSG                                                          
VAL60    TM    FIND,FIINP                                                       
         BZ    VAL70                                                            
         MVC   FERN,=AL2(SECLOCK)                                               
         B     OERRMSG                                                          
*                                                                               
VAL70    CLC   BVROUT(4),=C'SOON'                                               
         BNE   VAL80                                                            
         TM    RQSTAT1,FSOON       REQUIRED ON SOON FIELD?                      
         BZ    *+12                                                             
         TM    FIND,FIINP          INPUT ENTERED?                               
         BZ    OERRMSG                                                          
         TM    RQSTAT2,FNALL       'ALL' NOT ALLOWED ON 'SOON' REQUESTS         
         BZ    VAL80                                                            
         TM    FIND,FIALL                                                       
         BZ    VAL80                                                            
         MVC   FERN,=AL2(INVINPT)                                               
         B     OERRMSG                                                          
*                                                                               
VAL80    TM    FIND,FIINP                                                       
         BZ    VAL90                                                            
         CLC   FERN,=AL2(FF)                                                    
         BNE   OERRMSG             FLD INPUT INVALID                            
         B     VAL100              FLD INPUT VALID                              
VAL90    TM    RQSTAT1,FOPT        FIELD OPTIONAL                               
         BZ    OERRMSG                                                          
         B     VAL110                                                           
*                                                                               
VAL100   NI    FIND,X'FE'          FIND=B'XXXXXXX0'                             
         MVC   TEMP(1),RQSTAT1     TEMP=B'XXXXXXXX'                             
         NC    TEMP(1),FIND        IS FLD FORMAT OK FOR REQ                     
         BNZ   VAL110              YES                                          
*        TM    RFPSTAT,RFPINUSE                                                 
*        BO    VAL110                                                           
         MVC   FERN,=AL2(FRMNVAL)  FORMAT NOT VALID                             
         B     OERRMSG                                                          
*                                                                               
VAL110   LA    RA,3(RA)            FIND NEXT REQ MAP ENTRY                      
         MVC   HALF,1(RA)                                                       
         LH    R5,HALF                                                          
         AR    R5,R3               R5=A(NEXT UNPROT FLD HDR)                    
         CLI   0(RA),126           CARD REQUEST FORMAT                          
         BNE   VAL120                                                           
         MVC   ACQPROG+2(78),8(R5) MOVE SCREEN TO REQUEST RECORD                
         B     UPDATE                                                           
*                                                                               
VAL120   LA    R4,RQTBLNQ(R4)      FIND NEXT REQ TBL FLD                        
         B     VAL10                                                            
*                                                                               
VALX     CLI   0(RA),LREQMAPX      END OF REQ MAP TBL                           
         BE    UPDATE                                                           
         DC    H'0'                DUMP IF TBLS OUT OF STEP                     
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION                                        *         
***********************************************************************         
*                                                                               
UPDATE   DS    0H                                                               
         MVI   OLAYNUM,2           PASS CONTROL TO OVERLAY                      
         GOTO1 AOLAY                                                            
         CLC   FERN,=AL2(FF)                                                    
         BL    OERRMSG                                                          
*                                                                               
UPDATE0  TM    OUTSTAT,STCKQ+RUNQ+SOONQ                                         
         BZ    UPDATE1                                                          
         MVI   OLAYNUM,5                                                        
         GOTO1 AOLAY                                                            
         B     OERRMSG                                                          
*                                                                               
UPDATE1  CLI   REQACTN,C'D'                                                     
         BNE   *+16                                                             
         CLI   STATUS,1                                                         
         BE    CONTROL                                                          
         B     UPDATE2                                                          
         XC    BVRHDR,BVRHDR                                                    
         CLI   REQACTN,C'A'                                                     
         BNE   *+14                                                             
         MVC   BVRHDR(15),=C'Request amended'                                   
         B     UPDATE2                                                          
         CLI   REQACTN,C'N'                                                     
         BNE   *+14                                                             
         MVC   BVRHDR(13),=C'Request added'                                     
         B     UPDATE2                                                          
*                                                                               
UPDATE2  CLI   REQNDX1+1,X'FC'                                                  
         BE    *+12                MENU SCREENS SET FADR THEMSELVES             
         LA    R6,BVRNAMEH                                                      
         ST    R6,FADR                                                          
         MVI   STATUS,0                                                         
         MVC   PREQNUM,LREQNUM                                                  
         MVC   PREQACTN,LREQACTN                                                
         MVC   PREQNDX1(2),LREQNDX1                                             
         B     OHDR                                                             
         EJECT                                                                  
***********************************************************************         
* SET UP SCREEN HEADER MESSAGE TO CONTAIN THE ERROR                   *         
***********************************************************************         
         SPACE 1                                                                
OERRMSG  CLC   FERN,=AL2(FE)                                                    
         BNL   OHDR                                                             
*                                                                               
         USING GETTXTD,R1          BUILD PARAM LIST FOR GETTXT                  
         LA    R1,PLIST                                                         
         XC    PLIST(24),PLIST                                                  
         MVI   GTMTYP,GTMERR                                                    
         MVC   GTMSGNO,FERN        SETTING INVALID AT THIS LEVEL                
         CLI   FVXTRA,C' '         LOOK FOR ADDITIONAL TEXT                     
         BNH   OERRM10                                                          
         LA    R1,FVXTRA                                                        
         STCM  R1,7,GTATXT                                                      
         LA    RF,L'FVXTRA-1(R1)                                                
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,R1                                                            
         LA    RF,1(RF)                                                         
         STC   RF,GTLTXT                                                        
OERRM10  GOTO1 GETTXT,PLIST                                                     
*                                                                               
*        OUTPUT SCREEN HEADER MESSAGE , POSITION THE CURSOR , AND               
*        RETURN TO TERMINAL                                                     
*                                                                               
OHDR     OI    BVRHDRH+1,X'08'     HIGH INTENSITY                               
         OI    BVRHDRH+6,OI1T                                                   
         MVI   BVRHDRH+7,60                                                     
         L     RF,FADR                                                          
         OI    6(RF),OI1C                                                       
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* HANDLE OVERLAY LINKAGE                                              *         
***********************************************************************         
*                                                                               
OLAY     NTR1  BASE=ABASE                                                       
         CLI   OLAYNUM,5                                                        
         BE    *+8                                                              
         MVI   DISPMODE,0          RESET DISPLAY MODE IF NOT SOON               
         CLC   OLAYNUM,LOLAYNUM    IS OVERLAY IN CORE                           
         BE    OLAY1               YES                                          
         XC    CALLOVPL(8),CALLOVPL                                             
         MVC   CALLOVPL(1),OLAYNUM                                              
         ST    R3,CALLOVPL+4                                                    
         GOTO1 CALLOV,CALLOVPL                                                  
         CLI   CALLOVPL+4,X'FF'                                                 
         BNE   *+6                                                              
         DC    H'0'                DUMP IF CANT LOAD                            
         MVC   LOLAYNUM(4),CALLOVPL                                             
OLAY1    GOTO1 LOLAYNUM,PHASEPL,(R9)                                            
OLAYX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              READ RECORD SPECIFIED BY KEY                           *         
***********************************************************************         
*                                                                               
IOREAD   NTR1  BASE=ABASE                                                       
         MVC   NAME,SPACES                                                      
         L     R6,AIO1                                                          
         MVC   0(L'KEY,R6),KEY                                                  
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,(R6),(R6)                             
         CLI   DMCB+8,0                                                         
         BNE   XNO                                                              
*                                                                               
         USING NAMELD,R6                                                        
         L     R6,AIO1                                                          
         MVI   ELCODE,NAMELQ       X'20' NAME ELEMENT                           
         BAS   RE,GETEL                                                         
         BNE   XYES                                                             
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BM    XYES                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NAME(0),NAMEREC                                                  
*                                                                               
XYES     CLI   *+1,0                                                            
         B     EXIT                                                             
XNO      CLI   *,0                                                              
         B     EXIT                                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*              PROCESS AN INPUT FIELD                                 *         
***********************************************************************         
*                                                                               
INITV    NTR1  BASE=ABASE                                                       
         L     R1,FADR                                                          
         XC    FIND,FIND                                                        
         XC    IFLDH,IFLDH                                                      
         XC    NAME,NAME                                                        
         MVC   IFLD,SPACES                                                      
         MVC   FERN,=AL2(FF)                                                    
*                                                                               
         CLI   5(R1),0                                                          
         BNE   *+14                                                             
         MVC   FERN,=AL2(FLDMIS)                                                
         B     INITVX                                                           
*                                                                               
         SR    RF,RF                                                            
         IC    RF,5(R1)                                                         
         SH    RF,=H'1'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   IFLD(0),8(R1)                                                    
         MVC   IFLDH,0(R1)                                                      
         OI    FIND,FIINP          SET FIELD INPUT INDICATOR                    
*                                                                               
         CLI   IFLDH+5,3                                                        
         BNE   *+18                                                             
         CLC   IFLD(3),=C'ALL'                                                  
         BNE   *+8                                                              
         OI    FIND,FIALL          SET FIELD INPUT INDICATOR -> ALL             
*                                                                               
INITVX   DS    0H                                                               
         CLI   FIND,FIINP          LOW=MISSING,EQL=INPUT,HIGH=C'ALL'            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              RIGHT JUSTIFY A FIELD                                  *         
***********************************************************************         
*                                                                               
*        PARM1 = LENGTH OF INPUT                                                
*        PARM2 = A(FIELD HEADER)                                                
*                                                                               
*        OUTPUT RETURNED IN 'TEMP'                                              
*              CL1   BINARY VALUE                                               
*              CL3   RJ JUSTIFIED VALUE                                         
*                                                                               
RJN      NTR1  BASE=ABASE                                                       
         L     R5,0(R1)            PARM1                                        
         L     R4,4(R1)            PARM2                                        
         MVC   TEMP(4),=C'0000'                                                 
         MVC   FERN,=AL2(FF)                                                    
         LR    R1,R5                                                            
         BCTR  R1,R0                                                            
         LA    R6,3                                                             
         SR    R6,R5                                                            
         LA    R6,TEMP+1(R6)                                                    
         EX    R1,*+8              RJ JUSTIFY AT TEMP+1(3)                      
         B     *+10                                                             
         MVC   0(0,R6),8(R4)                                                    
         MVC   TEMP+10(3),=C'0000' CHECK FOR NUMERIC                            
         MVZ   TEMP+10(3),TEMP+1                                                
         CLC   TEMP+10(3),=C'0000'                                              
         BE    *+14                                                             
         MVC   FERN,=AL2(FLDNNUM)  FIELD NOT NUMERIC                            
         B     RJNX                                                             
         PACK  DUB,TEMP+1(3)       CHECK GT 0 AND LE 255                        
         CVB   R6,DUB                                                           
         CH    R6,=H'0'                                                         
         BE    RJN1                                                             
         CH    R6,=H'255'                                                       
         BH    RJN1                                                             
         STC   R6,TEMP             RETURN BINARY VALUE                          
         B     RJNX                                                             
RJN1     MVC   FERN,=AL2(INVINPT)  INVALID INPUT                                
RJNX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              $RFP INTERFACE                                         *         
***********************************************************************         
*                                                                               
RFPINIT  NTR1  BASE=ABASE                                                       
         MVI   OLAYNUM,9           PASS CONTROL TO OVERLAY                      
         GOTO1 AOLAY                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              SET REQUEST DETAILS                                    *         
***********************************************************************         
*                                                                               
SETREQ   NTR1  BASE=ABASE                                                       
         LA    RF,5                COUNT NUMBER OF REQUEST CARDS USED           
         LA    R1,ACQCARD5                                                      
SETREQ2  OC    0(L'ACQCARD5,R1),0(R1)                                           
         BZ    SETREQ4             CARD ALL BINARY ZEROES                       
         CLC   0(L'ACQCARD5,R1),SPACES                                          
         BNE   *+14                                                             
SETREQ4  SH    R1,=Y(L'ACQCARD5)                                                
         BCT   RF,SETREQ2                                                       
         DC    H'0'                                                             
*                                                                               
         BCTR  RF,R0                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     SET1                                                             
         B     SET2                                                             
         B     SET3                                                             
         B     SET4                                                             
         MVI   ACQCONT4,C'C'                                                    
SET4     MVI   ACQCONT3,C'C'                                                    
SET3     MVI   ACQCONT2,C'C'                                                    
SET2     MVI   ACQCONT1,C'C'                                                    
SET1     SLL   RF,2                                                             
         LA    RF,3(RF)            SET LINKED REQUEST - X'03' LO NIBBLE         
         STC   RF,REQFLAG          # REQUEST CARDS IN HI NIBBLE                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE ACCOUNT GROUPS (INPUT IN CLIENT OR ACCOUNT)   *         
***********************************************************************         
*                                                                               
GRPVAL   NTR1  BASE=ABASE                                                       
         CLC   =C'SJ',ACQUNT                                                    
         BE    GRPV02                                                           
         CLC   =C'SR',ACQUNT                                                    
         BE    GRPV02                                                           
         CLC   =C'1C',ACQUNT                                                    
         BE    GRPV02                                                           
         CLC   =C'29',ACQUNT                                                    
         BNE   GRPVX                                                            
*                                                                               
GRPV02   CLI   IFLD,C'G'                                                        
         BNE   GRPVX                                                            
         CLI   IFLD+2,C'='                                                      
         BNE   GRPVX                                                            
         CLI   IFLD+1,C'1'         MUST BE 1-9                                  
         BL    GRPVX                                                            
         CLI   IFLD+1,C'9'                                                      
         BH    GRPVX                                                            
         TM    COMPSTA7,CPYSAGRP   ACCOUNT GROUPS IN USE?                       
         BNZ   *+14                                                             
         MVC   FERN,=AL2(NOTONAG)                                               
         B     GRPVX                                                            
         MVC   AGTYPE,IFLD+1       EXTRACT ACCOUNT GROUP TYPE                   
         MVC   AGCODE,SPACES                                                    
*                                                                               
         CLC   =C'&&  ',IFLD+3     ALL CODES?                                   
         BNE   GRPV10              NO                                           
         CLC   =C'73',ACQPROG      TEST AC73 REQUEST                            
         BE    GRPV04                                                           
*                                                                               
         CLC   =C'SOON',BVROUT                                                  
         BNE   GRPV04                                                           
         MVC   FERN,=AL2(AGTSOON)                                               
         B     GRPVX                                                            
*                                                                               
GRPV04   LA    R2,KEY                                                           
         USING CPYRECD,R2                                                       
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,ACQCPY                                                   
         GOTO1 AIOREAD             READ COMPANY RECORD                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         L     R1,AIO1                                                          
         AH    R1,DATADISP                                                      
*                                                                               
         USING FFTELD,R1                                                        
GRPV06   CLI   FFTEL,0             TEST END OF RECORD                           
         BNE   *+14                                                             
         MVC   FERN,=AL2(AGRPNOF)  ACCOUNT GROUP NOT ON FILE                    
         B     GRPVX                                                            
         CLI   FFTEL,FFTELQ        TEST FREE FOR TEXT ELEMENT                   
         BNE   GRPV08                                                           
         CLI   FFTTYPE,FFTTAAGR    TEST ACCOUNT GROUP NAME                      
         BNE   GRPV08                                                           
         MVC   TEMP(L'FFTSEQ),FFTSEQ                                            
         OI    TEMP,X'F0'                                                       
         CLC   AGTYPE,TEMP         TEST SAME ACCOUNT GROUP TYPE                 
         BNE   GRPV08                                                           
         MVC   NAME,SPACES                                                      
         SR    RE,RE                                                            
         IC    RE,FFTDLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   NAME(0),FFTDATA                                                  
         B     GRPV20                                                           
GRPV08   IC    RE,FFTLN                                                         
         AR    R1,RE                                                            
         B     GRPV06                                                           
*                                                                               
GRPV10   MVC   AGCODE,IFLD+3       EXTRACT ACCOUNT GROUP CODE                   
         LA    R1,AGCODE                                                        
         LA    R0,L'AGCODE                                                      
*                                                                               
GRPV12   CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         MVI   0(R1),X'00'                                                      
         LA    R1,1(R1)                                                         
         BCT   R0,GRPV12                                                        
         LA    R2,KEY                                                           
         USING AGRRECD,R2                                                       
         XC    AGRKEY,AGRKEY                                                    
         MVI   AGRKTYP,AGRKTYPQ                                                 
         MVC   AGRKCPY,CMPY                                                     
         MVC   AGRKGTYP,AGTYPE                                                  
         MVC   AGRKAGR,AGCODE                                                   
         GOTO1 AIOREAD             READ ACCOUNT GROUP RECORD                    
         BE    GRPV14                                                           
         MVC   FERN,=AL2(AGRPNOF)  ACCOUNT GROUP NOT ON FILE                    
         B     GRPVX                                                            
*                                                                               
GRPV14   SR    RE,RE                                                            
         L     R1,AIO1                                                          
         AH    R1,DATADISP                                                      
*                                                                               
         USING NAMELD,R1                                                        
GRPV16   CLI   NAMEL,0             TEST END OF RECORD                           
         BE    GRPV20                                                           
         CLI   NAMEL,NAMELQ        TEST NAME ELEMENT                            
         BE    GRPV18                                                           
         IC    RE,NAMLN                                                         
         AR    R1,RE                                                            
         B     GRPV16                                                           
*                                                                               
GRPV18   IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         BM    GRPV20                                                           
         MVC   NAME,SPACES                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   NAME(0),NAMEREC                                                  
*                                                                               
GRPV20   MVI   ACQACT+0,C'G'                                                    
         MVC   ACQACT+1(L'AGTYPE),AGTYPE                                        
         MVI   ACQACT+1+L'AGTYPE,C'='                                           
         MVC   ACQACT+2+L'AGTYPE(3),IFLD+3                                      
         OI    FIND,FIVAL                                                       
*                                                                               
GRPVX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CHECK IF USER HAS ACCESS TO UPDATE THE FILE                         *         
***********************************************************************         
         SPACE 1                                                                
CHKACC   NTR1  BASE=ABASE                                                       
         MVC   FVXTRA,SPACES                                                    
         L     R1,ACOMFACS                                                      
         L     R2,CXTRAINF-COMFACSD(R1)                                         
         LA    RF,BVRNAMEH                                                      
         ST    RF,FADR                                                          
         USING XTRAINFD,R2                                                      
         TM    XIFLAG1,XIROSYS     CONNECTED TO READ ONLY SYS                   
         BNO   CHKA10                                                           
         MVC   FERN,=AL2(ADVUPDNO)       "ADV NOT ALLOWING UPDS"                
         B     CHKAX                                                            
CHKA10   TM    XIFLAG1,XIROMODE    CONNECTED IN READ ONLY MODE                  
         BNO   CHKA20                                                           
         MVC   FERN,=AL2(NOUPDALW)       "NOT AUTHORIZED TO UPD"                
         B     CHKAX                                                            
CHKA20   TM    XIFLAG1,XIWRONGF    CONNECTED TO WRONG FACPAK                    
         BNO   CHKAX                                                            
         MVC   FERN,=AL2(NOTHMADV)       "NOT CONNECTED TO HOME ADV"            
         MVC   FVXTRA,XIUPDFAC     GET THE UPDATIVE ADV NAME                    
CHKAX    B     EXIT                                                             
         DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADDRESS CONSTANTS                                                   *         
***********************************************************************         
         SPACE 1                                                                
DATADISP DC    H'49'                                                            
DMREAD   DC    CL8'DMREAD'                                                      
ACCFIL   DC    CL8'ACCOUNT'                                                     
*                                                                               
ADCONS   DS    0F                                                               
         DC    A(REQTBL)                                                        
         DC    A(FLDNUMT)                                                       
         DC    V(BMONVAL)                                                       
         DC    A(MOAVTBL)                                                       
         DC    A(SORTVTBL)                                                      
         DC    A(PRGTAB)                                                        
         DC    V(TWABLD)                                                        
*&&UK*&& DC    V(SECURE)                                                        
NADCONS  EQU   (*-ADCONS)/L'ADCONS                                              
         EJECT                                                                  
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
         SPACE 1                                                                
PRGTAB   DS    0C                                                               
         DC    C'B1',C'BGL',XL1'01',XL1'01',XL1'00',AL1(0),C' '                 
         DC    C'B2',C'BCM',XL1'02',XL1'01',XL1'00',AL1(0),C' '                 
         DC    C'B3',C'MBL',XL1'03',XL1'01',XL1'00',AL1(0),C' '                 
         DC    C'B4',C'BDU',XL1'04',XL1'01',XL1'00',AL1(0),C' '                 
         DC    C'50',C'VCR',XL1'05',XL1'02',XL1'00',AL1(0),C' '                 
         DC    C'57',C'CBK',XL1'06',XL1'02',XL1'00',AL1(0),C' '                 
         DC    C'58',C'CSH',XL1'07',XL1'02',XL1'00',AL1(0),C' '                 
         DC    C'C1',C'VAR',XL1'08',XL1'02',XL1'00',AL1(0),C' '                 
         DC    C'TT',C'FMS',XL1'09',XL1'02',XL1'20'                             
         DC    AL1(ACQOPT1-ACQD),C'Y'                                           
         DC    C'T7',C'FMS',XL1'09',XL1'02',XL1'20'                             
         DC    AL1(ACQOPT1-ACQD),C'Y'                                           
         DC    C'55',C'CHK',XL1'0A',XL1'03',XL1'40',AL1(0),C' '                 
         DC    C'97',C'ADL',XL1'0B',XL1'04',XL1'20'                             
         DC    AL1(ACQOPT1-ACQD),C' '                                           
         DC    C'98',C'APL',XL1'0C',XL1'04',XL1'20'                             
         DC    AL1(ACQOPT1-ACQD),C' '                                           
*        DC    C'32',C'SAR',XL1'0D',XL1'05',XL1'00',AL1(0),C' '                 
*        DC    C'33',C'HSA',XL1'0E',XL1'05',XL1'00',AL1(0),C' '                 
*        DC    C'34',C'CIA',XL1'0F',XL1'05',XL1'00',AL1(0),C' '                 
*        DC    C'40',C'HCS',XL1'10',XL1'05',XL1'00',AL1(0),C' '                 
*        DC    C'83',C'RAG',XL1'11',XL1'05',XL1'00',AL1(0),C' '                 
*        DC    C'RA',C'RAR',XL1'12',XL1'05',XL1'00',AL1(0),C' '                 
*        DC    C'XW',C'WPR',XL1'13',XL1'05',XL1'00',AL1(0),C' '                 
         DC    C'41',C'JPX',XL1'14',XL1'06',XL1'00',AL1(0),C' '                 
         DC    C'45',C'RSR',XL1'15',XL1'06',XL1'00',AL1(0),C' '                 
         DC    C'47',C'RDR',XL1'16',XL1'06',XL1'00',AL1(0),C' '                 
         DC    C'70',C'TRL',XL1'17',XL1'06',XL1'00',AL1(0),C' '                 
         DC    C'71',C'JCP',XL1'18',XL1'06',XL1'00',AL1(0),C' '                 
         DC    C'73',C'ALS',XL1'19',XL1'06',XL1'00',AL1(0),C' '                 
         DC    C'74',C'RPL',XL1'1A',XL1'06',XL1'00',AL1(0),C' '                 
         DC    C'75',C'ANA',XL1'1B',XL1'06',XL1'00',AL1(0),C' '                 
         DC    C'76',C'FVL',XL1'1C',XL1'06',XL1'00',AL1(0),C' '                 
         DC    C'77',C'ASY',XL1'1D',XL1'06',XL1'00',AL1(0),C' '                 
         DC    C'79',C'JAL',XL1'1E',XL1'06',XL1'00',AL1(0),C' '                 
         DC    C'80',C'LST',XL1'1F',XL1'06',XL1'00',AL1(0),C' '                 
         DC    C'AP',C'APG',XL1'20',XL1'06',XL1'00',AL1(0),C' '                 
         DC    C'CN',C'ACN',XL1'21',XL1'06',XL1'00',AL1(0),C' '                 
         DC    C'GR',C'AGR',XL1'2E',XL1'06',XL1'00',AL1(0),C' '                 
         DC    C'31',C'AST',XL1'22',XL1'07',XL1'00',AL1(0),C' '                 
         DC    C'38',C'CST',XL1'23',XL1'07',XL1'00',AL1(0),C' '                 
         DC    C'81',C'ATB',XL1'24',XL1'07',XL1'00',AL1(0),C' '                 
         DC    C'84',C'OTB',XL1'25',XL1'07',XL1'00',AL1(0),C' '                 
         DC    C'85',C'AHS',XL1'26',XL1'07',XL1'00',AL1(0),C' '                 
         DC    C'87',C'AHX',XL1'27',XL1'07',XL1'00',AL1(0),C' '                 
         DC    C'GT',C'GST',XL1'28',XL1'07',XL1'00',AL1(0),C' '                 
         DC    C'IJ',C'IJR',XL1'29',XL1'07',XL1'00',AL1(0),C' '                 
         DC    C'25',C'GLU',XL1'2A',XL1'08',XL1'20'                             
         DC    AL1(ACQOPT2-ACQD),C'L'                                           
         DC    C'G1',C'GLS',XL1'2B',XL1'08',XL1'00',AL1(0),C' '                 
         DC    C'G2',C'GLP',XL1'2C',XL1'08',XL1'00',AL1(0),C' '                 
         DC    C'G3',C'GLT',XL1'2D',XL1'08',XL1'00',AL1(0),C' '                 
*        DC    C'I7',C'PFI',XL1'2F',XL1'09',XL1'00',AL1(0),C' '                 
*        DC    C'I8',C'PVI',XL1'30',XL1'09',XL1'00',AL1(0),C' '                 
         DC    C'95',C'DEA',XL1'31',XL1'09',XL1'00',AL1(0),C' '                 
         DC    C'C4',C'PTC',XL1'32',XL1'09',XL1'00',AL1(0),C' '                 
         DC    C'C5',C'DTS',XL1'33',XL1'09',XL1'00',AL1(0),C' '                 
         DC    C'C6',C'DCS',XL1'34',XL1'09',XL1'00',AL1(0),C' '                 
*        DC    C'C9',C'EPS',XL1'35',XL1'09',XL1'00',AL1(0),C' '                 
         DC    C'CP',C'DSC',XL1'36',XL1'09',XL1'00',AL1(0),C' '                 
         DC    C'CS',C'CSS',XL1'37',XL1'09',XL1'00',AL1(0),C' '                 
         DC    C'DR',C'DEB',XL1'38',XL1'09',XL1'00',AL1(0),C' '                 
*        DC    C'I2',C'VTI',XL1'39',XL1'09',XL1'00',AL1(0),C' '                 
         DC    C'94',C'CPA',XL1'3A',XL1'09',XL1'00',AL1(0),C' '                 
         DC    C'21',C'JBL',XL1'3B',XL1'0A',XL1'40',AL1(0),C' '                 
         DC    C'22',C'DBL',XL1'3C',XL1'0A',XL1'00',AL1(0),C' '                 
         DC    C'23',C'REV',XL1'3D',XL1'0A',XL1'40',AL1(0),C' '                 
         DC    C'24',C'DUN',XL1'3E',XL1'0A',XL1'00',AL1(0),C' '                 
         DC    C'27',C'CGB',XL1'3F',XL1'0A',XL1'40',AL1(0),C' '                 
         DC    C'28',C'DCB',XL1'40',XL1'0A',XL1'00',AL1(0),C' '                 
         DC    C'29',C'CGR',XL1'41',XL1'0A',XL1'40',AL1(0),C' '                 
         DC    C'30',C'DCR',XL1'42',XL1'0A',XL1'00',AL1(0),C' '                 
         DC    C'AH',C'AHR',XL1'43',XL1'0A',XL1'00',AL1(0),C' '                 
         DC    C'BX',C'ABX',XL1'44',XL1'0A',XL1'00',AL1(0),C' '                 
         DC    C'I1',C'PBI',XL1'45',XL1'0A',XL1'00',AL1(0),C' '                 
         DC    C'59',C'JER',XL1'46',XL1'0B',XL1'00',AL1(0),C' '                 
         DC    C'61',C'JAR',XL1'47',XL1'0B',XL1'00',AL1(0),C' '                 
         DC    C'62',C'IBR',XL1'48',XL1'0B',XL1'00',AL1(0),C' '                 
         DC    C'64',C'CEA',XL1'49',XL1'0B',XL1'00',AL1(0),C' '                 
         DC    C'66',C'CBR',XL1'4A',XL1'0B',XL1'00',AL1(0),C' '                 
         DC    C'67',C'JST',XL1'4B',XL1'0B',XL1'00',AL1(0),C' '                 
         DC    C'P1',C'ACS',XL1'4C',XL1'0B',XL1'00',AL1(0),C' '                 
         DC    C'P8',C'WBS',XL1'4D',XL1'0B',XL1'00',AL1(0),C' '                 
         DC    C'P2',C'OES',XL1'4E',XL1'0C',XL1'00',AL1(0),C' '                 
         DC    C'P3',C'ODL',XL1'4F',XL1'0C',XL1'00',AL1(0),C' '                 
         DC    C'P4',C'JOD',XL1'50',XL1'0C',XL1'00',AL1(0),C' '                 
*        DC    C'26',C'RDR',XL1'51',XL1'0D',XL1'00',AL1(0),C' '                 
*        DC    C'48',C'SHB',XL1'52',XL1'0D',XL1'00',AL1(0),C' '                 
*        DC    C'89',C'INF',XL1'53',XL1'0D',XL1'00',AL1(0),C' '                 
*        DC    C'91',C'DTA',XL1'54',XL1'0D',XL1'00',AL1(0),C' '                 
*        DC    C'96',C'CPR',XL1'55',XL1'0D',XL1'00',AL1(0),C' '                 
*        DC    C'A1',C'CTA',XL1'56',XL1'0D',XL1'00',AL1(0),C' '                 
*        DC    C'IA',C'AIA',XL1'57',XL1'0D',XL1'00',AL1(0),C' '                 
*        DC    C'TI',C'TIM',XL1'58',XL1'0D',XL1'00',AL1(0),C' '                 
*        DC    C'AR',C'AAR',XL1'59',XL1'0D',XL1'00',AL1(0),C' '                 
         DC    C'92',C'DTN',XL1'5A',XL1'0E',XL1'00',AL1(0),C' '                 
         DC    C'93',C'CPY',XL1'5B',XL1'0E',XL1'00',AL1(0),C' '                 
         DC    C'9A',C'CPF',XL1'5C',XL1'0E',XL1'00',AL1(0),C' '                 
*        DC    C'A2',C'CST',XL1'5D',XL1'0E',XL1'80',AL1(0),C' '                 
         DC    C'C8',C'CRR',XL1'5E',XL1'0E',XL1'00',AL1(0),C' '                 
         DC    C'CA',C'CAR',XL1'5F',XL1'0E',XL1'A0'                             
         DC    AL1(ACQOPT1-ACQD),C'L'                                           
         DC    C'CH',C'CHR',XL1'60',XL1'0E',XL1'80',AL1(0),C' '                 
         DC    C'CU',C'SHT',XL1'61',XL1'0E',XL1'80',AL1(0),C' '                 
         DC    C'D8',C'AD8',XL1'62',XL1'0E',XL1'00',AL1(0),C' '                 
         DC    C'FI',C'FIN',XL1'63',XL1'0E',XL1'00',AL1(0),C' '                 
         DC    C'M2',C'MPW',XL1'64',XL1'0E',XL1'00',AL1(0),C' '                 
         DC    C'PA',C'PAL',XL1'65',XL1'0E',XL1'80',AL1(0),C' '                 
         DC    C'PC',C'PCR',XL1'66',XL1'0E',XL1'80',AL1(0),C' '                 
         DC    C'R1',C'CLR',XL1'67',XL1'0E',XL1'00',AL1(0),C' '                 
         DC    C'R2',C'EER',XL1'68',XL1'0E',XL1'00',AL1(0),C' '                 
         DC    C'R3',C'CFA',XL1'69',XL1'0E',XL1'00',AL1(0),C' '                 
         DC    C'R4',C'WDS',XL1'6A',XL1'0E',XL1'00',AL1(0),C' '                 
         DC    C'R5',C'WFR',XL1'6B',XL1'0E',XL1'00',AL1(0),C' '                 
         DC    C'R6',C'EUR',XL1'6C',XL1'0E',XL1'00',AL1(0),C' '                 
         DC    C'R7',C'CHA',XL1'6D',XL1'0E',XL1'00',AL1(0),C' '                 
         DC    C'82',C'ADS',XL1'6E',XL1'0F',XL1'00',AL1(0),C' '                 
         DC    C'CF',C'COF',XL1'6F',XL1'0F',XL1'00',AL1(0),C' '                 
         DC    C'E1',C'IAR',XL1'70',XL1'0F',XL1'00',AL1(0),C' '                 
         DC    C'IV',C'INV',XL1'71',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'IE',C'TEI',XL1'72',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'IT',C'AIT',XL1'73',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'R8',C'AR8',XL1'74',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'R9',C'EER',XL1'75',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'CX',C'COX',XL1'76',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'IC',C'BIR',XL1'77',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'7A',C'CAL',XL1'78',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'ID',C'AID',XL1'79',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'FA',C'FAL',XL1'7A',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'FR',C'FEE',XL1'7B',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'I5',C'BSI',XL1'7C',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'WP',C'ADA',XL1'7D',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'IS',C'AIS',XL1'7E',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'GF',C'GFI',XL1'7F',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'IR',C'BEI',XL1'80',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'CT',C'WTS',XL1'81',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'P9',C'POT',XL1'82',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'BF',C'ABF',XL1'83',XL1'0B',XL1'00',AL1(0),C' '                 
         DC    C'BF',C'ABF',XL1'83',XL1'10',XL1'00',AL1(0),C' '                 
*        DC    C'GL',C'GL1',XL1'83',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'BT',C'BTI',XL1'84',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'FE',C'FAR',XL1'85',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'PM',C'PME',XL1'86',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'WC',C'AWC',XL1'87',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'WI',C'AWI',XL1'88',XL1'10',XL1'00',AL1(0),C' '                 
*        DC    C'00',C'???',XL1'89',XL1'10',XL1'00',AL1(0),C' '                 
*        DC    C'8A',C'AIS',XL1'8A',XL1'10',XL1'00',AL1(0),C' '                 
*        DC    C'CB',C'CTB',XL1'8B',XL1'10',XL1'00',AL1(0),C' '                 
*        DC    C'CI',C'PIR',XL1'8C',XL1'10',XL1'00',AL1(0),C' '                 
*        DC    C'CL',C'CLI',XL1'8D',XL1'10',XL1'00',AL1(0),C' '                 
*        DC    C'I3',C'JBI',XL1'8E',XL1'10',XL1'00',AL1(0),C' '                 
*        DC    C'I4',C'JIR',XL1'8F',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'C2',C'TSE',XL1'90',XL1'11',XL1'00',AL1(0),C' '                 
         DC    C'C3',C'TSH',XL1'91',XL1'11',XL1'00',AL1(0),C' '                 
         DC    C'CM',C'MTS',XL1'92',XL1'11',XL1'00',AL1(0),C' '                 
*        DC    C'TJ',C'TIJ',XL1'93',XL1'11',XL1'00',AL1(0),C' '                 
         DC    C'36',C'ISR',XL1'94',XL1'13',XL1'00',AL1(0),C' '                 
         DC    C'BR',C'BLR',XL1'95',XL1'13',XL1'00',AL1(0),C' '                 
         DC    C'65',C'JCL',XL1'96',XL1'13',XL1'00',AL1(0),C' '                 
         DC    C'78',C'JMD',XL1'97',XL1'13',XL1'00',AL1(0),C' '                 
         DC    C'72',C'ARL',XL1'98',XL1'13',XL1'00',AL1(0),C' '                 
         DC    C'IG',C'AIG',XL1'99',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'AD',C'AAD',XL1'9A',XL1'06',XL1'00',AL1(0),C' '                 
         DC    C'AA',C'AAA',XL1'9B',XL1'02',XL1'00',AL1(0),C' '                 
         DC    C'AB',C'AAB',XL1'A2',XL1'02',XL1'00',AL1(0),C' '                 
         DC    C'MT',C'MTR',XL1'9C',XL1'11',XL1'00',AL1(0),C' '                 
         DC    C'TD',C'ATD',XL1'9D',XL1'11',XL1'00',AL1(0),C' '                 
         DC    C'TX',C'ATX',XL1'9E',XL1'11',XL1'00',AL1(0),C' '                 
         DC    C'IO',C'AIO',XL1'9F',XL1'08',XL1'00',AL1(0),C' '                 
         DC    C'AC',C'AAC',XL1'A0',XL1'10',XL1'00',AL1(0),C' '                 
         DC    C'JS',C'JSR',XL1'A1',XL1'0B',XL1'40',AL1(0),C' '                 
         DC    C'VE',C'AVE',XL1'A3',XL1'10',XL1'00',AL1(0),C' '                 
PRGTBNUM EQU   (*-PRGTAB)/PRGTBLNQ                                              
         EJECT                                                                  
***********************************************************************         
* REQUEST TABLES                                                      *         
***********************************************************************         
*                                                                               
*        CL2   ENTRY LENGTH                                                     
*        CL1   REQUEST NUM                                                      
*        CL1   STATUS BYTE         X'80'=DDS ONLY REQUEST                       
*                                                                               
*        CLI   INPUT BITS          X'01'=REQUESTOR                              
*                                  X'02'=EXTRA FILTER SCREEN                    
*                                  X'04'=NODOWN                                 
*                                  X'10'=NODEST                                 
*                                  X'20'=NOOUTTYP                               
*                                  X'40'=SOON REQUEST ALLOWED                   
*                                  X'80'=SOON REQUEST ONLY FOR DDS              
*        CL22  REQUEST NAME                                                     
*        CL1   DEFAULT VALUES ROUTINE NUM                                       
*        CL1   FURTHER VALIDATION ROUTINE NUM                                   
*        0CLN  ENTRY FOR EACH SCREEN FOR REQUEST                                
*        CL2   ENTRY LENGTH                                                     
*        CL1   MENU STATUS BYTE    X'80' = SHOWN ON MENU                        
*                                                                               
*        CL8   DEFINITION OF EACH FIELD                                         
*                   - CL1 FIELD NUMBER - X'00' END-OF-FIELD LIST                
*                   - CL1 FIELD FORMAT STATUS                                   
*                                  X'80' = DDS ONLY FIELD                       
*                                  X'40' = DISPLAY ON RHS                       
*                                  X'20' = DPS W/O LIMIT ACCESS ONLY            
*                                  X'10' = REQUIRED ON SOON REQUESTS            
*                                  X'08' = ALLOWABLE FORMAT(EG MMMDDYY)         
*                                  X'04' = ALLOWABLE FORMAT(EG MMMYY)           
*                                  X'02' = 'ALL' ALLOWED                        
*                                  X'01' = INPUT IS OPTIONAL                    
*                   - CL2 COMMENT CODE NUMBER OR X'0000' FOR REG FIELD          
*                   - CL1 STATUS2 FIELD                                         
*                                  X'80' = DISP CANADIAN SCREENS ONLY           
*                                  X'40' = DISP NEWOFF+EMULATED ACCFILE         
*                                  X'20' = HIGH INTENSITY W/O REQUIRED          
*                                  X'10' = 'ALL' NOT ALLOWED W/SOON REQ         
*                                  X'08' = EXTRA FILTER FIELD                   
*                                  X'04' = DISP FOR APG W/SECURITY ONLY         
*                                  X'02' = DISP FOR NEW SECURITY ONLY           
*                   - CL3 SPARE                                                 
*                                                                               
*        XL1   ZERO FOR END OF ENTRY                                            
*        CL2   ALPHA REQUEST ID                                                 
*                                                                               
REQTBL   DS    0CL1                                                             
*                                                                               
RT00     DC    AL2(RT00X-*+3),AL1(00)   00 - UNKNOWN REQUEST                    
         DC    AL1(0,0)                                                         
         DC    CL22'???-UNKNOWN'                                                
         DC    AL1(00,00)                                                       
         DC    AL2(RT00X-*+1),X'00'                                             
         DC    AL1(UNITQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(LEDGQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT00X    DC    X'00',C'00'                                                      
*&&DO                                                                           
***********************************************************************         
* These are no longer required in =REQ, however ...                   *         
* =SCRIBE uses the following numbers even though the'r commented out  *         
* 1-12, 203-208, 211,212                                                        
***********************************************************************         
RT1      DC    AL2(RT1X-*+3),AL1(1)                                             
         DC    AL1(0,HREQR)                                                     
         DC    CL22'RL1-RECVBLE STATEMNT'                                       
         DC    AL1(00,00)                                                       
         DC    AL2(RT1X-*+1),X'00'                                              
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT1X     DC    X'00',C'RL'                                                      
*                                                                               
RT2      DC    AL2(RT2X-*+3),AL1(2)                                             
         DC    AL1(0,HREQR)                                                     
         DC    CL22'IL1-INCOME STATEMENT'                                       
         DC    AL1(00,00)                                                       
         DC    AL2(RT2X-*+1),X'00'                                              
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT2X     DC    X'00',C'IL'                                                      
*                                                                               
RT3      DC    AL2(RT3X-*+3),AL1(3)                                             
         DC    AL1(0,HREQR)                                                     
         DC    CL22'XL1-EXPENSE STATEMNT'                                       
         DC    AL1(00,00)                                                       
         DC    AL2(RT3X-*+1),X'00'                                              
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT3X     DC    X'00',C'XL'                                                      
*                                                                               
RT4      DC    AL2(RT4X-*+3),AL1(4)                                             
         DC    AL1(0,HREQR)                                                     
         DC    CL22'PL1-PAYBLE STATEMENT'                                       
         DC    AL1(00,00)                                                       
         DC    AL2(RT4X-*+1),X'00'                                              
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT4X     DC    X'00',C'PL'                                                      
*                                                                               
RT5      DC    AL2(RT5X-*+3),AL1(5)                                             
         DC    AL1(0,HREQR)                                                     
         DC    CL22'RP1-RECVBLE STATEMNT'                                       
         DC    AL1(00,00)                                                       
         DC    AL2(RT5X-*+1),X'00'                                              
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT5X     DC    X'00',C'RP'                                                      
*                                                                               
RT6      DC    AL2(RT6X-*+3),AL1(6)                                             
         DC    AL1(0,HREQR)                                                     
         DC    CL22'IP1-INCOME STATEMENT'                                       
         DC    AL1(00,00)                                                       
         DC    AL2(RT6X-*+1),X'00'                                              
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT6X     DC    X'00',C'IP'                                                      
*                                                                               
RT7      DC    AL2(RT7X-*+3),AL1(7)                                             
         DC    AL1(0,HREQR)                                                     
         DC    CL22'XP1-EXPENSE STATEMNT'                                       
         DC    AL1(00,00)                                                       
         DC    AL2(RT7X-*+1),X'00'                                              
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT7X     DC    X'00',C'XP'                                                      
*                                                                               
RT8      DC    AL2(RT8X-*+3),AL1(8)                                             
         DC    AL1(0,HREQR)                                                     
         DC    CL22'PP1-PAYBLE STATEMENT'                                       
         DC    AL1(00,00)                                                       
         DC    AL2(RT8X-*+1),X'00'                                              
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT8X     DC    X'00',C'PP'                                                      
*                                                                               
RT9      DC    AL2(RT9X-*+3),AL1(9)                                             
         DC    AL1(0,HREQR)                                                     
         DC    CL22'VL1-PRODUCTION'                                             
         DC    AL1(00,00)                                                       
         DC    AL2(RT9X-*+1),X'00'                                              
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT9X     DC    X'00',C'VL'                                                      
*                                                                               
RT10     DC    AL2(RT10X-*+3),AL1(10)                                           
         DC    AL1(0,HREQR)                                                     
         DC    CL22'VP1-PRODUCTION'                                             
         DC    AL1(00,00)                                                       
         DC    AL2(RT10X-*+1),X'00'                                             
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT10X    DC    X'00',C'VP'                                                      
*                                                                               
RT11     DC    AL2(RT11X-*+3),AL1(11)                                           
         DC    AL1(0,HREQR)                                                     
         DC    CL22'1L1-PERSON'                                                 
         DC    AL1(00,00)                                                       
         DC    AL2(RT11X-*+1),X'00'                                             
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT11X    DC    X'00',C'1L'                                                      
*                                                                               
RT12     DC    AL2(RT12X-*+3),AL1(12)                                           
         DC    AL1(0,HREQR)                                                     
         DC    CL22'1P1-PRODUCTION'                                             
         DC    AL1(00,00)                                                       
         DC    AL2(RT12X-*+1),X'00'                                             
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT12X    DC    X'00',C'1P'                                                      
*&&                                                                             
RT13     DC    AL2(RT13X-*+3),AL1(13)                                           
         DC    AL1(0,HREQR)                                                     
         DC    CL22'AIT-TEXACO INTERFACE'                                       
         DC    AL1(VRQ02,00)                                                    
         DC    AL2(RT13X-*+1),X'00'                                             
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(BGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'1E00',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'0208',AL1(0),AL3(0)                       
RT13X    DC    X'00',C'IT'                                                      
*                                                                               
RT14     DC    AL2(RT14X-*+3),AL1(14)                                           
         DC    AL1(0,HREQR)                                                     
         DC    CL22'AIG-COLGATE INTERFACE'                                      
         DC    AL1(VRQ02,00)                                                    
         DC    AL2(RT14X-*+1),X'00'                                             
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(BGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'1E00',AL1(0),AL3(0)                       
RT14X    DC    X'00',C'IG'                                                      
                                                                                
RT15     DC    AL2(RT15X-*+3),AL1(15)                                           
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'CAD-CHK ACCT ADDRESSES'                                     
         DC    AL1(00,00)                                                       
         DC    AL2(RT15X-*+1),X'00'                                             
         DC    AL1(UNITQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(ACCTQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'012F',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'0130',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'0103',AL1(0),AL3(0)                       
RT15X    DC    X'00',C'AD'                                                      
                                                                                
RT16     DC    AL2(RT16X-*+3),AL1(16) RA - TRANSACTION COUNT                    
         DC    AL1(HDDSO,HREQR)                                                 
         DC    CL22'RAR-RECORD AGING   '                                        
         DC    AL1(00,00)                                                       
         DC    AL2(RT16X-*+1),X'00'                                             
*        DC    AL1(UNITQ,FDDSO+FVAL+FOPT),X'8000',AL1(0),AL3(0)                 
         DC    AL1(RMNTQ,FDDSO+FVAL),X'8C00',AL1(0),AL3(0)                      
         DC    AL1(OPT1Q,FDDSO+FVAL+FOPT),X'81DD',AL1(0),AL3(0)                 
         DC    AL1(OPT2Q,FDDSO+FVAL+FOPT),X'82DD',AL1(0),AL3(0)                 
RT16X    DC    X'00',C'RA'                                                      
                                                                                
RT17     DC    AL2(RT17X-*+3),AL1(17) D8 - ADJUSTMENT RATE REPORT               
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'AD8-ADJUSTMENT RATE'                                        
         DC    AL1(VRQ02,XVRQ44)                                                
         DC    AL2(RT17X-*+1),AL1(HMENU)                                        
         DC    AL1(OFFCQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(CLI1Q,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(PRD1Q,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(JOBQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                   
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'5C01',AL1(0),AL3(0)                       
RT17X    DC    X'00',C'D8'                                                      
                                                                                
RT18     DC    AL2(RT18X-*+3),AL1(18) BX - EDI INTERFACE TAPE                   
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'ABX-EDI INTERFACE'                                          
         DC    AL1(VRQ02,XVRQ45)                                                
         DC    AL2(RT18X-*+1),AL1(HMENU)                                        
         DC    AL1(CLI1Q,FVAL+FALL),X'8000',AL1(0),AL3(0)                       
         DC    AL1(PRD1Q,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(JOBQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                   
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL),X'012B',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL+FOPT),X'012C',AL1(0),AL3(0)                       
         DC    AL1(BNUMQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'010E',AL1(0),AL3(0)                       
RT18X    DC    X'00',C'BX'                                                      
                                                                                
RT19     DC    AL2(RT19X-*+3),AL1(19) BF - BILLING INTERFACE                    
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'ABF-BILLING INTERFACE'                                      
         DC    AL1(VRQ02,XVRQ45)                                                
         DC    AL2(RT19X-*+1),AL1(HMENU)                                        
         DC    AL1(CLI1Q,FVAL+FALL),X'8000',AL1(0),AL3(0)                       
         DC    AL1(PRD1Q,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(JOBQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                   
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'010E',AL1(0),AL3(0)                       
RT19X    DC    X'00',C'BF'                                                      
*                                                                               
RT20     DC    AL2(RT20X-*+3),AL1(20)                                           
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'AAA-AUTO APPROVE'                                           
         DC    AL1(VRQ29,XVRQ12)                                                
         DC    AL2(RT20X-*+1),X'00'                                             
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(CLI2Q,FVAL+FOPT+FALT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(MEDAQ,FVAL+FOPT),X'0131',AL1(0),AL3(0)                       
         DC    AL1(ENDMQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'0316',AL1(0),AL3(0)                       
         DC    AL1(LRUNQ,FVAL+FOPT),X'0315',AL1(0),AL3(0)                       
         DC    AL1(APPOQ,FVAL+FOPT),X'0317',AL1(0),AL3(0)                       
         DC    AL1(TOLR1Q,FVAL+FOPT),X'0320',AL1(0),AL3(0)                      
         DC    AL1(TOLR2Q,FVAL+FOPT),X'0323',AL1(0),AL3(0)                      
         DC    AL1(VNDLQ,FVAL+FOPT),X'0321',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT20X    DC    X'00',C'AA'                                                      
                                                                                
RT20A    DC    AL2(RT20AX-*+3),AL1(20)                                          
         DC    AL1(HDDSO,HSOON+HREQR)                                           
         DC    CL22'AAB-AA BY VENDOR'                                           
         DC    AL1(VRQ29,XVRQ12)                                                
         DC    AL2(RT20AX-*+1),X'00'                                            
         DC    AL1(NALQ,FVAL),X'0322',AL1(0),AL3(0)                             
*&&DO                                                                           
         DC    AL1(CLI2Q,FVAL+FOPT+FALT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(MEDAQ,FVAL+FOPT),X'0131',AL1(0),AL3(0)                       
         DC    AL1(ENDMQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'0316',AL1(0),AL3(0)                       
         DC    AL1(LRUNQ,FVAL+FOPT),X'0315',AL1(0),AL3(0)                       
         DC    AL1(APPOQ,FVAL+FOPT),X'0317',AL1(0),AL3(0)                       
         DC    AL1(TOLRQ,FVAL+FOPT),X'0320',AL1(0),AL3(0)                       
         DC    AL1(VNDLQ,FVAL+FOPT),X'0321',AL1(0),AL3(0)                       
*&&                                                                             
RT20AX   DC    X'00',C'AB'                                                      
                                                                                
RT21     DC    AL2(RT21X-*+3),AL1(21) 21 - PRODUCTION BILLING                   
         DC    AL1(0,HSOON+HNDES+HBILN+HREQR)                                   
         DC    CL22'JBL-PRODUCTION BILLING'                                     
         DC    AL1(VRQ02,XVRQ04)                                                
         DC    AL2(RT21X-*+1),AL1(HMENU)                                        
*&&DO                                                                           
         DC    AL1(CLI1Q,FSOON+FVAL+FALL),X'8000',AL1(FNALL),AL3(0)             
         DC    AL1(PRD1Q,FOPT+FSOON+FVAL+FALL),X'8000',AL1(FNALL)               
         DC    AL3(0)                                                           
         DC    AL1(JOBQ,FOPT+FSOON+FVAL+FALL),X'8000',AL1(FNALL)                
         DC    AL3(0)                                                           
*&&                                                                             
         DC    AL1(CLI1Q,FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)                 
         DC    AL1(PRD1Q,FOPT+FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)            
         DC    AL1(JOBQ,FOPT+FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)             
         DC    AL1(BGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(DDUEQ,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MEF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(BFLTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(BDATQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(CYCMQ,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(WCFTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FRENQ,FRHS+FVAL+FOPT),X'0000',AL1(FCANO),AL3(0)              
         DC    AL1(FRMTQ,FVAL+FOPT),X'0000',AL1(FNBLO),AL3(0)                   
*&&DO*&& DC    AL1(ASTDQ,FVAL+FOPT),X'0000',AL1(FNBLO),AL3(0)                   
*&&DO*&& DC    AL1(AENDQ,FRHS+FVAL+FOPT),X'0000',AL1(FNBLO),AL3(0)              
         DC    AL1(OPT1Q,FVAL+FOPT),X'A300',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'A500',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'C700',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'DD00',AL1(0),AL3(0)                       
         DC    AL1(OPT5Q,FVAL+FOPT),X'E700',AL1(0),AL3(0)                       
         DC    AL1(OPT6Q,FVAL+FOPT),X'3300',AL1(0),AL3(0)                       
         DC    AL1(OPT7Q,FVAL+FOPT),X'3400',AL1(0),AL3(0)                       
         DC    AL1(OPT8Q,FVAL+FOPT),X'0109',AL1(0),AL3(0)                       
RT21X    DC    X'00',C'21'                                                      
                                                                                
RT22     DC    AL2(RT22X-*+3),AL1(22) 22 - DRAFT BILLING                        
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'DBL-DRAFT BILLING'                                          
         DC    AL1(VRQ02,XVRQ04)                                                
         DC    AL2(RT22X-*+1),AL1(HMENU)                                        
*&&DO                                                                           
         DC    AL1(CLI1Q,FSOON+FVAL+FALL),X'8000',AL1(FNALL),AL3(0)             
         DC    AL1(PRD1Q,FOPT+FSOON+FVAL+FALL),X'8000',AL1(FNALL)               
         DC    AL3(0)                                                           
         DC    AL1(JOBQ,FOPT+FSOON+FVAL+FALL),X'8000',AL1(FNALL),AL3(0)         
*&&                                                                             
         DC    AL1(CLI1Q,FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)                 
         DC    AL1(PRD1Q,FOPT+FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)            
         DC    AL1(JOBQ,FOPT+FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)             
         DC    AL1(BGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(DDUEQ,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MEF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(BFLTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(BDATQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(CYCMQ,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(WCFTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FRENQ,FRHS+FVAL+FOPT),X'0000',AL1(FCANO),AL3(0)              
         DC    AL1(FRMTQ,FVAL+FOPT),X'0000',AL1(FNBLO),AL3(0)                   
*&&DO*&& DC    AL1(ASTDQ,FVAL+FOPT),X'0000',AL1(FNBLO),AL3(0)                   
*&&DO*&& DC    AL1(AENDQ,FRHS+FVAL+FOPT),X'0000',AL1(FNBLO),AL3(0)              
         DC    AL1(OPT1Q,FVAL+FOPT),X'A300',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'A500',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'C700',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'DD00',AL1(0),AL3(0)                       
         DC    AL1(OPT5Q,FVAL+FOPT),X'E700',AL1(0),AL3(0)                       
         DC    AL1(OPT6Q,FVAL+FOPT),X'3300',AL1(0),AL3(0)                       
         DC    AL1(OPT7Q,FVAL+FOPT),X'3400',AL1(0),AL3(0)                       
         DC    AL1(OPT8Q,FVAL+FOPT),X'0109',AL1(0),AL3(0)                       
RT22X    DC    X'00',C'22'                                                      
                                                                                
RT23     DC    AL2(RT23X-*+3),AL1(23) 23 - REVERSAL BILLING                     
         DC    AL1(0,HSOON+HBILN+HREQR)                                         
         DC    CL22'REV-REVERSAL BILLING'                                       
         DC    AL1(VRQ02,XVRQ27)                                                
         DC    AL2(RT23X-*+1),AL1(HMENU)                                        
*&&DO                                                                           
         DC    AL1(CLI1Q,FSOON+FVAL),X'8000',AL1(FNALL),AL3(0)                  
         DC    AL1(PRD1Q,FSOON+FVAL),X'8000',AL1(FNALL),AL3(0)                  
         DC    AL1(JOBQ,FOPT+FSOON+FVAL+FALL),X'8000',AL1(FNALL),AL3(0)         
*&&                                                                             
         DC    AL1(CLI1Q,FSOON+FVAL),X'8000',AL1(0),AL3(0)                      
         DC    AL1(PRD1Q,FSOON+FVAL),X'8000',AL1(0),AL3(0)                      
         DC    AL1(JOBQ,FOPT+FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)             
         DC    AL1(OBNOQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(DDUEQ,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(OBDAQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(FRMTQ,FRHS+FVAL+FOPT),X'0000',AL1(FNBLO),AL3(0)              
         DC    AL1(RBDAQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FRENQ,FRHS+FVAL+FOPT),X'0000',AL1(FCANO),AL3(0)              
         DC    AL1(OPT1Q,FVAL+FOPT),X'D800',AL1(0),AL3(0)                       
         DC    AL1(OPT7Q,FVAL+FOPT),X'3400',AL1(0),AL3(0)                       
RT23X    DC    X'00',C'23'                                                      
                                                                                
RT24     DC    AL2(RT24X-*+3),AL1(24) 24 - DRAFT UNBILLING                      
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'DUN-DRAFT UNBILLING'                                        
         DC    AL1(VRQ02,XVRQ27)                                                
         DC    AL2(RT24X-*+1),AL1(HMENU)                                        
*&&DO                                                                           
         DC    AL1(CLI1Q,FSOON+FVAL),X'8000',AL1(FNALL),AL3(0)                  
         DC    AL1(PRD1Q,FSOON+FVAL),X'8000',AL1(FNALL),AL3(0)                  
         DC    AL1(JOBQ,FOPT+FSOON+FVAL+FALL),X'8000',AL1(FNALL),AL3(0)         
*&&                                                                             
         DC    AL1(CLI1Q,FSOON+FVAL),X'8000',AL1(0),AL3(0)                      
         DC    AL1(PRD1Q,FSOON+FVAL),X'8000',AL1(0),AL3(0)                      
         DC    AL1(JOBQ,FOPT+FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)             
         DC    AL1(OBNOQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(DDUEQ,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(OBDAQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(FRMTQ,FRHS+FVAL+FOPT),X'0000',AL1(FNBLO),AL3(0)              
         DC    AL1(RBDAQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FRENQ,FRHS+FVAL+FOPT),X'0000',AL1(FCANO),AL3(0)              
         DC    AL1(OPT1Q,FVAL+FOPT),X'D800',AL1(0),AL3(0)                       
         DC    AL1(OPT7Q,FVAL+FOPT),X'3400',AL1(0),AL3(0)                       
RT24X    DC    X'00',C'24'                                                      
                                                                                
RT25     DC    AL2(RT25X-*+3),AL1(25)  25 - GENERAL LEDGR UPDATE                
         DC    AL1(0,HSOON+HNOUT+HREQR)                                         
         DC    CL22'GLU-GEN. LEDGER UPDATE'                                     
         DC    AL1(00,XVRQ06)                                                   
         DC    AL2(RT25X-*+1),AL1(HMENU)                                        
         DC    AL1(UNITQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(LEDGQ,FVAL+FALL),X'8000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(FNACO),AL3(0)                   
         DC    AL1(SEQNQ,FVAL+FOPT),X'0202',AL1(FNACO),AL3(0)                   
         DC    AL1(ARUNQ,FVAL+FOPT),X'E300',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL),X'7900',AL1(0),AL3(0)                            
RT25X    DC    X'00',C'25'                                                      
                                                                                
RT26     DC    AL2(RT26X-*+3),AL1(26)                                           
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'AAC-ACC CASHFLOW'                                           
         DC    AL1(VRQ29,XVRQ29)                                                
         DC    AL2(RT26X-*+1),X'00'                                             
         DC    AL1(CLI2Q,FVAL+FOPT+FALT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(MEDAQ,FVAL+FOPT),X'0131',AL1(0),AL3(0)                       
         DC    AL1(STRMQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDMQ,FVAL),X'0000',AL1(0),AL3(0)                            
RT26X    DC    X'00',C'AC'                                                      
                                                                                
RT27     DC    AL2(RT27X-*+3),AL1(27)                                           
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'CGB-CLIENT GRP BILL'                                        
         DC    AL1(VRQ02,XVRQ04)                                                
         DC    AL2(RT27X-*+1),AL1(HMENU)                                        
         DC    AL1(CLI1Q,FSOON+FVAL+FALL),X'8000',AL1(FNALL),AL3(0)             
         DC    AL1(PRD1Q,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(JOBQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                   
         DC    AL1(BGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(JOBGQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MEF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(WCFTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(WCTYQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FRMTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(LVL2Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(TISDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(TIEDQ,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FRENQ,FRHS+FVAL+FOPT),X'0000',AL1(FCANO),AL3(0)              
         DC    AL1(BDATQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'3300',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'0109',AL1(0),AL3(0)                       
RT27X    DC    X'00',C'27'                                                      
                                                                                
RT28     DC    AL2(RT28X-*+3),AL1(28)                                           
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'DCB-DRAFT CLI GRP BILL'                                     
         DC    AL1(VRQ02,XVRQ04)                                                
         DC    AL2(RT28X-*+1),AL1(HMENU)                                        
         DC    AL1(CLI1Q,FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)                 
         DC    AL1(PRD1Q,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(JOBQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                   
         DC    AL1(BGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(JOBGQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MEF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(WCFTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(WCTYQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FRMTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(LVL2Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(TISDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(TIEDQ,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FRENQ,FRHS+FVAL+FOPT),X'0000',AL1(FCANO),AL3(0)              
         DC    AL1(BDATQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'3300',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'0109',AL1(0),AL3(0)                       
RT28X    DC    X'00',C'28'                                                      
                                                                                
RT29     DC    AL2(RT29X-*+3),AL1(29)                                           
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'CGR-CLIENT GRP REVERSL'                                     
         DC    AL1(VRQ02,XVRQ27)                                                
         DC    AL2(RT29X-*+1),AL1(HMENU)                                        
         DC    AL1(CLI1Q,FSOON+FVAL),X'8000',AL1(FNALL),AL3(0)                  
         DC    AL1(PRD1Q,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(JOBQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                   
         DC    AL1(FRMTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(LVL2Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(TISDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(TIEDQ,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(OBNOQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(FRENQ,FRHS+FVAL+FOPT),X'0000',AL1(FCANO),AL3(0)              
         DC    AL1(OBDAQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(RBDAQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'D800',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'E100',AL1(0),AL3(0)                       
RT29X    DC    X'00',C'29'                                                      
                                                                                
RT30     DC    AL2(RT30X-*+3),AL1(30)                                           
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'DCR-DRAFT CLI GRP REVL'                                     
         DC    AL1(VRQ02,XVRQ27)                                                
         DC    AL2(RT30X-*+1),AL1(HMENU)                                        
         DC    AL1(CLI1Q,FSOON+FVAL),X'8000',AL1(0),AL3(0)                      
         DC    AL1(PRD1Q,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(JOBQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                   
         DC    AL1(FRMTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(LVL2Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(TISDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(TIEDQ,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(OBNOQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(FRENQ,FRHS+FVAL+FOPT),X'0000',AL1(FCANO),AL3(0)              
         DC    AL1(OBDAQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(RBDAQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'D800',AL1(0),AL3(0)                       
RT30X    DC    X'00',C'30'                                                      
                                                                                
RT31     DC    AL2(RT31X-*+3),AL1(31) 31 - STATEMENTS                           
         DC    AL1(0,HSOON+HXFLT+HREQR)                                         
         DC    CL22'AST-STATEMENTS'                                             
         DC    AL1(00,XVRQ18)      AFTER                                        
         DC    AL2(RT31X-*+1),AL1(HMENU)                                        
         DC    AL1(UNITQ,FSOON+FVAL),X'8000',AL1(0),AL3(0)                      
         DC    AL1(LEDGQ,FSOON+FVAL),X'8000',AL1(0),AL3(0)                      
         DC    AL1(ACCTQ,FOPT+FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)            
         DC    AL1(BGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MGRPQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MEF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(TRNTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ASTDQ,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(AENDQ,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MOARQ,FVAL+FOPT),X'1D00',AL1(0),AL3(0)                       
         DC    AL1(RVSLQ,FVAL+FOPT),X'010A',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'8100',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'BD00',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'5E00',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'A600',AL1(0),AL3(0)                       
RT31X    DC    X'00',C'31'                                                      
                                                                                
RT32     DC    AL2(RT32X-*+3),AL1(32) 32 - SALES ANALYSIS RPT                   
         DC    AL1(0,HREQR)                                                     
         DC    CL22'SAR-SALES ANALYSIS RPT'                                     
         DC    AL1(VRQ15,00)                                                    
         DC    AL2(RT32X-*+1),AL1(HMENU)                                        
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'E900',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'EB00',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'9A00',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'D700',AL1(0),AL3(0)                       
         DC    AL1(OPT5Q,FVAL+FOPT),X'F000',AL1(0),AL3(0)                       
RT32X    DC    X'00',C'32'                                                      
                                                                                
RT33     DC    AL2(RT33X-*+3),AL1(33) 33 - HISTORICAL SALES ANAL                
         DC    AL1(0,HREQR)                                                     
         DC    CL22'HSA-HISTRCL SALES ANLS'                                     
         DC    AL1(VRQ15,00)                                                    
         DC    AL2(RT33X-*+1),AL1(HMENU)                                        
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(OPT2Q,FVAL+FOPT),X'EB00',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'9A00',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'D700',AL1(0),AL3(0)                       
         DC    AL1(OPT5Q,FVAL+FOPT),X'F000',AL1(0),AL3(0)                       
RT33X    DC    X'00',C'33'                                                      
                                                                                
RT34     DC    AL2(RT34X-*+3),AL1(34)                                           
         DC    AL1(0,HREQR)                                                     
         DC    CL22'CIA-CLIENT INCOME ANLS'                                     
         DC    AL1(VRQ15,00)                                                    
         DC    AL2(RT34X-*+1),AL1(HMENU)                                        
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(CLI2Q,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'0D00',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'9A00',AL1(0),AL3(0)                       
RT34X    DC    X'00',C'34'                                                      
                                                                                
RT35     DC    AL2(RT35X-*+3),AL1(35)   MT - MISSING TIME REPORT                
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'MTR-MISSING TIME REPORT'                                    
         DC    AL1(VRQ16,XVRQ16)   SET U/L TO 1R                                
         DC    AL2(RT35X-*+1),AL1(HMENU)                                        
         DC    AL1(ACCTQ,FOPT+FVAL+FALL),X'8000',AL1(0),AL3(0)                  
         DC    AL1(PERSQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MISSQ,FVAL+FOPT),X'0319',AL1(0),AL3(0)                       
         DC    AL1(TERMQ,FVAL+FOPT),X'0318',AL1(0),AL3(0)                       
         DC    AL1(OVERQ,FVAL+FOPT),X'0318',AL1(0),AL3(0)                       
         DC    AL1(WAITQ,FVAL+FOPT),X'0318',AL1(0),AL3(0)                       
         DC    AL1(DOWNQ,FVAL+FOPT),X'012D',AL1(0),AL3(0)                       
RT35X    DC    X'00',C'MT'                                                      
                                                                                
RT36     DC    AL2(RT36X-*+3),AL1(36) 36 - ISR-INTERAGENCY SELLOFF RPT          
         DC    AL1(0,HREQR)                                                     
         DC    CL22'ISR-INTERAGNCY SELLOFF'                                     
         DC    AL1(00,00)                                                       
         DC    AL2(RT36X-*+1),AL1(HMENU)                                        
         DC    AL1(UNITQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MOARQ,FVAL+FOPT),X'1D00',AL1(0),AL3(0)                       
         DC    AL1(RVSLQ,FVAL+FOPT),X'0201',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'4600',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'0114',AL1(0),AL3(0)                       
RT36X    DC    X'00',C'36'                                                      
                                                                                
RT37     DC    AL2(RT37X-*+3),AL1(37) AH - AUTO HOLD REPORT                     
         DC    AL1(0,HREQR)                                                     
         DC    CL22'AHR-AUTO HOLD REPORT'                                       
         DC    AL1(VRQ02,XVRQ46)                                                
         DC    AL2(RT37X-*+1),AL1(HMENU)                                        
         DC    AL1(CLI1Q,FVAL+FALL),X'8000',AL1(0),AL3(0)                       
         DC    AL1(PRD1Q,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(JOBQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                   
         DC    AL1(BGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MGRPQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(MEF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(BFLTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL+FOPT),X'0110',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0110',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'010D',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'010E',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'010F',AL1(0),AL3(0)                       
RT37X    DC    X'00',C'AH'                                                      
                                                                                
RT38     DC    AL2(RT38X-*+3),AL1(38) 38 - CST-CONTRA ACCNT STATEMENTS          
         DC    AL1(0,HREQR)                                                     
         DC    CL22'CST-CONTRA A/C STMTS'                                       
         DC    AL1(00,XVRQ40)                                                   
         DC    AL2(RT38X-*+1),AL1(HMENU)                                        
         DC    AL1(UNITQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(LISTQ,FVAL+FOPT+FALT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(CAULQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(TRNTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MOARQ,FVAL+FOPT),X'1D00',AL1(0),AL3(0)                       
         DC    AL1(RVSLQ,FVAL+FOPT),X'0201',AL1(0),AL3(0)                       
         DC    AL1(ATTRQ,FVAL+FOPT),X'5700',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'BD00',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'A600',AL1(0),AL3(0)                       
RT38X    DC    X'00',C'38'                                                      
                                                                                
RT39     DC    AL2(RT39X-*+3),AL1(39)                                           
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'IOB-INTER-OFFICE BALANCING'                                 
         DC    AL1(VRQ28,XVRQ39)                                                
         DC    AL2(RT39X-*+1),AL1(HMENU)                                        
         DC    AL1(BATGQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(BATTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(BATRQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(PERSQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MOARQ,FVAL+FOPT+FALT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(REVOQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(LIVRQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(NARRQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT39X    DC    X'00',C'IO'                                                      
                                                                                
RT40     DC    AL2(RT40X-*+3),AL1(40) 40 - HISTORICAL CLIENT SALES              
         DC    AL1(0,HREQR)                                                     
         DC    CL22'HCS-HISTORICAL CLT SLS'                                     
         DC    AL1(VRQ15,00)                                                    
         DC    AL2(RT40X-*+1),X'00'                                             
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(CLI2Q,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'0D00',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'9A00',AL1(0),AL3(0)                       
RT40X    DC    X'00',C'40'                                                      
                                                                                
RT41     DC    AL2(RT41X-*+3),AL1(41) 41 - PROFILE EXCEPTION REP                
         DC    AL1(0,HREQR)                                                     
         DC    CL22'JPX-PROFILE EXCEPTION'                                      
         DC    AL1(VRQ02,00)                                                    
         DC    AL2(RT41X-*+1),X'00'                                             
         DC    AL1(OPT1Q,FVAL),X'9700',AL1(0),AL3(0) OPT1=P,X                   
RT41X    DC    X'00',C'41'                                                      
                                                                                
RT45     DC    AL2(RT45X-*+3),AL1(45)                                           
         DC    AL1(0,HREQR)                                                     
         DC    CL22'RSR-RETAIL SCHEME RPT'                                      
         DC    AL1(VRQ13,00)                                                    
         DC    AL2(RT45X-*+1),AL1(HMENU)                                        
         DC    AL1(ADVRQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(SCHMQ,FVAL),X'0000',AL1(0),AL3(0)                            
RT45X    DC    X'00',C'45'                                                      
                                                                                
RT47     DC    AL2(RT47X-*+3),AL1(47)                                           
         DC    AL1(0,HREQR)                                                     
         DC    CL22'RDR-RETAIL DISTB RPT'                                       
         DC    AL1(VRQ12,00)                                                    
         DC    AL2(RT47X-*+1),X'00'                                             
         DC    AL1(ADVRQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(SCHMQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT3Q,FVAL+FOPT),X'BB00',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'D200',AL1(0),AL3(0)                       
RT47X    DC    X'00',C'47'                                                      
                                                                                
RT48     DC    AL2(RT48X-*+3),AL1(48)                                           
         DC    AL1(0,HREQR)                                                     
         DC    CL22'SHB-SCHLITZ BILLING'                                        
         DC    AL1(VRQ13,00)                                                    
         DC    AL2(RT48X-*+1),X'00'                                             
         DC    AL1(ADVRQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(BDATQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(BCHRQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'F900',AL1(0),AL3(0)                       
RT48X    DC    X'00',C'48'                                                      
                                                                                
RT50     DC    AL2(RT50X-*+3),AL1(50)                                           
         DC    AL1(0,HREQR)                                                     
         DC    CL22'VCR-VOID CHECK RPT'                                         
         DC    AL1(VRQ08,00)                                                    
         DC    AL2(RT50X-*+1),AL1(HMENU)                                        
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT50X    DC    X'00',C'50'                                                      
                                                                                
RT55     DC    AL2(RT55X-*+3),AL1(55) 55 - CHECKS                               
         DC    AL1(0,HNDES+HREQR)                                               
         DC    CL22'CHK-CHECKS'                                                 
         DC    AL1(VRQ08,XVRQ02)                                                
         DC    AL2(RT55X-*+1),AL1(HMENU)                                        
         DC    AL1(LEDGQ,FSOON+FVAL),X'8000',AL1(0),AL3(0)                      
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(CLI3Q,FVAL+FOPT+FALT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(PRD2Q,15),X'8000',AL1(0),AL3(0)                              
         DC    AL1(ENDDQ,FVAL+FOPT+FALT),X'0000',AL1(FHIGH),AL3(0)              
         DC    AL1(DATEQ,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(CHKDQ,FALT+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MOARQ,FVAL+FOPT),X'1D00',AL1(0),AL3(0)                       
         DC    AL1(MMOSQ,FVAL+FOPT),X'1D00',AL1(0),AL3(0)                       
         DC    AL1(ESTNQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FRENQ,FVAL+FOPT),X'0000',AL1(FCANO),AL3(0)                   
         DC    AL1(OPT1Q,FVAL+FOPT),X'AD00',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'0113',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'0800',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'5D00',AL1(0),AL3(0)                       
         DC    AL1(CKDFQ,FVAL+FOPT),X'0324',AL1(0),AL3(0)                       
RT55X    DC    X'00',C'55'                                                      
                                                                                
RT57     DC    AL2(RT57X-*+3),AL1(57) 57 - BANK RECONCILIATION                  
         DC    AL1(0,HREQR)                                                     
         DC    CL22'CBK-BANK RECONCILN'                                         
         DC    AL1(VRQ09,XVRQ25)                                                
         DC    AL2(RT57X-*+1),AL1(HMENU)                                        
         DC    AL1(ACCTQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(MOARQ,FDDSO+FVAL+FOPT),X'1D00',AL1(0),AL3(0)                 
         DC    AL1(OPT1Q,FVAL),X'C800',AL1(0),AL3(0)                            
         DC    AL1(OPT2Q,FVAL+FOPT),X'D200',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'DE00',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'DB00',AL1(0),AL3(0)                       
         DC    AL1(OPT5Q,FVAL+FOPT),X'84DD',AL1(0),AL3(0)                       
         DC    AL1(OPT6Q,FVAL+FOPT),X'0121',AL1(0),AL3(0)                       
*        DC    AL1(OPT7Q,FVAL+FOPT),X'0123',AL1(0),AL3(0)                       
RT57X    DC    X'00',C'57'                                                      
                                                                                
RT58     DC    AL2(RT58X-*+3),AL1(58) 58 - CASH DISBURSEMENT                    
         DC    AL1(0,HREQR)                                                     
         DC    CL22'CSH-CASH DISBURSMENT'                                       
         DC    AL1(VRQ11,XVRQ03)                                                
         DC    AL2(RT58X-*+1),AL1(HMENU)                                        
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(CLI2Q,FVAL+FOPT+FALT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(STRDQ,FVAL+FOPT+FALT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(ENDDQ,FVAL+FOPT+FALT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(INC1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STATQ,FVAL+FOPT),X'010B',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'AF00',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'B100',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'D700',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'0800',AL1(0),AL3(0)                       
         DC    AL1(OPT6Q,FVAL+FOPT),X'C900',AL1(0),AL3(0)                       
RT58X    DC    X'00',C'58'                                                      
                                                                                
RT59     DC    AL2(RT59X-*+3),AL1(59) 59 - JOB EXCEPTION REPORT                 
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'JER-JOB EXCEPTION'                                          
         DC    AL1(VRQ02,XVRQ20)                                                
         DC    AL2(RT59X-*+1),AL1(HMENU)                                        
         DC    AL1(CLI1Q,FOPT+FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)            
         DC    AL1(PRD1Q,FOPT+FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)            
         DC    AL1(JOBQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                   
         DC    AL1(BGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MGRPQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(EXCPQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT1Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MEF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(BFLTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(USRFQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(STRDQ,FALT+FOPT),X'8C00',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FALT+FOPT),X'8C00',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'ED00',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'8500',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'B500',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'B700',AL1(0),AL3(0)                       
         DC    AL1(OPT5Q,FVAL+FOPT),X'E000',AL1(0),AL3(0)                       
RT59X    DC    X'00',C'59'                                                      
                                                                                
RT61     DC    AL2(RT61X-*+3),AL1(61) 61 - JOB AGING REPORT                     
         DC    AL1(0,HSOON+HNDWN+HREQR)                                         
         DC    CL22'JAR-JOB AGEING'                                             
         DC    AL1(VRQ02,XVRQ09)                                                
         DC    AL2(RT61X-*+1),AL1(HMENU)                                        
         DC    AL1(CLI1Q,FSOON+FVAL+FALL),X'8000',AL1(FNALL),AL3(0)             
         DC    AL1(PRD1Q,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(BGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MGRPQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(STAGQ,FALT+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENAGQ,FALT+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT1Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MEF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(BFLTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(TRNTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MOARQ,FVAL+FOPT),X'1D00',AL1(0),AL3(0)                       
         DC    AL1(XJOBQ,FVAL+FOPT),X'030A',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'9B00',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'A700',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'4200',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'5200',AL1(0),AL3(0)                       
         DC    AL1(OPT5Q,FVAL+FOPT),X'B300',AL1(0),AL3(0)                       
         DC    AL1(OPT6Q,FVAL+FOPT),X'6100',AL1(0),AL3(0)                       
         DC    AL1(OPT7Q,FVAL+FOPT),X'6200',AL1(0),AL3(0)                       
RT61X    DC    X'00',C'61'                                                      
                                                                                
RT62     DC    AL2(RT62X-*+3),AL1(62)  62 - INVENTORY BALANCE                   
         DC    AL1(0,HREQR)                                                     
         DC    CL22'IBR-INVENTORY BAL RPT'                                      
         DC    AL1(VRQ18,00)                                                    
         DC    AL2(RT62X-*+1),AL1(HMENU)                                        
         DC    AL1(CLI1Q,FVAL+FALL),X'8000',AL1(0),AL3(0)                       
         DC    AL1(PRD1Q,FVAL+FALL),X'8000',AL1(0),AL3(0)                       
         DC    AL1(MGRPQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MEF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FALT+FOPT),X'8C00',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'8F00',AL1(0),AL3(0)                       
RT62X    DC    X'00',C'62'                                                      
                                                                                
RT64     DC    AL2(RT64X-*+3),AL1(64) 64 - CLT EXPENDITURE ANLS                 
         DC    AL1(0,HREQR)                                                     
         DC    CL22'CEA-CLT EXPNDTURE ANLS'                                     
         DC    AL1(VRQ02,00)                                                    
         DC    AL2(RT64X-*+1),X'00'                                             
         DC    AL1(CLI1Q,FVAL+FALL),X'8000',AL1(0),AL3(0)                       
         DC    AL1(MGRPQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MEF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'B700',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'B500',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'ED00',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'A000',AL1(0),AL3(0)                       
         DC    AL1(OPT5Q,FVAL+FOPT),X'E000',AL1(0),AL3(0)                       
         DC    AL1(OPT6Q,FVAL+FOPT),X'0309',AL1(0),AL3(0)                       
         DC    AL1(OPT7Q,FVAL+FOPT),X'0116',AL1(0),AL3(0)                       
RT64X    DC    X'00',C'64'                                                      
                                                                                
RT65     DC    AL2(RT65X-*+3),AL1(65) 65 - JOB COMMENT LIST                     
         DC    AL1(0,HREQR)                                                     
         DC    CL22'JCL-JOB COMMENT LIST'                                       
         DC    AL1(00,00)                                                       
         DC    AL2(RT65X-*+1),X'00'                                             
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT65X    DC    X'00',C'65'                                                      
                                                                                
RT66     DC    AL2(RT66X-*+3),AL1(66) 66 - CLIENT BUDGET REPORT                 
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'CBR-CLIENT BUDGET REPT'                                     
         DC    AL1(VRQ02,XVRQ50)                                                
         DC    AL2(RT66X-*+1),AL1(HMENU)                                        
         DC    AL1(CLI1Q,FSOON+FVAL+FALL),X'8000',AL1(FNALL),AL3(0)             
         DC    AL1(PRD1Q,FOPT+FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)            
         DC    AL1(JOBQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                   
         DC    AL1(BGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT2Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MEF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(BFLTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FRHS+FVAL),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'7105',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'7106',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'010F',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'7107',AL1(0),AL3(0)                       
         DC    AL1(OPT5Q,FVAL+FOPT),X'ED00',AL1(0),AL3(0)                       
         DC    AL1(OPT6Q,FVAL+FOPT),X'8100',AL1(0),AL3(0)                       
         DC    AL1(OPT7Q,FVAL+FOPT),X'0117',AL1(0),AL3(0)                       
         DC    AL1(OPT8Q,FVAL+FOPT),X'3C00',AL1(0),AL3(0)                       
         DC    AL1(OPT9Q,FVAL+FOPT),X'011E',AL1(0),AL3(0)                       
         DC    AL1(OPTAQ,FVAL+FOPT),X'011F',AL1(0),AL3(0)                       
RT66X    DC    X'00',C'66'                                                      
                                                                                
RT67     DC    AL2(RT67X-*+3),AL1(67) 67 - NEW PRODUCTION JOB STATUS            
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'JST-JOB STATUS'                                             
         DC    AL1(VRQ02,XVRQ22)                                                
         DC    AL2(RT67X-*+1),AL1(HMENU)                                        
         DC    AL1(CLI1Q,FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)                 
         DC    AL1(PRD1Q,FOPT+FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)            
         DC    AL1(JOBQ,FOPT+FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)             
         DC    AL1(BGRPQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT2Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MEF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(BFLTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(STRDQ,FVAL+FOPT+FALT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(ENDDQ,FRHS+FALT+FVAL+FOPT),X'0000',AL1(0),AL3(0)             
         DC    AL1(MOARQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(XJOBQ,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(EXCPQ,FVAL+FOPT),X'F700',AL1(0),AL3(0)                       
         DC    AL1(RVSLQ,FVAL+FOPT),X'0201',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'EA00',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'F200',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'B700',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'A600',AL1(0),AL3(0)                       
         DC    AL1(OPT5Q,FVAL+FOPT),X'A900',AL1(0),AL3(0)                       
         DC    AL1(OPT6Q,FVAL+FOPT),X'AB00',AL1(0),AL3(0)                       
         DC    AL1(OPT7Q,FVAL+FOPT),X'1B00',AL1(0),AL3(0)                       
RT67X    DC    X'00',C'67'                                                      
                                                                                
RT70     DC    AL2(RT70X-*+3),AL1(70) 70 - USE & SALE TAX RATE LISTING          
         DC    AL1(0,HREQR)                                                     
         DC    CL22'TRL-TAX RATE LISTING'                                       
         DC    AL1(00,00)                                                       
         DC    AL2(RT70X-*+1),AL1(HMENU)                                        
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT70X    DC    X'00',C'70'                                                      
                                                                                
RT71     DC    AL2(RT71X-*+3),AL1(71) 71 - CLIENT PROFILES                      
         DC    AL1(0,HREQR)                                                     
         DC    CL22'JCP-CLIENT PROFILES'                                        
         DC    AL1(VRQ02,00)                                                    
         DC    AL2(RT71X-*+1),X'00'                                             
         DC    AL1(CLI1Q,FVAL+FALL),X'8000',AL1(0),AL3(0)                       
         DC    AL1(BGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'8700',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'F300',AL1(0),AL3(0)                       
RT71X    DC    X'00',C'71'                                                      
                                                                                
RT72     DC    AL2(RT72X-*+3),AL1(72) 72 - ACCOUNT RULES                        
         DC    AL1(0,HREQR)                                                     
         DC    CL22'ARL-ACCOUNT RULES LIST'                                     
         DC    AL1(VRQ18,00)                                                    
         DC    AL2(RT72X-*+1),X'00'                                             
         DC    AL1(CLI1Q,FVAL+FALL),X'8000',AL1(0),AL3(0)                       
         DC    AL1(PRD1Q,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(JOBQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                   
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT72X    DC    X'00',C'72'                                                      
                                                                                
RT73     DC    AL2(RT73X-*+3),AL1(73) 73 - ACCOUNT LISTING                      
         DC    AL1(0,HREQR)                                                     
         DC    CL22'ALS-ACCOUNT LISTING'                                        
         DC    AL1(00,XVRQ31)                                                   
         DC    AL2(RT73X-*+1),AL1(HMENU)                                        
         DC    AL1(UNITQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(BGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MGRPQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MEF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(BFLTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(XJOBQ,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(DJOBQ,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(SRTOQ,FVAL+FOPT),X'4100',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'D000',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'9500',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'B500',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'B700',AL1(0),AL3(0)                       
         DC    AL1(OPT5Q,FVAL+FOPT),X'B900',AL1(0),AL3(0)                       
         DC    AL1(OPT6Q,FVAL+FOPT),X'8B00',AL1(0),AL3(0)                       
         DC    AL1(OPT7Q,FVAL+FOPT),X'1000',AL1(0),AL3(0)                       
RT73X    DC    X'00',C'73'                                                      
                                                                                
RT74     DC    AL2(RT74X-*+3),AL1(74)  74 - REPORT PROFILE LIST                 
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'RPL-REPORT PROF LIST'                                       
         DC    AL1(00,00)                                                       
         DC    AL2(RT74X-*+1),AL1(HMENU)                                        
         DC    AL1(RPTRQ,FVAL+FALL),X'0000',AL1(0),AL3(0)                       
RT74X    DC    X'00',C'74'                                                      
                                                                                
RT75     DC    AL2(RT75X-*+3),AL1(75) 75 - ACCOUNT N&A LISTING                  
         DC    AL1(0,HREQR)                                                     
         DC    CL22'ANA-ACCOUNT N&&A LIST'                                      
         DC    AL1(00,00)                                                       
         DC    AL2(RT75X-*+1),AL1(HMENU)                                        
         DC    AL1(UNITQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(SRTOQ,FVAL+FOPT),X'8D00',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'9300',AL1(0),AL3(0)                       
RT75X    DC    X'00',C'75'                                                      
                                                                                
RT76     DC    AL2(RT76X-*+3),AL1(76)  76 - FILTER VALUES LIST                  
         DC    AL1(0,HREQR)                                                     
         DC    CL22'FVL-FILTER VALUES LIST'                                     
         DC    AL1(00,00)                                                       
         DC    AL2(RT76X-*+1),AL1(HMENU)                                        
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT76X    DC    X'00',C'76'                                                      
                                                                                
RT77     DC    AL2(RT77X-*+3),AL1(77) 77 - STICKY LABELS                        
         DC    AL1(0,HREQR)                                                     
         DC    CL22'ASY-STICKY LABELS'                                          
         DC    AL1(00,00)                                                       
         DC    AL2(RT77X-*+1),AL1(HMENU)                                        
         DC    AL1(UNITQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(SRTOQ,FVAL+FOPT),X'DA00',AL1(0),AL3(0)                       
         DC    AL1(ASTDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(AENDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'B700',AL1(0),AL3(0)                       
RT77X    DC    X'00',C'77'                                                      
                                                                                
RT78     DC    AL2(RT78X-*+3),AL1(78) 78 - MEDIA CODE LISTING                   
         DC    AL1(0,HREQR)                                                     
         DC    CL22'JMD-MEDIA LISTING'                                          
         DC    AL1(00,00)                                                       
         DC    AL2(RT78X-*+1),X'00'                                             
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT78X    DC    X'00',C'78'                                                      
                                                                                
RT79     DC    AL2(RT79X-*+3),AL1(79) 79 - ANALYSIS CODE LISTING                
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'JAL-ANALYSIS CODE LIST'                                     
         DC    AL1(00,00)                                                       
         DC    AL2(RT79X-*+1),AL1(HMENU)                                        
         DC    AL1(UNITQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(LEDGQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FSOON+FVAL),X'FD00',AL1(0),AL3(0)                      
RT79X    DC    X'00',C'79'                                                      
                                                                                
RT80     DC    AL2(RT80X-*+3),AL1(80) 80 - LIST REPORT                          
         DC    AL1(0,HREQR)                                                     
         DC    CL22'LST-LIST REPORT'                                            
         DC    AL1(00,00)                                                       
         DC    AL2(RT80X-*+1),AL1(HMENU)                                        
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT80X    DC    X'00',C'80'                                                      
                                                                                
RT81     DC    AL2(RT81X-*+3),AL1(81) 81 - TRIAL BALANCE                        
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'ATB-TRIAL BALANCE'                                          
         DC    AL1(00,00)                                                       
         DC    AL2(RT81X-*+1),AL1(HMENU)                                        
         DC    AL1(UNITQ,FSOON+FVAL),X'8000',AL1(0),AL3(0)                      
         DC    AL1(LEDGQ,FOPT+FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(MGRPQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MEF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MOARQ,FVAL+FOPT),X'1D00',AL1(0),AL3(0)                       
         DC    AL1(SEQNQ,FVAL+FOPT),X'0202',AL1(FNACO),AL3(0)                   
         DC    AL1(OPT1Q,FVAL+FOPT),X'8100',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'C500',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'D300',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FDDSO+FVAL+FOPT),X'43DD',AL1(0),AL3(0)                 
         DC    AL1(OPT5Q,FVAL+FOPT),X'2100',AL1(0),AL3(0)                       
         DC    AL1(OPT6Q,FVAL+FOPT),X'0122',AL1(0),AL3(0)                       
         DC    AL1(OPT7Q,FVAL+FOPT),X'3C00',AL1(0),AL3(0)                       
RT81X    DC    X'00',C'81'                                                      
                                                                                
RT82     DC    AL2(RT82X-*+3),AL1(82) 82 - ADVANCE STATEMENT                    
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'ADS-ADVANCE STATEMENT'                                      
         DC    AL1(VRQ27,00)                                                    
         DC    AL2(RT82X-*+1),AL1(HMENU)                                        
         DC    AL1(ACCTQ,FOPT+FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)            
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MOARQ,FVAL+FOPT),X'1D00',AL1(0),AL3(0)                       
         DC    AL1(MINAQ,FVAL+FOPT),X'2000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'A400',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'A000',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'0103',AL1(0),AL3(0)                       
RT82X    DC    X'00',C'82'                                                      
                                                                                
RT83     DC    AL2(RT83X-*+3),AL1(83) 83 - AGED CASH/RECEIVABLES                
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'RAG-RECEIVABLE AGEING'                                      
         DC    AL1(VRQ03,XVRQ07)                                                
         DC    AL2(RT83X-*+1),AL1(HMENU)                                        
         DC    AL1(ACCTQ,FOPT+FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)            
         DC    AL1(LISTQ,FVAL+FOPT+FALT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FDDSO+FVAL+FOPT),X'0000',AL1(0),AL3(0)                 
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MOARQ,FVAL+FOPT),X'1D00',AL1(0),AL3(0)                       
         DC    AL1(INC1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'8800',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'8A00',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'1900',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'1A00',AL1(0),AL3(0)                       
         DC    AL1(OPT5Q,FVAL+FOPT),X'8E00',AL1(0),AL3(0)                       
         DC    AL1(OPT6Q,FVAL+FOPT),X'9000',AL1(0),AL3(0)                       
         DC    AL1(OPT7Q,FVAL+FOPT),X'A400',AL1(0),AL3(0)                       
RT83X    DC    X'00',C'83'                                                      
                                                                                
RT84     DC    AL2(RT84X-*+3),AL1(84) 84 - OPEN ITEM TRIAL BAL                  
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'OTB-OPEN-ITEM TRL BAL'                                      
         DC    AL1(00,00)                                                       
         DC    AL2(RT84X-*+1),AL1(HMENU)                                        
         DC    AL1(UNITQ,FSOON+FVAL),X'8000',AL1(0),AL3(0)                      
         DC    AL1(LEDGQ,FSOON+FVAL),X'8000',AL1(0),AL3(0)                      
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(MGRPQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MEF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
RT84X    DC    X'00',C'84'                                                      
                                                                                
RT85     DC    AL2(RT85X-*+3),AL1(85) 85 - HISTORICAL ANALYSIS                  
         DC    AL1(0,HREQR)                                                     
         DC    CL22'AHS-HISTORICAL ANLS'                                        
         DC    AL1(00,XVRQ01)                                                   
         DC    AL2(RT85X-*+1),AL1(HMENU)                                        
         DC    AL1(UNITQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(MGRPQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MEF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(FNACO),AL3(0)                   
         DC    AL1(SEQNQ,FVAL+FOPT),X'0202',AL1(FNACO),AL3(0)                   
         DC    AL1(CLI2Q,FVAL+FOPT+FALT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(BKT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL),X'8900',AL1(0),AL3(0)                            
         DC    AL1(OPT5Q,FVAL+FOPT),X'A800',AL1(0),AL3(0)                       
RT85X    DC    X'00',C'85'                                                      
                                                                                
RT87     DC    AL2(RT87X-*+3),AL1(87) 87 - A/C HISTORY X-ANALYS                 
         DC    AL1(0,HREQR)                                                     
         DC    CL22'AHX-HIST CROSS-ANLS'                                        
         DC    AL1(00,XVRQ01)                                                   
         DC    AL2(RT87X-*+1),AL1(HMENU)                                        
         DC    AL1(UNITQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MEF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(SEQNQ,FVAL+FOPT),X'0202',AL1(FNACO),AL3(0)                   
         DC    AL1(CLI2Q,FVAL+FOPT+FALT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(BKT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL),X'8900',AL1(0),AL3(0)                            
         DC    AL1(OPT2Q,FVAL+FOPT),X'C100',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'9E00',AL1(0),AL3(0)                       
         DC    AL1(OPT5Q,FVAL+FOPT),X'A800',AL1(0),AL3(0)                       
RT87X    DC    X'00',C'87'                                                      
                                                                                
RT90     DC    AL2(RT90X-*+3),AL1(90)                                           
         DC    AL1(0,HREQR)                                                     
         DC    CL22'AIS-INCOME SUSPENSE'                                        
         DC    AL1(VRQ08,XVRQ36)                                                
         DC    AL2(RT90X-*+1),X'00'                                             
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(ACCTQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'5100',AL1(0),AL3(0)                       
RT90X    DC    X'00',C'8A'                                                      
                                                                                
RT92     DC    AL2(RT92X-*+3),AL1(92) 92 - DIRECT TIME ANALYSIS                 
         DC    AL1(0,HREQR)                                                     
         DC    CL22'DTN-DIRECT TIME ANLS'                                       
         DC    AL1(VRQ16,XVRQ01)                                                
         DC    AL2(RT92X-*+1),X'00'                                             
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(CNTRQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL),X'9D00',AL1(0),AL3(0)                            
         DC    AL1(OPT2Q,FVAL+FOPT),X'8400',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'8600',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'6300',AL1(0),AL3(0)                       
         DC    AL1(MTHDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT92X    DC    X'00',C'92'                                                      
                                                                                
RT93     DC    AL2(RT93X-*+3),AL1(93) 93 - PAYROLL RATE LIST                    
         DC    AL1(0,HREQR)                                                     
         DC    CL22'CPY-PAYROLL RATE LIST'                                      
         DC    AL1(VRQ16,00)                                                    
         DC    AL2(RT93X-*+1),X'00'                                             
         DC    AL1(ACCTQ,FVAL+FALL),X'8000',AL1(0),AL3(0)                       
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT+FOPT),X'8C00',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FALT+FOPT),X'8C00',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'B700',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'A401',AL1(0),AL3(0)                       
RT93X    DC    X'00',C'93'                                                      
                                                                                
RT94     DC    AL2(RT94X-*+3),AL1(94) 94 - PAYROLL COST ANALYSIS                
         DC    AL1(0,HREQR)                                                     
         DC    CL22'CPA-PAYROLL COST ANLS'                                      
         DC    AL1(VRQ16,XVRQ01)                                                
         DC    AL2(RT94X-*+1),X'00'                                             
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT+FOPT),X'8C00',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL),X'9D00',AL1(0),AL3(0)                            
         DC    AL1(OPT2Q,FVAL+FOPT),X'C300',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'F500',AL1(0),AL3(0)                       
RT94X    DC    X'00',C'94'                                                      
                                                                                
RT95     DC    AL2(RT95X-*+3),AL1(95) 95 - DPT EXPENSE ANAL                     
         DC    AL1(0,HREQR)                                                     
         DC    CL22'DEA-DPT EXPENSE ANLS'                                       
         DC    AL1(VRQ05,XVRQ01)                                                
         DC    AL2(RT95X-*+1),X'00'                                             
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT+FOPT),X'8C00',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL),X'9D00',AL1(0),AL3(0)                            
         DC    AL1(OPT3Q,FVAL+FOPT),X'F500',AL1(0),AL3(0)                       
RT95X    DC    X'00',C'95'                                                      
                                                                                
RT97     DC    AL2(RT97X-*+3),AL1(97) 97 - A/C DELETER REPORT                   
         DC    AL1(0,HREQR)                                                     
         DC    CL22'ADL-ACCOUNT DELETER'                                        
         DC    AL1(00,00)                                                       
         DC    AL2(RT97X-*+1),AL1(HMENU)                                        
         DC    AL1(UNITQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'A100',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'0128',AL1(0),AL3(0)                       
RT97X    DC    X'00',C'97'                                                      
                                                                                
RT98     DC    AL2(RT98X-*+3),AL1(98) 98 - TRANSACTION PEEL OFF                 
         DC    X'0031'                                                          
         DC    CL22'APL-TRANSACTION PEEL'                                       
         DC    AL1(00,XVRQ08)                                                   
         DC    AL2(RT98X-*+1),AL1(HMENU)                                        
         DC    AL1(UNTLQ,14),X'8000',AL1(0),AL3(0)                              
         DC    AL1(LEDGQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(ACCTQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDMQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'1100',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'1200',AL1(0),AL3(0)                       
RT98X    DC    X'00',C'98'                                                      
                                                                                
RT100    DC    AL2(RT100X-*+3),AL1(100)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'CPF-CLT PROFITABILITY'                                      
         DC    AL1(VRQ06,XVRQ26)                                                
         DC    AL2(RT100X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(BUDTQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'9F00',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'9C00',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'F100',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'BE00',AL1(0),AL3(0)                       
         DC    AL1(MTHDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT100X   DC    X'00',C'9A'                                                      
                                                                                
RT103    DC    AL2(RT103X-*+3),AL1(103)                                         
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'CAR-COST ALLOCATION'                                        
         DC    AL1(VRQ16,XVRQ11)                                                
         DC    AL2(RT103X-*+1),X'00'                                            
         DC    AL1(ACCTQ,23),X'8000',AL1(FNALL),AL3(0)                          
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'7103',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'EC00',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'FB00',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'0500',AL1(0),AL3(0)                       
         DC    AL1(OPT5Q,FVAL+FOPT),X'0111',AL1(0),AL3(0)                       
         DC    AL1(OPT6Q,FVAL+FOPT),X'9801',AL1(0),AL3(0)                       
         DC    AL1(OPT7Q,FVAL+FOPT),X'0129',AL1(0),AL3(0)                       
         DC    AL1(MTHDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT103X   DC    X'00',C'CA'                                                      
                                                                                
RT104    DC    AL2(RT104X-*+3),AL1(104)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'CHR-COST HOURLY RATES'                                      
         DC    AL1(VRQ16,00)                                                    
         DC    AL2(RT104X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL+FOPT),X'8C00',AL1(0),AL3(0)                       
         DC    AL1(MTHDQ,FVAL+FOPT),X'0112',AL1(0),AL3(0)                       
RT104X   DC    X'00',C'CH'                                                      
                                                                                
RT105    DC    AL2(RT105X-*+3),AL1(105)                                         
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'APG-APG RULES LISTING'                                      
         DC    AL1(00,00)                                                       
         DC    AL2(RT105X-*+1),X'00'                                            
         DC    AL1(UNITQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
RT105X   DC    X'00',C'AP'                                                      
                                                                                
RT106    DC    AL2(RT106X-*+3),AL1(106)                                         
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'GST-GST REPORT       '                                      
         DC    AL1(VRQ35,XVRQ47)       U/L=SG                                   
         DC    AL2(RT106X-*+1),AL1(HMENU)                                       
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ASTDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(AENDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MOARQ,FVAL+FOPT),X'1D00',AL1(0),AL3(0)                       
         DC    AL1(RVSLQ,FVAL+FOPT),X'0201',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'0108',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'F901',AL1(0),AL3(0)                       
RT106X   DC    X'00',C'GT'                                                      
                                                                                
RT107    DC    AL2(RT107X-*+3),AL1(107)                                         
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'AAR-ACCOUNT RECEIVABLE'                                     
         DC    AL1(VRQ03,00)       U/L=SR                                       
         DC    AL2(RT107X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FOPT+FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)            
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'4500',AL1(0),AL3(0)                       
RT107X   DC    X'00',C'AR'                                                      
                                                                                
RT110    DC    AL2(RT110X-*+3),AL1(110)                                         
         DC    AL1(HDDSO,HSOON+HREQR)                                           
         DC    CL22'A6E- ESTIMATE LIST'                                         
         DC    AL1(VRQ02,XVRQ13)                                                
         DC    AL2(RT110X-*+1),AL1(HMENU)                                       
         DC    AL1(CLI1Q,FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)                 
         DC    AL1(PRD1Q,FOPT+FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)            
         DC    AL1(JOBQ,FOPT+FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)             
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'AA00',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'AC00',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'AE00',AL1(0),AL3(0)                       
RT110X   DC    X'00',C'6E'                                                      
                                                                                
RT111    DC    AL2(RT111X-*+3),AL1(111)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'BGL-BUDGET LIST'                                            
         DC    AL1(00,XVRQ01)                                                   
         DC    AL2(RT111X-*+1),X'00'                                            
         DC    AL1(UNITQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(BUDTQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(BKT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT+FOPT),X'8C00',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FALT+FOPT),X'8C00',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'D600',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'4900',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'4A00',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'4B00',AL1(0),AL3(0)                       
         DC    AL1(OPT5Q,FVAL+FOPT),X'4C00',AL1(0),AL3(0)                       
RT111X   DC    X'00',C'B1'                                                      
                                                                                
RT112    DC    AL2(RT112X-*+3),AL1(112)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'BCM-BUDGET COMPARSION'                                      
         DC    AL1(00,XVRQ17)                                                   
         DC    AL2(RT112X-*+1),X'00'                                            
         DC    AL1(UNITQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(LVL1Q,FVAL+FOPT),X'DF00',AL1(0),AL3(0)                       
         DC    AL1(CURRQ,FVAL+FOPT),X'E200',AL1(0),AL3(0)                       
         DC    AL1(YTDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                        
         DC    AL1(YRLYQ,FVAL+FOPT+FALT),X'E400',AL1(0),AL3(0)                  
         DC    AL1(DATAQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT6Q,FVAL+FOPT),X'EE00',AL1(0),AL3(0)                       
RT112X   DC    X'00',C'B2'                                                      
                                                                                
RT113    DC    AL2(RT113X-*+3),AL1(113)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'MBL-MNTHLY BUDGET LIST'                                     
         DC    AL1(00,XVRQ01)                                                   
         DC    AL2(RT113X-*+1),X'00'                                            
         DC    AL1(UNITQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(LVL1Q,FVAL+FOPT),X'DF00',AL1(0),AL3(0)                       
         DC    AL1(BUDTQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(COMPQ,FVAL),X'E600',AL1(0),AL3(0)                            
RT113X   DC    X'00',C'B3'                                                      
                                                                                
RT114    DC    AL2(RT114X-*+3),AL1(114)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'BDU-BUDGET DATA UTIL.'                                      
         DC    AL1(00,XVRQ33)                                                   
         DC    AL2(RT114X-*+1),X'00'                                            
         DC    AL1(UNITQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(LEDGQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(FBUDQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(TBUDQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(BKT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL),X'2F00',AL1(0),AL3(0)                            
         DC    AL1(OPT2Q,FVAL+FOPT),X'3000',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'3D00',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT114X   DC    X'00',C'B4'                                                      
                                                                                
RT115    DC    AL2(RT115X-*+3),AL1(115)                                         
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'IJR-INPUT JOURNAL RPT'                                      
         DC    AL1(00,XVRQ05)                                                   
         DC    AL2(RT115X-*+1),AL1(HMENU)                                       
         DC    AL1(BATGQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(BATTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(BATRQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(PERSQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MOARQ,FVAL+FOPT+FALT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(OPT1Q,FVAL+FOPT),X'0301',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'0302',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'0303',AL1(0),AL3(0)                       
         DC    AL1(OPT5Q,FVAL+FOPT),X'0304',AL1(0),AL3(0)                       
         DC    AL1(OPT6Q,FVAL+FOPT),X'0305',AL1(0),AL3(0)                       
         DC    AL1(OPT7Q,FVAL+FOPT),X'0306',AL1(0),AL3(0)                       
         DC    AL1(OPT8Q,FVAL+FOPT),X'0204',AL1(0),AL3(0)                       
RT115X   DC    X'00',C'IJ'                                                      
                                                                                
RT116    DC    AL2(RT116X-*+3),AL1(116)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'CAL-CONVERSION LISTING'                                     
         DC    AL1(00,00)                                                       
         DC    AL2(RT116X-*+1),X'00'                                            
         DC    AL1(UNITQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(OPT1Q,FVAL+FOPT),X'020B',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'020C',AL1(0),AL3(0)                       
RT116X   DC    X'00',C'7A'                                                      
                                                                                
                                                                                
RT117    DC    AL2(RT117X-*+3),AL1(117)                                         
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'BLR-BILLING RECAP'                                          
         DC    AL1(VRQ03,00)       U/L=SR                                       
         DC    AL2(RT117X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FOPT+FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)            
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT117X   DC    X'00',C'BR'                                                      
                                                                                
                                                                                
RT118    DC    AL2(RT118X-*+3),AL1(118)                                         
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'ATX-TIME MOVE'                                              
         DC    AL1(VRQ02,XVRQ32)                                                
         DC    AL2(RT118X-*+1),X'00'                                            
         DC    AL1(FUNTQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(FLDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(FACTQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(FWRKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
*        DC    AL1(FMOAQ,FVAL+FOPT),X'1D00',AL1(0),AL3(0)                       
         DC    AL1(FCNTQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(FTIMQ,FVAL+FOPT),X'0123',AL1(0),AL3(0)                       
         DC    AL1(FPERQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FHRSQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FAMTQ,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(TUNTQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(TLDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(TACTQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(TWRKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(TMOAQ,FVAL+FOPT),X'8C00',AL1(0),AL3(0)                       
         DC    AL1(TTIMQ,FVAL+FOPT),X'0123',AL1(0),AL3(0)                       
         DC    AL1(REPVQ,FVAL+FOPT),X'1800',AL1(0),AL3(0)                       
         DC    AL1(REVRQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(DOWNQ,FVAL+FOPT),X'012D',AL1(0),AL3(0)                       
         DC    AL1(LIVEQ,FVAL+FOPT),X'012D',AL1(0),AL3(0)                       
         DC    AL1(NARRQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT118X   DC    X'00',C'TX'                                                      
                                                                                
RT120    DC    AL2(RT120X-*+3),AL1(120)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'CTB-COKE TRIAL BALANCE'                                     
         DC    AL1(VRQ25,00)       SET U/L TO SE/QOPT2=A                        
         DC    AL2(RT120X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL),X'3200',AL1(0),AL3(0)                            
RT120X   DC    X'00',C'CB'                                                      
                                                                                
RT121    DC    AL2(RT121X-*+3),AL1(121)                                         
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'JSR- JOB STATUS'                                            
         DC    AL1(VRQ02,00)       SET U/L TO SJ/QOPT2=Y                        
         DC    AL2(RT121X-*+1),X'00'                                            
         DC    AL1(CLI1Q,FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)                 
         DC    AL1(PRD1Q,FOPT+FVAL+FALL),X'8000',AL1(0),AL3(0)                  
         DC    AL1(JOBQ,FOPT+FVAL+FALL),X'8000',AL1(0),AL3(0)                   
RT121X   DC    X'00',C'JS'                                                      
                                                                                
*                                                                               
RT122    DC    AL2(RT122X-*+3),AL1(122)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'AVE-Vendor Enrollment'                                      
         DC    AL1(VRQ29,00)                                                    
         DC    AL2(RT122X-*+1),X'00'                                            
         DC    AL1(LEDGQ,FOPT+FVAL),X'8000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FOPT+FVAL),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FOPT+FVAL),X'0000',AL1(0),AL3(0)                       
RT122X   DC    X'00',C'VE'                                                      
                                                                                
RT125    DC    AL2(RT125X-*+3),AL1(125)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'DSC-DIV/SECT CLT PROFT'                                     
         DC    AL1(VRQ26,00)        SET QOPT2=A                                 
         DC    AL2(RT125X-*+1),X'00'                                            
         DC    AL1(STRDQ,FALT+FVAL),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FALT+FVAL),X'0000',AL1(0),AL3(0)                       
RT125X   DC    X'00',C'CP'                                                      
                                                                                
RT127    DC    AL2(RT127X-*+3),AL1(127)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'CS -CLIENT STATEMENT'                                       
         DC    AL1(VRQ03,00)       U/L=SR                                       
         DC    AL2(RT127X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(BDATQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'3E00',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'3F00',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'4000',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'3300',AL1(0),AL3(0)                       
         DC    AL1(NAR1Q,FVAL+FOPT),X'0B00',AL1(0),AL3(0)                       
RT127X   DC    X'00',C'CS'                                                      
                                                                                
RT129    DC    AL2(RT129X-*+3),AL1(129)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'COX-COKE EXPEND RPT'                                        
         DC    AL1(VRQ25,XVRQ34)   SET U/L TO SE                                
         DC    AL2(RT129X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(AGYFQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MEF2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(BUF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(TYRQ,FVAL),X'0000',AL1(0),AL3(0)                             
         DC    AL1(OPT1Q,FVAL+FOPT),X'2500',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'3100',AL1(0),AL3(0)                       
RT129X   DC    X'00',C'CX'                                                      
                                                                                
RT131    DC    AL2(RT131X-*+3),AL1(131)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'UAR-UNDISBURSED ANLS'                                       
         DC    AL1(VRQ08,00)                                                    
         DC    AL2(RT131X-*+1),AL1(HMENU)                                       
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MOARQ,FVAL+FOPT),X'1D00',AL1(0),AL3(0)                       
         DC    AL1(DRNGQ,FVAL+FOPT+FALT),X'C000',AL1(0),AL3(0)                  
         DC    AL1(STATQ,FVAL+FOPT),X'010B',AL1(0),AL3(0)                       
         DC    AL1(RVSLQ,FVAL+FOPT),X'0201',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'DC00',AL1(0),AL3(0)                       
         DC    AL1(OPT5Q,FVAL+FOPT),X'0700',AL1(0),AL3(0)                       
         DC    AL1(OPT7Q,FVAL+FOPT),X'B100',AL1(0),AL3(0)                       
RT131X   DC    X'00',C'C1'                                                      
                                                                                
RT132    DC    AL2(RT132X-*+3),AL1(132)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'TSE-TIMESHEET EDIT'                                         
         DC    AL1(VRQ16,XVRQ23)                                                
         DC    AL2(RT132X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT+FVAL),X'0000',AL1(0),AL3(0)                       
         DC    AL1(DLYUQ,FVAL+FOPT),X'0B00',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'CE00',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'9600',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'B700',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'0C00',AL1(0),AL3(0)                       
         DC    AL1(OPT5Q,FVAL+FOPT),X'2B00',AL1(0),AL3(0)                       
         DC    AL1(OPT6Q,FVAL+FOPT),X'6000',AL1(0),AL3(0)                       
         DC    AL1(OPT7Q,FVAL+FOPT),X'7800',AL1(0),AL3(0)                       
RT132X   DC    X'00',C'C2'                                                      
                                                                                
RT133    DC    AL2(RT133X-*+3),AL1(133)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'TSH-TIMESHEET'                                              
         DC    AL1(VRQ16,00)                                                    
         DC    AL2(RT133X-*+1),AL1(HMENU)                                       
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT133X   DC    X'00',C'C3'                                                      
                                                                                
RT134    DC    AL2(RT134X-*+3),AL1(134)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'PTC-PROJ TIME CONFIRM'                                      
         DC    AL1(VRQ16,00)                                                    
         DC    AL2(RT134X-*+1),AL1(HMENU)                                       
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(NAR1Q,FVAL+FOPT),X'0B00',AL1(0),AL3(0)                       
RT134X   DC    X'00',C'C4'                                                      
                                                                                
RT135    DC    AL2(RT135X-*+3),AL1(135)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'DTS-DAILY TIMESHEET'                                        
         DC    AL1(VRQ16,00)                                                    
         DC    AL2(RT135X-*+1),AL1(HMENU)                                       
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'9600',AL1(0),AL3(0)                       
RT135X   DC    X'00',C'C5'                                                      
                                                                                
RT136    DC    AL2(RT136X-*+3),AL1(136)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'DCS-DEPT COST STATEMNT'                                     
         DC    AL1(VRQ16,XVRQ01)   SET U/L TO 1R                                
         DC    AL2(RT136X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
RT136X   DC    X'00',C'C6'                                                      
                                                                                
RT137    DC    AL2(RT137X-*+3),AL1(137)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'WTS-WEEKLY TIMESHEET'                                       
         DC    AL1(VRQ16,0)                                                     
         DC    AL2(RT137X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'D601',AL1(0),AL3(0)                       
RT137X   DC    X'00',C'CT'                                                      
                                                                                
RT138    DC    AL2(RT138X-*+3),AL1(138)                                         
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'CRR-CHARGE RATE REPORT'                                     
         DC    AL1(00,00)                                                       
         DC    AL2(RT138X-*+1),X'00'                                            
         DC    AL1(OFFCQ,FSOON+FVAL+FALL),X'0000',AL1(0),AL3(0)                 
         DC    AL1(DPT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(TASKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL+FOPT),X'FE02',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'FE02',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'6400',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'0500',AL1(0),AL3(0)                       
RT138X   DC    X'00',C'C8'                                                      
                                                                                
RT145    DC    AL2(RT145X-*+3),AL1(145)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'PIR-PAID ITEMS REPORT'                                      
         DC    AL1(VRQ25,XVRQ21)   SET U/L TO SE                                
         DC    AL2(RT145X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL),X'2200',AL1(0),AL3(0)                            
         DC    AL1(OPT5Q,FVAL+FOPT),X'2E01',AL1(0),AL3(0)                       
RT145X   DC    X'00',C'CI'                                                      
                                                                                
RT146    DC    AL2(RT146X-*+3),AL1(146)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'CLI-CLIENT INVESTMENT'                                      
         DC    AL1(VRQ24,00)       SET U/L TO SR                                
         DC    AL2(RT146X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(MOARQ,FVAL),X'1D00',AL1(0),AL3(0)                            
RT146X   DC    X'00',C'CL'                                                      
                                                                                
RT147    DC    AL2(RT147X-*+3),AL1(147)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'IAR-INTERAGENCY REPORT'                                     
         DC    AL1(00,00)                                                       
         DC    AL2(RT147X-*+1),X'00'                                            
         DC    AL1(UNITQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(ACCTQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(OPT2Q,FVAL+FOPT),X'D200',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'7200',AL1(0),AL3(0)                       
RT147X   DC    X'00',C'E1'                                                      
                                                                                
RT148    DC    AL2(RT148X-*+3),AL1(148) 148 - LEDGER-ANALYSIS                   
         DC    AL1(HDDSO,HSOON)                                                 
         DC    CL22'ALA-LEDGER ANALYSIS'                                        
         DC    AL1(VRQ08,00)       SET UNIT TO 'S'                              
         DC    AL2(RT148X-*+1),X'00'                                            
         DC    AL1(UNITQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(LEDGQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(MOARQ,FVAL+FOPT),X'1D00',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'FE03',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'FE04',AL1(0),AL3(0)                       
RT148X   DC    X'00',C'LA'                                                      
                                                                                
RT149    DC    AL2(RT149X-*+3),AL1(149)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'GFX-GENERAL FOODS INT'                                      
         DC    AL1(0,0)                                                         
         DC    AL2(RT149X-*+1),X'00'                                            
         DC    AL1(RMONQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL),X'3900',AL1(0),AL3(0)                            
         DC    AL1(OPT3Q,FVAL+FOPT),X'3A00',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'3B00',AL1(0),AL3(0)                       
RT149X   DC    X'00',C'GX'                                                      
                                                                                
RT150    DC    AL2(RT150X-*+3),AL1(150)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'BTI-BANKERS TRUST INT'                                      
         DC    AL1(VRQ02,00)                                                    
         DC    AL2(RT150X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(BGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(MNTHQ,FVAL),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'1E00',AL1(0),AL3(0)                       
RT150X   DC    X'00',C'BT'                                                      
                                                                                
RT151    DC    AL2(RT151X-*+3),AL1(151)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'GLS-GEN LDGR SUMMARY'                                       
         DC    AL1(00,00)                                                       
         DC    AL2(RT151X-*+1),AL1(HMENU)                                       
         DC    AL1(UNITQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'C500',AL1(0),AL3(0)                       
RT151X   DC    X'00',C'G1'                                                      
                                                                                
RT152    DC    AL2(RT152X-*+3),AL1(152)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'GLP-GEN LDGR P&&L'                                          
         DC    AL1(00,00)                                                       
         DC    AL2(RT152X-*+1),AL1(HMENU)                                       
         DC    AL1(UNITQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'C400',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT152X   DC    X'00',C'G2'                                                      
                                                                                
RT153    DC    AL2(RT153X-*+3),AL1(153)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'GLT-GEN LDGR STATEMENT'                                     
         DC    AL1(VRQ17,00)                                                    
         DC    AL2(RT153X-*+1),AL1(HMENU)                                       
         DC    AL1(LEDGQ,FVAL+FALL),X'8000',AL1(0),AL3(0)                       
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MOARQ,FVAL+FOPT),X'1D00',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'0E00',AL1(0),AL3(0)                       
RT153X   DC    X'00',C'G3'                                                      
                                                                                
RT154    DC    AL2(RT154X-*+3),AL1(154)                                         
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'TAR-TIMESHEET AUDIT'                                        
         DC    AL1(VRQ16,XVRQ42)      BEFORE AND AFTER                          
         DC    AL2(RT154X-*+1),X'10'                                            
         DC    AL1(ACCTQ,FVAL+FALL),X'8000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ASTDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(AENDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL),X'1F00',AL1(0),AL3(0)                            
         DC    AL1(OPT2Q,FVAL+FOPT),X'1C00',AL1(0),AL3(0)                       
         DC    AL1(OPT7Q,FVAL+FOPT),X'030D',AL1(0),AL3(0)                       
RT154X   DC    X'00',C'TD'                                                      
                                                                                
RT160    DC    AL2(RT160X-*+3),AL1(160)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'FAL-FEE ALLOCATION'                                         
         DC    AL1(VRQ23,XVRQ19)      BEFORE AND AFTER                          
         DC    AL2(RT160X-*+1),X'10'                                            
         DC    AL1(CLI5Q,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(RMONQ,FALT),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'0200',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'0300',AL1(0),AL3(0)                       
RT160X   DC    X'00',C'FA'                                                      
                                                                                
RT161    DC    AL2(RT161X-*+3),AL1(161)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'FAR-FEE ADJUST REPORT'                                      
         DC    AL1(VRQ34,00)                                                    
         DC    AL2(RT161X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'010C',AL1(0),AL3(0)                       
RT161X   DC    X'00',C'FE'                                                      
                                                                                
RT162    DC    AL2(RT162X-*+3),AL1(162)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'FIN-FINANCIAL STATMNTS'                                     
         DC    AL1(00,XVRQ21)                                                   
         DC    AL2(RT162X-*+1),X'00'                                            
         DC    AL1(UNITQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(LEDGQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT1Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(STRDQ,FALT+FVAL),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(ENDDQ,FALT+FVAL),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(RFMTQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(FLT4Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(SELTQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT5Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(OPT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT6Q,FVAL+FOPT),X'0203',AL1(FAPGS),AL3(0)                   
         DC    AL1(SALRQ,FVAL+FOPT),X'020D',AL1(FNSEC),AL3(0)                   
         DC    AL1(CCNVQ,FVAL+FOPT),X'2600',AL1(0),AL3(0)                       
         DC    AL1(BUF2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(NAR2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OVRHQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MTHDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT162X   DC    X'00',C'FI'                                                      
                                                                                
RT163    DC    AL2(RT163X-*+3),AL1(163)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'INV-INVOICE SUMMARY'                                        
         DC    AL1(VRQ24,XVRQ21)                                                
         DC    AL2(RT163X-*+1),X'00'                                            
         DC    AL1(RFMTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(STRDQ,FALT+FVAL),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FALT+FVAL),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT6Q,FVAL+FOPT),X'0203',AL1(FAPGS),AL3(0)                   
         DC    AL1(BUF2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT163X   DC    X'00',C'IV'                                                      
                                                                                
RT164    DC    AL2(RT164X-*+3),AL1(164)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'PBI-PROD BILLING INTER'                                     
         DC    AL1(VRQ02,XVRQ28)                                                
         DC    AL2(RT164X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(BGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(NUMBQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'1E00',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT164X   DC    X'00',C'I1'                                                      
                                                                                
RT166    DC    AL2(RT166X-*+3),AL1(166)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'JBI-JOB BILLING INTER'                                      
         DC    AL1(VRQ02,00)                                                    
         DC    AL2(RT166X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'1E00',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'4700',AL1(0),AL3(0)                       
RT166X   DC    X'00',C'I3'                                                      
                                                                                
RT167    DC    AL2(RT167X-*+3),AL1(167)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'JIR-JOB INTERFACE RPT'                                      
         DC    AL1(VRQ02,00)                                                    
         DC    AL2(RT167X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'1E00',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'4700',AL1(0),AL3(0)                       
RT167X   DC    X'00',C'I4'                                                      
                                                                                
RT168    DC    AL2(RT168X-*+3),AL1(168)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'BIR-BILLED ITEMS RPRT'                                      
         DC    AL1(VRQ25,XVRQ30)   SET U/L TO SE                                
         DC    AL2(RT168X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(TMONQ,FVAL+FOPT),X'2300',AL1(0),AL3(0)                       
         DC    AL1(TYRQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                        
         DC    AL1(OPT1Q,FVAL+FOPT),X'D200',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'2200',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'2400',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'2A00',AL1(0),AL3(0)                       
*        DC    AL1(OPT5Q,FVAL+FOPT),X'2E00',AL1(0),AL3(0)                       
         DC    AL1(OPT6Q,FVAL+FOPT),X'0124',AL1(0),AL3(0)                       
RT168X   DC    X'00',C'IC'                                                      
                                                                                
RT169    DC    AL2(RT169X-*+3),AL1(169)                                         
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'BEI-BILLING EST INTER'                                      
         DC    AL1(VRQ02,XVRQ37)   SET U/L TO SJ                                
         DC    AL2(RT169X-*+1),X'00'                                            
         DC    AL1(CLI1Q,FVAL+FALL),X'8000',AL1(0),AL3(0)                       
         DC    AL1(PRD1Q,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(JOBQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                   
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(BGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'1E00',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'0307',AL1(0),AL3(0)                       
RT169X   DC    X'00',C'IR'                                                      
                                                                                
RT170    DC    AL2(RT170X-*+3),AL1(170) 170 M2 - MANPOWER REPORTS               
         DC    AL1(0,HREQR)                                                     
         DC    CL22'MPW-MANPOWER RPT'                                           
         DC    AL1(00,XVRQ21)                                                   
         DC    AL2(RT170X-*+1),X'00'                                            
         DC    AL1(UNITQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(LEDGQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT1Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(RFMTQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(FLT2Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(STRDQ,FALT+FVAL),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(ENDDQ,FALT+FVAL),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(SELTQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT5Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(BUF2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT6Q,FVAL+FOPT),X'0203',AL1(FAPGS),AL3(0)                   
         DC    AL1(NAR2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MTHDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT170X   DC    X'00',C'M2'                                                      
                                                                                
RT171    DC    AL2(RT171X-*+3),AL1(171)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'BSI-BILLING SUMM INTER'                                     
         DC    AL1(VRQ02,00)       SET U/L TO SJ                                
         DC    AL2(RT171X-*+1),X'00'                                            
         DC    AL1(CLI1Q,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(PRD1Q,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(JOBQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                   
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'1E00',AL1(0),AL3(0)                       
RT171X   DC    X'00',C'I5'                                                      
                                                                                
RT173    DC    AL2(RT173X-*+3),AL1(173)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'PFI-PRODCTN FEED INTER'                                     
         DC    AL1(VRQ08,00)       SET U/L TO S                                 
         DC    AL2(RT173X-*+1),X'00'                                            
         DC    AL1(MOARQ,FVAL+FOPT),X'1D00',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'1E00',AL1(0),AL3(0)                       
RT173X   DC    X'00',C'I7'                                                      
                                                                                
RT174    DC    AL2(RT174X-*+3),AL1(174)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'PVI-PRODCTN VEND INTER'                                     
         DC    AL1(VRQ32,00)          SET U/L TO SV                             
         DC    AL2(RT174X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'1E00',AL1(0),AL3(0)                       
RT174X   DC    X'00',C'I8'                                                      
                                                                                
RT175    DC    AL2(RT175X-*+3),AL1(175) PA - PROJECT ALLOC                      
         DC    AL1(0,HREQR)                                                     
         DC    CL22'PAL-PROJECT ALLOCATN'                                       
         DC    AL1(VRQ16,00)                                                    
         DC    AL2(RT175X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'9200',AL1(0),AL3(0)                       
RT175X   DC    X'00',C'PA'                                                      
                                                                                
RT176    DC    AL2(RT176X-*+3),AL1(176)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'GFI-GENERAL FOODS INT '                                     
         DC    AL1(VRQ02,00)                                                    
         DC    AL2(RT176X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(BGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'1E00',AL1(0),AL3(0)                       
RT176X   DC    X'00',C'GF'                                                      
                                                                                
RT177    DC    AL2(RT177X-*+3),AL1(177) PC - PROJECT CONTROL                    
         DC    AL1(0,HREQR)                                                     
         DC    CL22'PCR-PROJECT CONTROL'                                        
         DC    AL1(VRQ22,XVRQ01)                                                
         DC    AL2(RT177X-*+1),X'00'                                            
         DC    AL1(CLDVQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(OFDPQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(APROQ,FVAL+FOPT),X'E800',AL1(0),AL3(0)                       
         DC    AL1(BDIVQ,FVAL+FOPT),X'E800',AL1(0),AL3(0)                       
         DC    AL1(CDPTQ,FVAL+FOPT),X'E800',AL1(0),AL3(0)                       
         DC    AL1(DSDPQ,FVAL+FOPT),X'E800',AL1(0),AL3(0)                       
         DC    AL1(ETSKQ,FVAL+FOPT),X'E800',AL1(0),AL3(0)                       
RT177X   DC    X'00',C'PC'                                                      
                                                                                
RT178    DC    AL2(RT178X-*+3),AL1(178)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'AIA-MCKIM INTERFACE   '                                     
         DC    AL1(VRQ08,00)       SET U/L TO S                                 
         DC    AL2(RT178X-*+1),X'00'                                            
         DC    AL1(ASTDQ,FVAL),X'FE02',AL1(0),AL3(0)                            
         DC    AL1(AENDQ,FVAL),X'FE02',AL1(0),AL3(0)                            
         DC    AL1(MOARQ,FVAL+FOPT),X'1D00',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'D200',AL1(0),AL3(0)                       
RT178X   DC    X'00',C'IA'                                                      
                                                                                
RT179    DC    AL2(RT179X-*+3),AL1(179)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'PME-PHILLIP MORRIS EST'                                     
         DC    AL1(VRQ02,00)       SET U/L TO SJ                                
         DC    AL2(RT179X-*+1),X'00'                                            
         DC    AL1(CLI1Q,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(PRD1Q,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(JOBQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                        
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'D200',AL1(0),AL3(0)                       
RT179X   DC    X'00',C'PM'                                                      
                                                                                
RT180    DC    AL2(RT180X-*+3),AL1(180)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'AID-DONER INTERFACE   '                                     
         DC    AL1(VRQ08,00)       SET U/L TO S                                 
         DC    AL2(RT180X-*+1),X'00'                                            
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'D200',AL1(0),AL3(0)                       
RT180X   DC    X'00',C'ID'                                                      
                                                                                
RT181    DC    AL2(RT181X-*+3),AL1(181)                                         
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'ACS-ANLS CODE SUMMARY'                                      
         DC    AL1(VRQ18,XVRQ24)                                                
         DC    AL2(RT181X-*+1),AL1(HMENU)                                       
         DC    AL1(CLI1Q,FSOON+FVAL+FALL),X'8000',AL1(FNALL),AL3(0)             
         DC    AL1(PRD1Q,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(JOBQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                   
         DC    AL1(LISTQ,FVAL+FOPT+FALT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(BGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MGRPQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT2Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MEF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(TRNTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(RVSLQ,FVAL+FOPT),X'0201',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'D400',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'1300',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'1400',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'1500',AL1(0),AL3(0)                       
         DC    AL1(OPT5Q,FVAL+FOPT),X'1600',AL1(0),AL3(0)                       
RT181X   DC    X'00',C'P1'                                                      
                                                                                
RT182    DC    AL2(RT182X-*+3),AL1(182) P2 - ORDER + EST SUMMARY                
         DC    AL1(0,HREQR)                                                     
         DC    CL22'OES-ORDER+EST SUMMARY'                                      
         DC    AL1(VRQ18,00)                                                    
         DC    AL2(RT182X-*+1),AL1(HMENU)                                       
         DC    AL1(CLI1Q,FVAL+FALL),X'8000',AL1(0),AL3(0)                       
         DC    AL1(PRD1Q,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(MGRPQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MEF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'0600',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'B500',AL1(0),AL3(0)                       
RT182X   DC    X'00',C'P2'                                                      
                                                                                
RT183    DC    AL2(RT183X-*+3),AL1(183) P3 - ORDER LIST                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'ODL-ORDER LIST'                                             
         DC    AL1(00,XVRQ38)                                                   
         DC    AL2(RT183X-*+1),AL1(HMENU)                                       
         DC    AL1(MGRPQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(MEF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'CC00',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'0F00',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'4400',AL1(0),AL3(0)                       
RT183X   DC    X'00',C'P3'                                                      
                                                                                
RT184    DC    AL2(RT184X-*+3),AL1(184) P4 - JOB ORDER DETAIL                   
         DC    AL1(0,HREQR)                                                     
         DC    CL22'JOD-JOB ORDER DETAIL'                                       
         DC    AL1(00,00)                                                       
         DC    AL2(RT184X-*+1),AL1(HMENU)                                       
         DC    AL1(UNITQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(ACCTQ,FVAL+FALL),X'8000',AL1(0),AL3(0)                       
         DC    AL1(MGRPQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MEF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0) OFFICE=OPT/XX         
RT184X   DC    X'00',C'P4'                                                      
                                                                                
RT185    DC    AL2(RT185X-*+3),AL1(185)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'TEI-TEXACO ESTIMATE'                                        
         DC    AL1(VRQ02,XVRQ51)   SET U/L TO SJ                                
         DC    AL2(RT185X-*+1),X'00'                                            
         DC    AL1(CLI1Q,FVAL+FALL),X'8000',AL1(0),AL3(0)                       
         DC    AL1(PRD1Q,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(JOBQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                        
         DC    AL1(BGRPQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'D200',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'B601',AL1(0),AL3(0)                       
RT185X   DC    X'00',C'IE'                                                      
                                                                                
RT186    DC    AL2(RT186X-*+3),AL1(186)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'POT-USMC P.O. TAPE'                                         
         DC    AL1(VRQ18,00)                                                    
         DC    AL2(RT186X-*+1),X'00'                                            
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL),X'0000',AL1(0),AL3(0)                            
RT186X   DC    X'00',C'P9'                                                      
                                                                                
RT187    DC    AL2(RT187X-*+3),AL1(187)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'AIS-INTERREP INTERFACE'                                     
         DC    AL1(VRQ08,00)       SET U/L TO S                                 
         DC    AL2(RT187X-*+1),X'00'                                            
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ASTDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(AENDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'D200',AL1(0),AL3(0)                       
RT187X   DC    X'00',C'IS'                                                      
                                                                                
RT188    DC    AL2(RT188X-*+3),AL1(188) P8 - WORK-CODE/BILLING SUMMARY          
         DC    AL1(0,HREQR)                                                     
         DC    CL22'WBS-W-C/BILLING SUMM'                                       
         DC    AL1(VRQ18,00)                                                    
         DC    AL2(RT188X-*+1),AL1(HMENU)                                       
         DC    AL1(CLI1Q,FVAL+FALL),X'8000',AL1(0),AL3(0)                       
         DC    AL1(LISTQ,FALT),X'8000',AL1(0),AL3(0)                            
         DC    AL1(MGRPQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(MEF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(OPT2Q,FVAL+FOPT),X'0A00',AL1(0),AL3(0)                       
RT188X   DC    X'00',C'P8'                                                      
                                                                                
RT189    DC    AL2(RT189X-*+3),AL1(189)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'CLR-CLIENT REALIZATION'                                     
         DC    AL1(VRQ33,00)          SET U/L TO 1C                             
         DC    AL2(RT189X-*+1),AL1(HMENU)                                       
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(MOARQ,FVAL+FOPT),X'1D00',AL1(0),AL3(0)                       
         DC    AL1(BUDTQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'4D00',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'4E00',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'6B00',AL1(0),AL3(0)                       
RT189X   DC    X'00',C'R1'                                                      
                                                                                
RT190    DC    AL2(RT190X-*+3),AL1(190)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'TER-T+E/EXPENSE REPRTS'                                     
         DC    AL1(00,XVRQ21)                                                   
         DC    AL2(RT190X-*+1),X'00'                                            
         DC    AL1(RFMTQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(UNITQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(STRDQ,FALT+FVAL),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FALT+FVAL),X'0000',AL1(0),AL3(0)                       
         DC    AL1(SELTQ,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
RT190X   DC    X'00',C'TE'                                                      
                                                                                
RT191    DC    AL2(RT191X-*+3),AL1(191)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'EER-EMPLOYEE REALIZATN'                                     
         DC    AL1(VRQ16,XVRQ56)   SET U/L TO 1R                                
         DC    AL2(RT191X-*+1),AL1(HMENU)                                       
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(MOARQ,FVAL+FOPT),X'1D00',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'8200',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'5600',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'6D00',AL1(0),AL3(0)                       
         DC    AL1(SALRQ,FVAL+FOPT),X'020D',AL1(FNSEC),AL3(0)                   
RT191X   DC    X'00',C'R2'                                                      
                                                                                
RT192    DC    AL2(RT192X-*+3),AL1(192)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'CFA-CLIENT FEE ANALYS'                                      
         DC    AL1(VRQ16,00)       SET U/L TO 1R                                
         DC    AL2(RT192X-*+1),AL1(HMENU)                                       
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(CLI2Q,FVAL+FOPT+FALT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MOARQ,FALT),X'6700',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'6600',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'BF00',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'0104',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'0106',AL1(0),AL3(0)                       
         DC    AL1(SALRQ,FVAL+FOPT),X'020D',AL1(FNSEC),AL3(0)                   
RT192X   DC    X'00',C'R3'                                                      
                                                                                
RT193    DC    AL2(RT193X-*+3),AL1(193)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'WDS-WORK DISTRIB REPRT'                                     
         DC    AL1(VRQ02,XVRQ45)   SET U/L TO SJ                                
         DC    AL2(RT193X-*+1),AL1(HMENU)                                       
         DC    AL1(CLI1Q,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(PRD1Q,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(JOBQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                   
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MOARQ,FALT+FOPT),X'6700',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'7100',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'7101',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'7102',AL1(0),AL3(0)                       
         DC    AL1(SALRQ,FVAL+FOPT),X'020D',AL1(FNSEC),AL3(0)                   
RT193X   DC    X'00',C'R4'                                                      
                                                                                
RT194    DC    AL2(RT194X-*+3),AL1(194)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'WFR-WEEKLY FLASH REPRT'                                     
         DC    AL1(VRQ16,00)       SET U/L TO 1R                                
         DC    AL2(RT194X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(SHRSQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(SRATQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(TPCTQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'7300',AL1(0),AL3(0)                       
RT194X   DC    X'00',C'R5'                                                      
                                                                                
RT195    DC    AL2(RT195X-*+3),AL1(195)                                         
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'AGR-GST RULES REC RPT'                                      
         DC    AL1(0,0)            SET U/L TO SJ                                
         DC    AL2(RT195X-*+1),AL1(HMENU)                                       
         DC    AL1(RECDQ,15),X'0126',AL1(0),AL3(0)                              
         DC    AL1(OPT1Q,FVAL+FOPT),X'0127',AL1(0),AL3(0)                       
RT195X   DC    X'00',C'GR'                                                      
                                                                                
RT196    DC    AL2(RT196X-*+3),AL1(196)                                         
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'ZMB-ZENITH BILLING    '                                     
         DC    AL1(0,0)            SET U/L TO 1R                                
         DC    AL2(RT196X-*+1),X'00'                                            
         DC    AL1(RMNTQ,FVAL),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'1100',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FDDSO+FVAL+FOPT),X'0209',AL1(0),AL3(0)                 
         DC    AL1(OPT3Q,FDDSO+FVAL+FOPT),X'020A',AL1(0),AL3(0)                 
RT196X   DC    X'00',C'MB'                                                      
                                                                                
RT197    DC    AL2(RT197X-*+3),AL1(197)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'EUR-EMPLOYEE UTILIZATN'                                     
         DC    AL1(VRQ16,XVRQ49)   SET U/L TO 1R                                
         DC    AL2(RT197X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MOARQ,FVAL),X'1D00',AL1(0),AL3(0)                            
         DC    AL1(CALPQ,FALT+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'7C00',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'7D00',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'0100',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'0101',AL1(0),AL3(0)                       
         DC    AL1(OPT5Q,FVAL+FOPT),X'0102',AL1(0),AL3(0)                       
         DC    AL1(OPT6Q,FVAL+FOPT),X'FE01',AL1(0),AL3(0)                       
         DC    AL1(OPT7Q,FVAL+FOPT),X'C200',AL1(0),AL3(0)                       
         DC    AL1(OPT8Q,FVAL+FOPT),X'0205',AL1(0),AL3(0)                       
RT197X   DC    X'00',C'R6'                                                      
                                                                                
RT198    DC    AL2(RT198X-*+3),AL1(198)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'CHA-CHRGBLE TIME ANAL'                                      
         DC    AL1(VRQ16,00)       SET U/L TO 1R                                
         DC    AL2(RT198X-*+1),AL1(HMENU)                                       
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(CLI2Q,FVAL+FOPT+FALT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MOARQ,FALT),X'6700',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'0105',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'7100',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'7104',AL1(0),AL3(0)                       
         DC    AL1(SALRQ,FVAL+FOPT),X'020D',AL1(FNSEC),AL3(0)                   
RT198X   DC    X'00',C'R7'                                                      
                                                                                
RT199    DC    AL2(RT199X-*+3),AL1(199)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'AR8-INCOME ACCRUAL'                                         
         DC    AL1(VRQ02,00)       SET U/L TO SJ                                
         DC    AL2(RT199X-*+1),AL1(HMENU)                                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDMQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FDDSO+FVAL+FOPT),X'3000',AL1(0),AL3(0)                 
         DC    AL1(OPT2Q,FVAL+FOPT),X'0206',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'0207',AL1(0),AL3(0)                       
RT199X   DC    X'00',C'R8'                                                      
                                                                                
RT201    DC    AL2(RT201X-*+3),AL1(201) ** TALENT REPORT **                     
         DC    AL1(0,HREQR)                                                     
         DC    CL22'CDS-COMM DATA SHEET'                                        
         DC    AL1(VRQ19,XVRQ14)                                                
         DC    AL2(RT201X-*+1),X'00'                                            
         DC    AL1(CLI1Q,FVAL+FALL),X'8000',AL1(0),AL3(0)                       
         DC    AL1(PRD1Q,FVAL+FALL),X'8000',AL1(0),AL3(0)                       
         DC    AL1(COMMQ,FVAL+FALL),X'8000',AL1(0),AL3(0)                       
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'B800',AL1(0),AL3(0)                       
RT201X   DC    X'00',C'T1'                                                      
*&&DO                                                                           
RT203    DC    AL2(RT203X-*+3),AL1(203)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'FL1-P&&L'                                                   
         DC    AL1(00,00)                                                       
         DC    AL2(RT203X-*+1),X'00'                                            
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT203X   DC    X'00',C'FL'                                                      
*                                                                               
RT204    DC    AL2(RT204X-*+3),AL1(204)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'FP1-P&&L'                                                   
         DC    AL1(00,00)                                                       
         DC    AL2(RT204X-*+1),X'00'                                            
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT204X   DC    X'00',C'FP'                                                      
*                                                                               
RT205    DC    AL2(RT205X-*+3),AL1(205)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'BL1-CASH'                                                   
         DC    AL1(00,00)                                                       
         DC    AL2(RT205X-*+1),X'00'                                            
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT205X   DC    X'00',C'BL'                                                      
*                                                                               
RT206    DC    AL2(RT206X-*+3),AL1(206)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'BP1-CASH'                                                   
         DC    AL1(00,00)                                                       
         DC    AL2(RT206X-*+1),X'00'                                            
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT206X   DC    X'00',C'BP'                                                      
*                                                                               
RT207    DC    AL2(RT207X-*+3),AL1(207)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'GL1-GENERAL LEDGER'                                         
         DC    AL1(00,00)                                                       
         DC    AL2(RT207X-*+1),X'00'                                            
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT207X   DC    X'00',C'GL'                                                      
*                                                                               
RT208    DC    AL2(RT208X-*+3),AL1(208)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'GP1-GENERAL LEDGER'                                         
         DC    AL1(00,00)                                                       
         DC    AL2(RT208X-*+1),X'00'                                            
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT208X   DC    X'00',C'GP'                                                      
*&&                                                                             
RT209    DC    AL2(RT209X-*+3),AL1(209) WC - CLIENT GROUPING REPORT             
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'AWC-CLT GROUPING RPT'                                       
         DC    AL1(VRQ02,XVRQ55)                                                
         DC    AL2(RT210X-*+1),AL1(HMENU)                                       
         DC    AL1(GRPLQ,FOPT),X'0000',AL1(0),AL3(0)                            
         DC    AL1(GRPCQ,FOPT),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'0313',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'030D',AL1(0),AL3(0)                       
RT209X   DC    X'00',C'WC'                                                      
                                                                                
RT210    DC    AL2(RT210X-*+3),AL1(210) WI - WORKING INVESTMENT                 
         DC    AL1(0,HREQR)                                                     
         DC    CL22'AWI-WORKING INVESTMENT'                                     
         DC    AL1(VRQ02,XVRQ55)                                                
         DC    AL2(RT210X-*+1),AL1(HMENU)                                       
         DC    AL1(GRPLQ,FOPT),X'0000',AL1(0),AL3(0)                            
         DC    AL1(GRPCQ,FOPT),X'0000',AL1(0),AL3(0)                            
         DC    AL1(CLI1Q,FOPT+FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)            
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL+FOPT+FALT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(ENDDQ,FVAL+FOPT+FALT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(MOARQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(SRUNQ,FVAL+FOPT),X'0312',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL),X'030C',AL1(0),AL3(0)                            
         DC    AL1(OPT2Q,FVAL+FOPT),X'030D',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'030E',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'030F',AL1(0),AL3(0)                       
         DC    AL1(OPT5Q,FVAL+FOPT),X'0310',AL1(0),AL3(0)                       
         DC    AL1(OPT6Q,FVAL+FOPT),X'0311',AL1(0),AL3(0)                       
         DC    AL1(OPT7Q,FVAL+FOPT),X'0314',AL1(0),AL3(0)                       
RT210X   DC    X'00',C'WI'                                                      
                                                                                
*&&DO                                                                           
RT211    DC    AL2(RT211X-*+3),AL1(211)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'ZL1-MEDIA CONTROL'                                          
         DC    AL1(00,00)                                                       
         DC    AL2(RT211X-*+1),X'00'                                            
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT211X   DC    X'00',C'ZL'                                                      
*                                                                               
RT212    DC    AL2(RT212X-*+3),AL1(212)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'ZP1-MEDIA CONTROL'                                          
         DC    AL1(00,00)                                                       
         DC    AL2(RT212X-*+1),X'00'                                            
         DC    AL1(BLNKQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
RT212X   DC    X'00',C'ZP'                                                      
*&&                                                                             
RT220    DC    AL2(RT220X-*+3),AL1(220) ** TALENT REPORT **                     
         DC    AL1(0,HREQR)                                                     
         DC    CL22'PHE-P&&H EARNINGS'                                          
         DC    AL1(VRQ20,00)       SET U/L TO SN                                
         DC    AL2(RT220X-*+1),X'00'                                            
         DC    AL1(EMPRQ,FVAL+FALL),X'8000',AL1(0),AL3(0)                       
         DC    AL1(EMPEQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(UNN2Q,FVAL+FALL),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'1E00',AL1(0),AL3(0)                       
RT220X   DC    X'00',C'TK'                                                      
                                                                                
RT225    DC    AL2(RT225X-*+3),AL1(225) ** TALENT REPORT **                     
         DC    AL1(0,HREQR)                                                     
         DC    CL22'PWS-P+W SUMMARY'                                            
         DC    AL1(VRQ19,00)                                                    
         DC    AL2(RT225X-*+1),X'00'                                            
         DC    AL1(LEDGQ,FDPSL+FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)            
         DC    AL1(CLI1Q,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(PRD1Q,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(OFF1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(UNN2Q,FVAL+FALL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(OPT4Q,FVAL+FOPT),X'4300',AL1(0),AL3(0)                       
         DC    AL1(OPT5Q,FVAL+FOPT),X'5000',AL1(0),AL3(0)                       
RT225X   DC    X'00',C'TP'                                                      
                                                                                
RT229    DC    AL2(RT229X-*+3),AL1(229)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'FMS-1099-MISC FORMS'                                        
         DC    AL1(VRQ28,XVRQ15)                                                
         DC    AL2(RT229X-*+1),AL1(HMENU)                                       
         DC    AL1(UNITQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(LEDGQ,FVAL),X'8000',AL1(0),AL3(0)                            
         DC    AL1(ACCTQ,FOPT+FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)            
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(LIVFQ,FVAL+FOPT),X'2C00',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'6800',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'6801',AL1(0),AL3(0)                       
         DC    AL1(FRM99Q,FVAL),X'0325',AL1(0),AL3(0)                           
RT229X   DC    X'00',C'TT'                                                      
                                                                                
RT229A   DC    AL2(RT229AX-*+3),AL1(229)                                        
         DC    AL1(0,HREQR)                                                     
         DC    CL22'FMS-1099-NEC FORMS'                                         
         DC    AL1(0,0)                                                         
         DC    AL2(RT229AX-*+1),X'00'                                           
         DC    AL1(NALQ,FVAL),X'0322',AL1(0),AL3(0)                             
RT229AX  DC    X'00',C'T7'                                                      
                                                                                
RT236    DC    AL2(RT236X-*+3),AL1(236)                                         
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'COF-COST OF FINANCE'                                        
         DC    AL1(VRQ24,XVRQ43)   SET U/L TO SR                                
         DC    AL2(RT236X-*+1),AL1(HMENU)                                       
         DC    AL1(ACCTQ,FOPT+FSOON+FVAL+FALL),X'8000',AL1(0),AL3(0)            
         DC    AL1(LISTQ,FVAL+FOPT+FALT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ASTDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(AENDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MOARQ,FVAL+FOPT),X'1D00',AL1(0),AL3(0)                       
         DC    AL1(RVSLQ,FVAL+FOPT),X'0201',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'7400',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'7600',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'7500',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'7700',AL1(0),AL3(0)                       
RT236X   DC    X'00',C'CF'                                                      
                                                                                
RT237    DC    AL2(RT237X-*+3),AL1(237)                                         
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'ADA-AGED DEBTORS   '                                        
         DC    AL1(VRQ24,00)       SET U/L TO SR                                
         DC    AL2(RT237X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL),X'0000',AL1(0),AL3(0)                            
         DC    AL1(ASTDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(AENDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(MOARQ,FVAL+FOPT),X'1D00',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'7B00',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'0107',AL1(0),AL3(0)                       
RT237X   DC    X'00',C'WP'                                                      
                                                                                
RT238    DC    AL2(RT238X-*+3),AL1(238)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'WPR-MED/WRKCD PURGE'                                        
         DC    AL1(VRQ02,00)       SET U/L TO SJ                                
         DC    AL2(RT238X-*+1),X'00'                                            
         DC    AL1(OPT1Q,FDDSO+FVAL+FOPT),X'7900',AL1(0),AL3(0)                 
         DC    AL1(OPT2Q,FVAL+FOPT),X'7A00',AL1(0),AL3(0)                       
RT238X   DC    X'00',C'XW'                                                      
                                                                                
RT239    DC    AL2(RT239X-*+3),AL1(239)                                         
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'MTS-MISSING TIMESHEET'                                      
         DC    AL1(VRQ16,XVRQ48)                                                
         DC    AL2(RT239X-*+1),X'00'                                            
         DC    AL1(ACCTQ,23),X'8000',AL1(FNALL),AL3(0)                          
         DC    AL1(PRNCQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(TS#RQ,FVAL+FOPT),X'0308',AL1(0),AL3(0)                       
         DC    AL1(INCFQ,FVAL+FOPT),X'0115',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'030B',AL1(0),AL3(0)                       
RT239X   DC    X'00',C'CM'                                                      
                                                                                
RT240    DC    AL2(RT240X-*+3),AL1(240)                                         
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'SHT-SALARY HISTORY'                                         
         DC    AL1(VRQ16,XVRQ01)                                                
         DC    AL2(RT240X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT+FVAL),X'0000',AL1(0),AL3(0)                       
         DC    AL1(ENDDQ,FALT+FVAL),X'0000',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'D602',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'D603',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'D604',AL1(0),AL3(0)                       
         DC    AL1(MTHDQ,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(PYRLQ,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
RT240X   DC    X'00',C'CU'                                                      
                                                                                
RT241    DC    AL2(RT241X-*+3),AL1(241)                                         
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'ACN-PERSON REPORT'                                          
         DC    AL1(VRQ16,00)                                                    
         DC    AL2(RT241X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(OPT1Q,FVAL+FOPT),X'0118',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'0119',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'011A',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'011B',AL1(0),AL3(0)                       
         DC    AL1(OPT5Q,FVAL+FOPT),X'011C',AL1(0),AL3(0)                       
         DC    AL1(OPT6Q,FVAL+FOPT),X'011D',AL1(0),AL3(0)                       
RT241X   DC    X'00',C'CN'                                                      
                                                                                
*&&DO                                                                           
RT242    DC    AL2(RT242X-*+3),AL1(242)                                         
         DC    AL1(0,HSOON+HREQR)                                               
         DC    CL22'TIJ-TIME JOURNAL      '                                     
         DC    AL1(VRQ16,XVRQ52)   SET U/L TO 1R                                
         DC    AL2(RT242X-*+1),AL1(HMENU)                                       
         DC    AL1(ACCTQ,FSOON+FVAL),X'8000',AL1(0),AL3(0)                      
RT242X   DC    X'00',C'TJ'                                                      
*&&                                                                             
                                                                                
RT243    DC    AL2(RT243X-*+3),AL1(243)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'FEE-FEE REPORT'                                             
         DC    AL1(VRQ16,XVRQ54)   SET U/L TO 1R                                
         DC    AL2(RT243X-*+1),X'00'                                            
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(CAF1Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(CAF2Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(CAF3Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(CAF4Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(CAF5Q,FRHS+FVAL+FOPT),X'0000',AL1(0),AL3(0)                  
         DC    AL1(CLI2Q,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(PRD2Q,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(DPT2Q,FVAL+FOPT),X'8000',AL1(0),AL3(0)                       
         DC    AL1(GRPCQ,FOPT),X'0000',AL1(0),AL3(0)                            
         DC    AL1(OPT1Q,FVAL+FOPT),X'0120',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'0125',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'012A',AL1(0),AL3(0)                       
         DC    AL1(OPT4Q,FVAL+FOPT),X'012E',AL1(0),AL3(0)                       
RT243X   DC    X'00',C'FR'                                                      
                                                                                
RT244    DC    AL2(RT244X-*+3),AL1(244)                                         
         DC    AL1(0,HREQR)                                                     
         DC    CL22'EER-EMPLOYEE REALIZATN'                                     
         DC    AL1(VRQ16,00)       SET U/L TO 1R                                
         DC    AL2(RT244X-*+1),AL1(HMENU)                                       
         DC    AL1(ACCTQ,FVAL+FALL+FOPT),X'8000',AL1(0),AL3(0)                  
         DC    AL1(FLT1Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT2Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT3Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT4Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(FLT5Q,FVAL+FOPT),X'0000',AL1(0),AL3(0)                       
         DC    AL1(STRDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(ENDDQ,FALT),X'8C00',AL1(0),AL3(0)                            
         DC    AL1(MOARQ,FVAL+FOPT),X'1D00',AL1(0),AL3(0)                       
         DC    AL1(OPT1Q,FVAL+FOPT),X'8200',AL1(0),AL3(0)                       
         DC    AL1(OPT2Q,FVAL+FOPT),X'5600',AL1(0),AL3(0)                       
         DC    AL1(OPT3Q,FVAL+FOPT),X'6D00',AL1(0),AL3(0)                       
RT244X   DC    X'00',C'R9'                                                      
                                                                                
REQTBLX  DC    AL2(0)                                                           
         EJECT                                                                  
***********************************************************************         
* FIELD TABLE                                                         *         
***********************************************************************         
FLDNUMT  DS    0X                                                               
                                                                                
* UNIT                                                                          
         DC    AL1(UNITQ,OVR3,FVUNIT,3,0)                                       
         DC    AL1(L'DC@UNIT),AL2(DC@UNIT-T60400)                               
* LEDGER                                                                        
         DC    AL1(LEDGQ,OVR3,FVLEDG,3,0)                                       
         DC    AL1(L'DC@LEDG),AL2(DC@LEDG-T60400)                               
* ACCOUNT                                                                       
         DC    AL1(ACCTQ,OVR4,FVACCT,15,0)                                      
         DC    AL1(L'DC@ACCT),AL2(DC@ACCT-T60400)                               
* BILLING GRP                                                                   
         DC    AL1(BGRPQ,OVR3,FVBGRP,4,0)                                       
         DC    AL1(L'DC@BGRP),AL2(DC@BGRP-T60400)                               
* FILTER#1                                                                      
         DC    AL1(FLT1Q,OVR3,FVFLT1,2,0)                                       
         DC    AL1(L'DC@FLT1),AL2(DC@FLT1-T60400)                               
* FILTER#2                                                                      
         DC    AL1(FLT2Q,OVR3,FVFLT2,2,0)                                       
         DC    AL1(L'DC@FLT2),AL2(DC@FLT2-T60400)                               
* START DATE                                                                    
         DC    AL1(STRDQ,OVR3,FVSTRD,9,0)                                       
         DC    AL1(L'DC@STRD),AL2(DC@STRD-T60400)                               
* END DATE                                                                      
         DC    AL1(ENDDQ,OVR3,FVENDD,9,0)                                       
         DC    AL1(L'DC@ENDD),AL2(DC@ENDD-T60400)                               
* SORT ORDER                                                                    
         DC    AL1(SRTOQ,OVR3,FVSRTO,2,0)                                       
         DC    AL1(L'DC@SORT),AL2(DC@SORT-T60400)                               
* MEDIA GROUP                                                                   
         DC    AL1(MGRPQ,OVR4,FVMGRP,2,0)                                       
         DC    AL1(L'DC@MGRP),AL2(DC@MGRP-T60400)                               
* OPTION #1                                                                     
         DC    AL1(OPT1Q,OVR3,FVOPT1,1,0)                                       
         DC    AL1(L'DC@OPT1),AL2(DC@OPT1-T60400)                               
* RECORD DATE (EFFECTIVE DATE)                                                  
         DC    AL1(RECDQ,OVR3,FVRECD,9,0)                                       
         DC    AL1(L'DC@RECD),AL2(DC@RECD-T60400)                               
* CLIENT                                                                        
         DC    AL1(CLI1Q,OVR3,FVCLI1,6,0)                                       
         DC    AL1(L'DC@CLI1),AL2(DC@CLI1-T60400)                               
* PRODUCT                                                                       
         DC    AL1(PRD1Q,OVR3,FVPRD1,6,0)                                       
         DC    AL1(L'DC@PRD1),AL2(DC@PRD1-T60400)                               
* JOB                                                                           
         DC    AL1(JOBQ,OVR3,FVJOB,6,0)                                         
         DC    AL1(L'DC@JOB),AL2(DC@JOB-T60400)                                 
* TIMESHEET # RANGE                                                             
         DC    AL1(TS#RQ,OVR3,FVTS#R,5,0)                                       
         DC    AL1(L'DC@TS#R),AL2(DC@TS#R-T60400)                               
* FRENCH                                                                        
         DC    AL1(FRENQ,OVR3,FVFREN,1,0)                                       
         DC    AL1(L'DC@FREN),AL2(DC@FREN-T60400)                               
* OPTION#2                                                                      
         DC    AL1(OPT2Q,OVR3,FVOPT2,1,0)                                       
         DC    AL1(L'DC@OPT2),AL2(DC@OPT2-T60400)                               
* OPTION#3                                                                      
         DC    AL1(OPT3Q,OVR3,FVOPT3,1,0)                                       
         DC    AL1(L'DC@OPT3),AL2(DC@OPT3-T60400)                               
* FILTER#3                                                                      
         DC    AL1(FLT3Q,OVR3,FVFLT3,2,0)                                       
         DC    AL1(L'DC@FLT3),AL2(DC@FLT3-T60400)                               
* MEDIA FILTER                                                                  
         DC    AL1(MEF1Q,OVR3,FVMEF1,2,0)                                       
         DC    AL1(L'DC@MEF1),AL2(DC@MEF1-T60400)                               
* BILLING FILTER                                                                
         DC    AL1(BFLTQ,OVR3,FVBFLT,2,0)                                       
         DC    AL1(L'DC@BFLT),AL2(DC@BFLT-T60400)                               
* FILTER #5                                                                     
         DC    AL1(FLT5Q,OVR3,FVFLT5,2,0)                                       
         DC    AL1(L'DC@FLT5),AL2(DC@FLT5-T60400)                               
* BATCH TYPE                                                                    
         DC    AL1(BATTQ,OVR3,FVBATT,2,0)                                       
         DC    AL1(L'DC@BATT),AL2(DC@BATT-T60400)                               
* PERSON                                                                        
         DC    AL1(PERSQ,OVR3,FVPERS,8,0)                                       
         DC    AL1(L'DC@PERS),AL2(DC@PERS-T60400)                               
* CLIENT(BANK)                                                                  
         DC    AL1(CLI2Q,OVR3,FVCLI2,6,0)                                       
         DC    AL1(L'DC@CLI2),AL2(DC@CLI2-T60400)                               
* CHECK DATE                                                                    
         DC    AL1(CHKDQ,OVR3,FVCHKD,10,0)                                      
         DC    AL1(L'DC@CHKD),AL2(DC@CHKD-T60400)                               
* TITLE END IN RAPPL+8(2)                                                       
         DC    AL1(TIEDQ,OVR4,FVTIED,9,0)                                       
         DC    AL1(L'DC@TIED),AL2(DC@TIED-T60400)                               
* FILTER #4                                                                     
         DC    AL1(FLT4Q,OVR3,FVFLT4,2,0)                                       
         DC    AL1(L'DC@FLT4),AL2(DC@FLT4-T60400)                               
* BILL DATE                                                                     
         DC    AL1(BDATQ,OVR3,FVBDAT,9,0)                                       
         DC    AL1(L'DC@BDAT),AL2(DC@BDAT-T60400)                               
* OPTION#4                                                                      
         DC    AL1(OPT4Q,OVR3,FVOPT4,1,0)                                       
         DC    AL1(L'DC@OPT4),AL2(DC@OPT4-T60400)                               
* OPTION#5                                                                      
         DC    AL1(OPT5Q,OVR3,FVOPT5,1,0)                                       
         DC    AL1(L'DC@OPT5),AL2(DC@OPT5-T60400)                               
* ADVERTISER (SAME AS LEDGER)                                                   
         DC    AL1(ADVRQ,OVR3,FVADVR,1,0)                                       
         DC    AL1(L'DC@ADVR),AL2(DC@ADVR-T60400)                               
* SCHEME                                                                        
         DC    AL1(SCHMQ,OVR3,FVSCHM,2,0)                                       
         DC    AL1(L'DC@SCHM),AL2(DC@SCHM-T60400)                               
* INCREMENT                                                                     
         DC    AL1(INC1Q,OVR3,FVINC1,2,0)                                       
         DC    AL1(L'DC@INC1),AL2(DC@INC1-T60400)                               
* START MONTH                                                                   
         DC    AL1(STRMQ,OVR4,FVSTRM,9,0)                                       
         DC    AL1(L'DC@STRM),AL2(DC@STRM-T60400)                               
* END MONTH                                                                     
         DC    AL1(ENDMQ,OVR4,FVENDM,9,0)                                       
         DC    AL1(L'DC@ENDM),AL2(DC@ENDM-T60400)                               
* OPTION#6                                                                      
         DC    AL1(OPT6Q,OVR3,FVOPT6,1,0)                                       
         DC    AL1(L'DC@OPT6),AL2(DC@OPT6-T60400)                               
* BUCKET TYPE IN OPT 3                                                          
         DC    AL1(BKT1Q,OVR3,FVBKT1,1,0)                                       
         DC    AL1(L'DC@BKT1),AL2(DC@BKT1-T60400)                               
* CONTRA FILTER 1                                                               
         DC    AL1(CAF1Q,OVR4,FVCAF1,2,0)                                       
         DC    AL1(L'DC@CAF1),AL2(DC@CAF1-T60400)                               
* RPT MTH                                                                       
         DC    AL1(RMONQ,OVR3,FVRMON,9,0)                                       
         DC    AL1(L'DC@RMON),AL2(DC@RMON-T60400)                               
* BILL CHAR (OPT2)                                                              
         DC    AL1(BCHRQ,OVR3,FVBCHR,1,0)                                       
         DC    AL1(L'DC@BCHR),AL2(DC@BCHR-T60400)                               
* TRANSACTION TYPE FILTER                                                       
         DC    AL1(TRNTQ,OVR3,FVTRNT,3,0)                                       
         DC    AL1(L'DC@TRNT),AL2(DC@TRNT-T60400)                               
* COMMERCIAL - LIKE JOB                                                         
         DC    AL1(COMMQ,OVR3,FVCOMM,6,0)                                       
         DC    AL1(L'DC@COMM),AL2(DC@COMM-T60400)                               
* EMPLOYER - LIKE ACCOUNT                                                       
         DC    AL1(EMPRQ,OVR4,FVEMPR,3,0)                                       
         DC    AL1(L'DC@EMPR),AL2(DC@EMPR-T60400)                               
* EMPLOYEE                                                                      
         DC    AL1(EMPEQ,OVR3,FVEMPE,9,0)                                       
         DC    AL1(L'DC@EMPE),AL2(DC@EMPE-T60400)                               
* BATCH REF                                                                     
         DC    AL1(BATRQ,OVR3,FVBATR,4,0)                                       
         DC    AL1(L'DC@BATR),AL2(DC@BATR-T60400)                               
* BILL NUMBER                                                                   
         DC    AL1(BNUMQ,OVR4,FVBNUM,6,0)                                       
         DC    AL1(L'DC@BNUM),AL2(DC@BNUM-T60400)                               
* SEQUENCE                                                                      
         DC    AL1(SEQNQ,OVR4,FVSEQN,1,0)                                       
         DC    AL1(L'DC@SEQN),AL2(DC@SEQN-T60400)                               
* DAY RANGE - LIKE INCREMENT                                                    
         DC    AL1(DRNGQ,OVR3,FVDRNG,2,0)                                       
         DC    AL1(L'DC@DRNG),AL2(DC@DRNG-T60400)                               
* GROUP RECORD LEDGER (WI/WC)                                                   
         DC    AL1(GRPLQ,OVR3,FVGRPL,1,0)                                       
         DC    AL1(L'DC@GRPL),AL2(DC@GRPL-T60400)                               
* GROUP CODE                                                                    
         DC    AL1(GRPCQ,OVR4,FVGRPC,7,0)                                       
         DC    AL1(L'DC@GRPC),AL2(DC@GRPC-T60400)                               
* REPORT                                                                        
         DC    AL1(RPTRQ,OVR4,FVRPTR,3,0)                                       
         DC    AL1(L'DC@RPTR),AL2(DC@RPTR-T60400)                               
* EXPENSE TYPE - (LIKE CLIENT)                                                  
         DC    AL1(EXPTQ,OVR3,FVEXPT,6,0)                                       
         DC    AL1(L'DC@EXPT),AL2(DC@EXPT-T60400)                               
* ORIGINAL BILL NUMBER                                                          
         DC    AL1(OBNOQ,OVR4,FVOBNO,6,0)                                       
         DC    AL1(L'DC@OBNO),AL2(DC@OBNO-T60400)                               
* ORI BILL DATE                                                                 
         DC    AL1(OBDAQ,OVR3,FVOBDA,9,0)                                       
         DC    AL1(L'DC@OBDA),AL2(DC@OBDA-T60400)                               
* REV BILL DATE-IN RSTRD                                                        
         DC    AL1(RBDAQ,OVR3,FVRBDA,9,0)                                       
         DC    AL1(L'DC@RBDA),AL2(DC@RBDA-T60400)                               
* UNIT/LIST                                                                     
         DC    AL1(UNTLQ,OVR3,FVUNTL,6,0)                                       
         DC    AL1(L'DC@UNTL),AL2(DC@UNTL-T60400)                               
* LIST                                                                          
         DC    AL1(LISTQ,OVR3,FVLIST,6,0)                                       
         DC    AL1(L'DC@LIST),AL2(DC@LIST-T60400)                               
* LEVEL IN OPTION 1                                                             
         DC    AL1(LVL1Q,OVR3,FVLVL1,1,0)                                       
         DC    AL1(L'DC@LVL1),AL2(DC@LVL1-T60400)                               
* CURRENT IN OPTION 2                                                           
         DC    AL1(CURRQ,OVR3,FVCURR,1,0)                                       
         DC    AL1(L'DC@CURR),AL2(DC@CURR-T60400)                               
* YTD IN OPTION 3                                                               
         DC    AL1(YTDQ,03,FVYTD,1,0)                                           
         DC    AL1(L'DC@YTD),AL2(DC@YTD-T60400)                                 
* YEARLY IN OPTION 4+5                                                          
         DC    AL1(YRLYQ,OVR4,FVYRLY,2,0)                                       
         DC    AL1(L'DC@YRLY),AL2(DC@YRLY-T60400)                               
* DATA TYPES IN QSRTAREA                                                        
         DC    AL1(DATAQ,OVR4,FVDATA,63,0)                                      
         DC    AL1(L'DC@DATA),AL2(DC@DATA-T60400)                               
* BUDGET TYPE IN QSRTAREA                                                       
         DC    AL1(BUDTQ,OVR4,FVBUDT,10,0)                                      
         DC    AL1(L'DC@BUDT),AL2(DC@BUDT-T60400)                               
* COMPARSION IN OPTION 2                                                        
         DC    AL1(COMPQ,OVR3,FVCOMP,1,0)                                       
         DC    AL1(L'DC@COMP),AL2(DC@COMP-T60400)                               
* EXCEPTION LIST IN QSELECT                                                     
         DC    AL1(EXCPQ,OVR4,FVEXCP,6,0)                                       
         DC    AL1(L'DC@EXCP),AL2(DC@EXCP-T60400)                               
* CLIENT/DIV - LIKE CLIENT                                                      
         DC    AL1(CLDVQ,OVR3,FVCLDV,16,0)                                      
         DC    AL1(L'DC@CLDV),AL2(DC@CLDV-T60400)                               
* OFF/DEPT IN QSELECT                                                           
         DC    AL1(OFDPQ,OVR4,FVOFDP,3,0)                                       
         DC    AL1(L'DC@OFDP),AL2(DC@OFDP-T60400)                               
* PROJECT IN OPT 1                                                              
         DC    AL1(APROQ,OVR3,FVAPRO,1,0)                                       
         DC    AL1(L'DC@APRO),AL2(DC@APRO-T60400)                               
* DIVISION IN OPT 2                                                             
         DC    AL1(BDIVQ,OVR3,FVBDIV,1,0)                                       
         DC    AL1(L'DC@BDIV),AL2(DC@BDIV-T60400)                               
* DEPARTMENT IN OPT 3                                                           
         DC    AL1(CDPTQ,OVR3,FVCDPT,1,0)                                       
         DC    AL1(L'DC@CDPT),AL2(DC@CDPT-T60400)                               
* OPTION 7                                                                      
         DC    AL1(OPT7Q,OVR3,FVOPT7,1,0)                                       
         DC    AL1(L'DC@OPT7),AL2(DC@OPT7-T60400)                               
* SUB DPT/PRJ IN OPT 4                                                          
         DC    AL1(DSDPQ,OVR3,FVDSDP,1,0)                                       
         DC    AL1(L'DC@DSDP),AL2(DC@DSDP-T60400)                               
* TASK IN OPT 5                                                                 
         DC    AL1(ETSKQ,OVR3,FVETSK,1,0)                                       
         DC    AL1(L'DC@ETSK),AL2(DC@ETSK-T60400)                               
* CONTRA UNIT/LEDGER IN RCON                                                    
         DC    AL1(CAULQ,OVR3,FVCAUL,2,0)                                       
         DC    AL1(L'DC@CAUL),AL2(DC@CAUL-T60400)                               
* XJOB IN RXJOB(1)                                                              
         DC    AL1(XJOBQ,OVR4,FVXJOB,1,0)                                       
         DC    AL1(L'DC@XJOB),AL2(DC@XJOB-T60400)                               
* SELECT FIELD FOR APG REPORT                                                   
         DC    AL1(SELTQ,OVR4,FVSELT,6,0)                                       
         DC    AL1(L'DC@SELT),AL2(DC@SELT-T60400)                               
* CONTRA SELECT                                                                 
         DC    AL1(CNTRQ,OVR4,FVCNTR,7,0)                                       
         DC    AL1(L'DC@CNTR),AL2(DC@CNTR-T60400)                               
* REPORT FORMAT IN OPT 2                                                        
         DC    AL1(RFMTQ,OVR3,FVRFMT,1,0)                                       
         DC    AL1(L'DC@RFMT),AL2(DC@RFMT-T60400)                               
* NARRATIVE IN QSELECT                                                          
         DC    AL1(NAR1Q,OVR4,FVNAR1,2,0)                                       
         DC    AL1(L'DC@NAR1),AL2(DC@NAR1-T60400)                               
* DAILY UNITS (SAME AS NARRA                                                    
         DC    AL1(DLYUQ,OVR4,FVDLYU,2,0)                                       
         DC    AL1(L'DC@DLYU),AL2(DC@DLYU-T60400)                               
* BATCH GROUP                                                                   
         DC    AL1(BATGQ,OVR3,FVBATG,1,0)                                       
         DC    AL1(L'DC@BATG),AL2(DC@BATG-T60400)                               
* SPECIFIC U/L TO RUN ACWI                                                      
         DC    AL1(SRUNQ,OVR3,FVSRUN,2,0)                                       
         DC    AL1(L'DC@SRUN),AL2(DC@SRUN-T60400)                               
* CONTRA FILTER 2                                                               
         DC    AL1(CAF2Q,OVR4,FVCAF2,2,0)                                       
         DC    AL1(L'DC@CAF2),AL2(DC@CAF2-T60400)                               
* MOS IN QMOS(4) 9101                                                           
         DC    AL1(RMNTQ,OVR4,FVRMNT,6,0)                                       
         DC    AL1(L'DC@RMNT),AL2(DC@RMNT-T60400)                               
* START/END MOS                                                                 
         DC    AL1(MOARQ,OVR4,FVMOAR,13,0)                                      
         DC    AL1(L'DC@MOAR),AL2(DC@MOAR-T60400)                               
* NUMBER IN QSELECT                                                             
         DC    AL1(NUMBQ,OVR4,FVNUMB,5,0)                                       
         DC    AL1(L'DC@NUMB),AL2(DC@NUMB-T60400)                               
* OPTION 9 IN ACQOPT9                                                           
         DC    AL1(OPT9Q,OVR3,FVOPT9,1,0)                                       
         DC    AL1(L'DC@OPT9),AL2(DC@OPT9-T60400)                               
* OPTION 10 IN ACQOPT10                                                         
         DC    AL1(OPTAQ,OVR3,FVOPTA,1,0)                                       
         DC    AL1(L'DC@OPTA),AL2(DC@OPTA-T60400)                               
* MINIMUM AMOUNT IN RSORTA (C)                                                  
         DC    AL1(MINAQ,OVR4,FVMINA,3,0)                                       
         DC    AL1(L'DC@MINA),AL2(DC@MINA-T60400)                               
* TRANSMISSION MONTH IN QSEL                                                    
         DC    AL1(TMONQ,OVR3,FVTMON,2,0)                                       
         DC    AL1(L'DC@TMON),AL2(DC@TMON-T60400)                               
* TRANSMISSION YEAR IN QSELECT                                                  
         DC    AL1(TYRQ,03,FVTYR,2,0)                                           
         DC    AL1(L'DC@TYR),AL2(DC@TYR-T60400)                                 
* CURRENCY CONVERSION IN BILUP                                                  
         DC    AL1(CCNVQ,OVR4,FVCCNV,4,0)                                       
         DC    AL1(L'DC@CCNV),AL2(DC@CCNV-T60400)                               
* CLIENT(BANK) FOR AC55 ONLY                                                    
         DC    AL1(CLI3Q,OVR3,FVCLI3,6,0)                                       
         DC    AL1(L'DC@CLI3),AL2(DC@CLI3-T60400)                               
* AGENCY FILTER IN QSRTAREA                                                     
         DC    AL1(AGYFQ,OVR4,FVAGYF,3,0)                                       
         DC    AL1(L'DC@AGYF),AL2(DC@AGYF-T60400)                               
* MEDIA FILTER IN QSRTAREA+3                                                    
         DC    AL1(MEF2Q,OVR4,FVMEF2,2,0)                                       
         DC    AL1(L'DC@MEF2),AL2(DC@MEF2-T60400)                               
* BUDGET FILTER IN QSRTAREA+                                                    
         DC    AL1(BUF1Q,OVR4,FVBUF1,2,0)                                       
         DC    AL1(L'DC@BUF1),AL2(DC@BUF1-T60400)                               
* LIVE FORMS IN OPT 1                                                           
         DC    AL1(LIVFQ,OVR3,FVLIVF,1,0)                                       
         DC    AL1(L'DC@LIVF),AL2(DC@LIVF-T60400)                               
* PRODUCT IN QSELECT+3                                                          
         DC    AL1(PRD2Q,OVR3,FVPRD2,6,0)                                       
         DC    AL1(L'DC@PRD2),AL2(DC@PRD2-T60400)                               
* FROM BUDGET TYPE IN QSRTAR                                                    
         DC    AL1(FBUDQ,OVR4,FVFBUD,10,0)                                      
         DC    AL1(L'DC@FBUD),AL2(DC@FBUD-T60400)                               
* TO BUDGET TYPE IN QSRTAREA                                                    
         DC    AL1(TBUDQ,OVR4,FVTBUD,10,0)                                      
         DC    AL1(L'DC@TBUD),AL2(DC@TBUD-T60400)                               
* WORKCODE FILTER IN QTRNSFL                                                    
         DC    AL1(WCFTQ,OVR4,FVWCFT,3,0)                                       
         DC    AL1(L'DC@WCFT),AL2(DC@WCFT-T60400)                               
* UNION EQUATE IN QSELECT+5                                                     
         DC    AL1(UNN1Q,OVR4,FVUNN1,3,0)                                       
         DC    AL1(L'DC@UNN1),AL2(DC@UNN1-T60400)                               
* FROM CONTRA UNIT/LEDGER INTRA                                                 
         DC    AL1(FCAUQ,OVR3,FVFCAU,2,0)                                       
         DC    AL1(L'DC@FCAU),AL2(DC@FCAU-T60400)                               
* UNION CODE IN QSELECT(3)                                                      
         DC    AL1(UNN2Q,OVR4,FVUNN2,3,0)                                       
         DC    AL1(L'DC@UNN2),AL2(DC@UNN2-T60400)                               
* CONTRA FILTER 3                                                               
         DC    AL1(CAF3Q,OVR4,FVCAFT,2,0)                                       
         DC    AL1(L'DC@CAF3),AL2(DC@CAF3-T60400)                               
* TO CONTRA UNIT/LEDGER IN QT+2                                                 
         DC    AL1(TCAUQ,OVR3,FVTCAU,2,0)                                       
         DC    AL1(L'DC@TCAU),AL2(DC@TCAU-T60400)                               
* DEPARTMENT IN QSELECT(6)                                                      
         DC    AL1(DPT1Q,OVR4,FVDPT1,3,0)                                       
         DC    AL1(L'DC@DPT1),AL2(DC@DPT1-T60400)                               
* TASK IN QAPPL+2(2)                                                            
         DC    AL1(TASKQ,OVR4,FVTASK,2,0)                                       
         DC    AL1(L'DC@TASK),AL2(DC@TASK-T60400)                               
* *** SPARE ***                                                                 
         DC    AL1(RPCDQ,0,FVRPCD,2,0)                                          
         DC    AL1(L'DC@RPCD),AL2(DC@RPCD-T60400)                               
* ATTRIBUTE IN OPT 4                                                            
         DC    AL1(ATTRQ,OVR4,FVATTR,3,0)                                       
         DC    AL1(L'DC@ATTR),AL2(DC@ATTR-T60400)                               
* CONTRA FILTER 4                                                               
         DC    AL1(CAF4Q,OVR4,FVCAF4,2,0)                                       
         DC    AL1(L'DC@CAF4),AL2(DC@CAF4-T60400)                               
* CONTRA FILTER 5                                                               
         DC    AL1(CAF5Q,OVR4,FVCAF5,2,0)                                       
         DC    AL1(L'DC@CAF5),AL2(DC@CAF5-T60400)                               
* DATE TYPE (OPTION 5)                                                          
         DC    AL1(DATEQ,OVR3,FVDATE,1,0)                                       
         DC    AL1(L'DC@DATE),AL2(DC@DATE-T60400)                               
* START AGING IN RSTRD                                                          
         DC    AL1(STAGQ,OVR3,FVSTAG,9,0)                                       
         DC    AL1(L'DC@STAG),AL2(DC@STAG-T60400)                               
* ESTIMATE NUMBER IN RBGRP                                                      
         DC    AL1(ESTNQ,OVR4,FVESTN,4,0)                                       
         DC    AL1(L'DC@ESTN),AL2(DC@ESTN-T60400)                               
* *** SPARE ***                                                                 
         DC    AL1(Q117,00,FVQ117,0,0)                                          
         DC    AL1(0),AL2(0)                                                    
* END AGING IN RENDD                                                            
         DC    AL1(ENAGQ,OVR3,FVENAG,9,0)                                       
         DC    AL1(L'DC@ENAG),AL2(DC@ENAG-T60400)                               
* DATA TYPES IN RAPPL                                                           
         DC    AL1(BUF2Q,OVR4,FVBUF2,63,0)                                      
         DC    AL1(L'DC@BUF2),AL2(DC@BUF2-T60400)                               
* USER FIELD IN RAPPL(2)                                                        
         DC    AL1(USRFQ,OVR4,FVUSRF,2,0)                                       
         DC    AL1(L'DC@USRF),AL2(DC@USRF-T60400)                               
* MEDIA MOS IN ACQDTSTR/END                                                     
         DC    AL1(MMOSQ,OVR4,FVMMOS,13,0)                                      
         DC    AL1(L'DC@MMOS),AL2(DC@MMOS-T60400)                               
* OFFICE IN ROFFICE/RNUM+24                                                     
         DC    AL1(OFF1Q,OVR3,FVOFF1,3,0)                                       
         DC    AL1(L'DC@OFF1),AL2(DC@OFF1-T60400)                               
* OFFICE IN ROFFICE/RNUM+24                                                     
         DC    AL1(OFF2Q,OVR3,FVOFF2,3,0)                                       
         DC    AL1(L'DC@OFF2),AL2(DC@OFF2-T60400)                               
* JOB GROUP IN RAPPL(3)                                                         
         DC    AL1(JOBGQ,OVR4,FVJOBG,4,0)                                       
         DC    AL1(L'DC@JOBG),AL2(DC@JOBG-T60400)                               
* WORKCODE TYPE IN RAPPL+3(1                                                    
         DC    AL1(WCTYQ,OVR4,FVWCTY,1,0)                                       
         DC    AL1(L'DC@WCTY),AL2(DC@WCTY-T60400)                               
* *** SPARE ***                                                                 
         DC    AL1(Q126,00,0,0,0)                                               
         DC    AL1(0),AL2(0)                                                    
* BLANK FIELD  *                                                                
         DC    AL1(BLNKQ,00,0,1,0)                                              
         DC    AL1(L'DCBLANK),AL2(DCBLANK-T60400)                               
* FORMAT IN RAPPL+4(1)                                                          
         DC    AL1(FRMTQ,OVR4,FVFRMT,1,0)                                       
         DC    AL1(L'DC@FRMT),AL2(DC@FRMT-T60400)                               
* TITLE START IN RAPPL+6(2)                                                     
         DC    AL1(TISDQ,OVR4,FVTISD,9,0)                                       
         DC    AL1(L'DC@TISD),AL2(DC@TISD-T60400)                               
* STD HRS BUD IN RAPPL(1)                                                       
         DC    AL1(SHRSQ,OVR4,FVSHRS,10,0)                                      
         DC    AL1(L'DC@SHRS),AL2(DC@SHRS-T60400)                               
* STD RTE BUD IN RAPPL+1(1)                                                     
         DC    AL1(SRATQ,OVR4,FVSRAT,10,0)                                      
         DC    AL1(L'DC@SRAT),AL2(DC@SRAT-T60400)                               
* TARGET PERCENT IN RAPPL+2(                                                    
         DC    AL1(TPCTQ,OVR4,FVTPCT,10,0)                                      
         DC    AL1(L'DC@TPCT),AL2(DC@TPCT-T60400)                               
* (QOPT10) SALARY IN QOPT10                                                     
         DC    AL1(SALRQ,OVR3,FVSALR,1,0)                                       
         DC    AL1(L'DC@SALR),AL2(DC@SALR-T60400)                               
* LEVEL IN RAPPL+5(1)                                                           
         DC    AL1(LVL2Q,OVR4,FVLVL2,1,0)                                       
         DC    AL1(L'DC@LVL2),AL2(DC@LVL2-T60400)                               
* ACTIVITY START IN RACTSTRT                                                    
         DC    AL1(ASTDQ,OVR4,FVASTD,8,0)                                       
         DC    AL1(L'DC@ASTD),AL2(DC@ASTD-T60400)                               
* ACTIVITY END IN RACTEND(6)                                                    
         DC    AL1(AENDQ,OVR4,FVAEND,8,0)                                       
         DC    AL1(L'DC@AEND),AL2(DC@AEND-T60400)                               
* CLIENT IN RAPPL(3)                                                            
         DC    AL1(CLI4Q,OVR3,FVCLI4,3,0)                                       
         DC    AL1(L'DC@CLI4),AL2(DC@CLI4-T60400)                               
* MONTH IN RAPPL+6(2)                                                           
         DC    AL1(MNTHQ,OVR4,FVMNTH,6,0)                                       
         DC    AL1(L'DC@MNTH),AL2(DC@MNTH-T60400)                               
* CLIENT ACCOUNT IN RCUL(14)                                                    
         DC    AL1(CLIAQ,OVR4,FVCLIA,14,0)                                      
         DC    AL1(L'DC@CLIA),AL2(DC@CLIA-T60400)                               
* OFFICE IN ROFFICE - 'ALL' ED                                                  
         DC    AL1(OFFCQ,OVR3,FVOFFC,3,0)                                       
         DC    AL1(L'DC@OFFC),AL2(DC@OFFC-T60400)                               
* OPTION 8 IN QOPT8                                                             
         DC    AL1(OPT8Q,OVR3,FVOPT8,1,0)                                       
         DC    AL1(L'DC@OPT8),AL2(DC@OPT8-T60400)                               
* NARRATIVE                                                                     
         DC    AL1(NAR2Q,OVR4,FVNAR2,6,0)                                       
         DC    AL1(L'DC@NAR2),AL2(DC@NAR2-T60400)                               
* INCREMENT %                                                                   
         DC    AL1(INC2Q,OVR4,FVINC2,6,0)                                       
         DC    AL1(L'DC@INC2),AL2(DC@INC2-T60400)                               
* REVERSALS IN QREVERSE(1)                                                      
         DC    AL1(RVSLQ,OVR4,FVRVSL,1,0)                                       
         DC    AL1(L'DC@RVSL),AL2(DC@RVSL-T60400)                               
* STATUS IN QSTATUS(1)                                                          
         DC    AL1(STATQ,OVR4,FVSTAT,2,0)                                       
         DC    AL1(L'DC@STAT),AL2(DC@STAT-T60400)                               
* INCLUDE FLAGS IN QOPT7                                                        
         DC    AL1(INCFQ,OVR4,FVINCF,1,0)                                       
         DC    AL1(L'DC@INCF),AL2(DC@INCF-T60400)                               
* CYCLE MONTH IN QAPPL+6(2)                                                     
         DC    AL1(CYCMQ,OVR4,FVCYCM,6,0)                                       
         DC    AL1(L'DC@CYCM),AL2(DC@CYCM-T60400)                               
* OVERHEAD IN QFLT1(5)                                                          
         DC    AL1(OVRHQ,OVR4,FVOVRH,6,0)                                       
         DC    AL1(L'DC@OVRH),AL2(DC@OVRH-T60400)                               
* METHOD IN QMTHD                                                               
         DC    AL1(MTHDQ,OVR4,FVMTHD,3,0)                                       
         DC    AL1(L'DC@MTHD),AL2(DC@MTHD-T60400)                               
* PAYROLL CODE IN QSELECT                                                       
         DC    AL1(PYRLQ,OVR4,FVPYRL,5,0)                                       
         DC    AL1(L'DC@PYRL),AL2(DC@PYRL-T60400)                               
* BUCKET TYPE IN QSRTAREA+2                                                     
         DC    AL1(BKT2Q,OVR3,FVBKT2,1,0)                                       
         DC    AL1(L'DC@BKT2),AL2(DC@BKT2-T60400)                               
* CLIENT - VALIDATED IN 1F                                                      
         DC    AL1(CLI5Q,OVR3,FVCLI5,12,0)                                      
         DC    AL1(L'DC@CLI5),AL2(DC@CLI5-T60400)                               
* CALENDAR PERIOD IN ACQDTST                                                    
         DC    AL1(CALPQ,OVR3,FVCALP,17,0)                                      
         DC    AL1(L'DC@CALP),AL2(DC@CALP-T60400)                               
* BILLING DUE DAYS ACQBDAYS                                                     
         DC    AL1(DDUEQ,OVR4,FVDDUE,2,0)                                       
         DC    AL1(L'DC@DDUE),AL2(DC@DDUE-T60400)                               
* PERSON CODE - NEW COST VAL                                                    
         DC    AL1(PRNCQ,OVR3,FVPRNC,8,0)                                       
         DC    AL1(L'DC@PRNC),AL2(DC@PRNC-T60400)                               
* FR DEPARTMENT IN QAPPL                                                        
         DC    AL1(DPT2Q,OVR3,FVDPT2,3,0)                                       
         DC    AL1(L'DC@DPT2),AL2(DC@DPT2-T60400)                               
* AA LIVE RUN IN OPTION#4                                                       
         DC    AL1(LRUNQ,OVR3,FVLRUN,1,0)                                       
         DC    AL1(L'DC@LRUN),AL2(DC@LRUN-T60400)                               
* AA MEDIA IN QAPPL+8                                                           
         DC    AL1(MEDAQ,OVR4,FVMEDA,2,0)                                       
         DC    AL1(L'DC@MEDA),AL2(DC@MEDA-T60400)                               
* AA APP OPTION OPTION#9                                                        
         DC    AL1(APPOQ,OVR3,FVOPT9,1,0)                                       
         DC    AL1(L'DC@APPO),AL2(DC@APPO-T60400)                               
* AA TOLERANCE 1                                                                
         DC    AL1(TOLR1Q,OVR4,FVTOLR,5,0)                                      
         DC    AL1(12),AL2(DC@TOLR1-T60400)                                     
* AA TOLERANCE 2                                                                
         DC    AL1(TOLR2Q,OVR4,FVTOLR2,6,0)                                     
         DC    AL1(12),AL2(DC@TOLR2-T60400)                                     
* AA VENDOR LEVEL                                                               
         DC    AL1(VNDLQ,OVR3,FVVNDL,1,0)                                       
         DC    AL1(L'DC@VNDL),AL2(DC@VNDL-T60400)                               
* N/A FIELD                                                                     
         DC    AL1(NALQ,OVR4,FVNA,1,0)                                          
         DC    AL1(L'DC@NALQ),AL2(DC@NALQ-T60400)                               
* 25 ALTERNATE RUN TYPE IN OPTION#1                                             
         DC    AL1(ARUNQ,OVR3,FVARUN,1,0)                                       
         DC    AL1(L'DC@ARUN),AL2(DC@ARUN-T60400)                               
* 73 DJOB IN RDJOB(1)                                                           
         DC    AL1(DJOBQ,OVR4,FVDJOB,1,0)                                       
         DC    AL1(L'DC@DJOB),AL2(DC@DJOB-T60400)                               
* MT MISS OPTION=OPTION#1                                                       
         DC    AL1(MISSQ,OVR3,FVOPT1,1,0)                                       
         DC    AL1(L'DC@MISS),AL2(DC@MISS-T60400)                               
* MT TERM OPTION=OPTION#2                                                       
         DC    AL1(TERMQ,OVR3,FVOPT2,1,0)                                       
         DC    AL1(L'DC@TERM),AL2(DC@TERM-T60400)                               
* MT OVER OPTION=OPTION#3                                                       
         DC    AL1(OVERQ,OVR3,FVOPT3,1,0)                                       
         DC    AL1(L'DC@OVER),AL2(DC@OVER-T60400)                               
* MT WAIT OPTION=OPTION#4                                                       
         DC    AL1(WAITQ,OVR3,FVOPT4,1,0)                                       
         DC    AL1(L'DC@WAIT),AL2(DC@WAIT-T60400)                               
* MT DOWN OPTION=OPTION#7                                                       
         DC    AL1(DOWNQ,OVR3,FVOPT7,1,0)                                       
         DC    AL1(L'DC@DOWN),AL2(DC@DOWN-T60400)                               
* TT FORM                                                                       
         DC    AL1(FRM99Q,OVR4,FVFRM99,1,0)                                     
         DC    AL1(L'DC@FRM99),AL2(DC@FRM99-T60400)                             
* TX FUNT ACQUNT                                                                
         DC    AL1(FUNTQ,OVR3,FVUNIT,1,0)                                       
         DC    AL1(L'DC@FUNT),AL2(DC@FUNT-T60400)                               
* TX FLDG ACQLDG                                                                
         DC    AL1(FLDGQ,OVR3,FVLEDG,1,0)                                       
         DC    AL1(L'DC@FLDG),AL2(DC@FLDG-T60400)                               
* TX FACT ACQACT                                                                
         DC    AL1(FACTQ,OVR4,FVACCT,15,0)                                      
         DC    AL1(L'DC@FACT),AL2(DC@FACT-T60400)                               
* TX FWRK ACQTRNF                                                               
         DC    AL1(FWRKQ,OVR4,FVWCFT,3,0)                                       
         DC    AL1(L'DC@FWRK),AL2(DC@FWRK-T60400)                               
* TX FPER OPTION=ACQDTSTR                                                       
         DC    AL1(FPERQ,OVR3,FVCALP,17,0)                                      
         DC    AL1(L'DC@FPER),AL2(DC@FPER-T60400)                               
* TX FMOA ACQMOSST                                                              
*        DC    AL1(FMOAQ,OVR4,FVMOAR,13,0)                                      
*        DC    AL1(L'DC@FMOA),AL2(DC@FMOA-T60400)                               
* TX FCNT ACQCNTR                                                               
         DC    AL1(FCNTQ,OVR4,FVCONT,12,0)                                      
         DC    AL1(L'DC@FCNT),AL2(DC@FCNT-T60400)                               
* TX FTIM OPTION=OPTION#1                                                       
         DC    AL1(FTIMQ,OVR3,FVOPT1,1,0)                                       
         DC    AL1(L'DC@FTIM),AL2(DC@FTIM-T60400)                               
* TX FHRS OPTION=FILTER#6                                                       
         DC    AL1(FHRSQ,OVR3,FVFLTX6,6,0)                                      
         DC    AL1(L'DC@FHRS),AL2(DC@FHRS-T60400)                               
* TX FAMT OPTION=FILTER#5                                                       
         DC    AL1(FAMTQ,OVR4,FVOVRH,8,0)                                       
         DC    AL1(L'DC@FAMT),AL2(DC@FAMT-T60400)                               
* TX TUNT OPTION=FILTER#4(1)                                                    
         DC    AL1(TUNTQ,OVR3,FVFLTX4,1,0)                                      
         DC    AL1(L'DC@TUNT),AL2(DC@TUNT-T60400)                               
* TX TLDG OPTION=FILTER#4+1(1)                                                  
         DC    AL1(TLDGQ,OVR3,FVFLTX4A,1,0)                                     
         DC    AL1(L'DC@TLDG),AL2(DC@TLDG-T60400)                               
* TX TACT OPTION=FILTER#4+2(12)                                                 
         DC    AL1(TACTQ,OVR3,FVFLTX4B,12,0)                                    
         DC    AL1(L'DC@TACT),AL2(DC@TACT-T60400)                               
* TX TWRK OPTION=FILTER#3                                                       
         DC    AL1(TWRKQ,OVR3,FVFLTX3,2,0)                                      
         DC    AL1(L'DC@TWRK),AL2(DC@TWRK-T60400)                               
* TX TMOA OPTION=FILTER#2                                                       
         DC    AL1(TMOAQ,OVR3,FVFLTX2,6,0)                                      
         DC    AL1(L'DC@TMOA),AL2(DC@TMOA-T60400)                               
* TX TTIM OPTION=OPTION#4                                                       
         DC    AL1(TTIMQ,OVR3,FVOPT4,1,0)                                       
         DC    AL1(L'DC@TTIM),AL2(DC@TTIM-T60400)                               
* TX REPV OPTION=OPTION#6                                                       
         DC    AL1(REPVQ,OVR3,FVOPT6,1,0)                                       
         DC    AL1(L'DC@REPV),AL2(DC@REPV-T60400)                               
* TX REVR OPTION=FILTER#8                                                       
         DC    AL1(REVRQ,OVR3,FVFLTX8,6,0)                                      
         DC    AL1(L'DC@REVR),AL2(DC@REVR-T60400)                               
* TX LIVE OPTION=OPTION#3                                                       
         DC    AL1(LIVEQ,OVR3,FVOPT3,1,0)                                       
         DC    AL1(L'DC@LRUN),AL2(DC@LRUN-T60400)                               
* TX NARR ACQCARD5                                                              
         DC    AL1(NARRQ,OVR3,FVNARR,60,0)                                      
         DC    AL1(L'DC@NAR1),AL2(DC@NAR1-T60400)                               
* IO REVO OPTION=QOPT2                                                          
         DC    AL1(REVOQ,OVR3,FVOPT2,1,0)                                       
         DC    AL1(L'DC@REVO),AL2(DC@REVO-T60400)                               
* IO LIVR OPTION=QOPT1                                                          
         DC    AL1(LIVRQ,OVR3,FVOPT1,1,0)                                       
         DC    AL1(L'DC@LRUN),AL2(DC@LRUN-T60400)                               
* CHECK DELIVERY FLAG=QOPT8                                                     
         DC    AL1(CKDFQ,OVR3,FVOPT8,1,0)                                       
         DC    AL1(L'DC@CKDF),AL2(DC@CKDF-T60400)                               
*                                                                               
         DC    X'00'                                                            
                                                                                
DCBLANK  DC    C' '                                                             
DC@UNIT  DC    C'Unit'                                                          
DC@LEDG  DC    C'Ledger'                                                        
DC@ACCT  DC    C'Account'                                                       
DC@BGRP  DC    C'Billing Grp'                                                   
DC@FLT1  DC    C'Filter 1'                                                      
DC@FLT2  DC    C'Filter 2'                                                      
DC@STRD  DC    C'Start Date'                                                    
DC@ENDD  DC    C'End Date'                                                      
DC@SORT  DC    C'Sort Order'                                                    
DC@MGRP  DC    C'Media Group'                                                   
DC@OPT1  DC    C'Option 1'                                                      
DC@RECD  DC    C'RECORD DATE'                                                   
DC@CLI1  DC    C'CLIENT'                                                        
DC@PRD1  DC    C'Product'                                                       
DC@JOB   DC    C'Job'                                                           
DC@TS#R  DC    C'TS#Range'                                                      
DC@FREN  DC    C'French'                                                        
DC@OPT2  DC    C'Option 2'                                                      
DC@OPT3  DC    C'Option 3'                                                      
DC@FLT3  DC    C'Filter 3'                                                      
DC@MEF1  DC    C'Media Fltr'                                                    
DC@BFLT  DC    C'Billing Fltr'                                                  
DC@FLT5  DC    C'Filter 5'                                                      
DC@BATT  DC    C'Batch Type'                                                    
DC@PERS  DC    C'Person'                                                        
DC@CLI2  DC    C'Client'                                                        
DC@CHKD  DC    C'Check Date'                                                    
DC@TIED  DC    C'Title End Dte'                                                 
DC@FLT4  DC    C'Filter 4'                                                      
DC@BDAT  DC    C'Bill Date'                                                     
DC@OPT4  DC    C'Option 4'                                                      
DC@OPT5  DC    C'Option 5'                                                      
DC@ADVR  DC    C'Advertiser'                                                    
DC@SCHM  DC    C'Scheme'                                                        
DC@INC1  DC    C'Increment'                                                     
DC@ENDM  DC    C'End Month'                                                     
DC@OPT6  DC    C'Option 6'                                                      
DC@BKT1  DC    C'Bucket Type'                                                   
DC@CAF1  DC    C'CA Filter 1'                                                   
DC@RMON  DC    C'Report Month'                                                  
DC@BCHR  DC    C'Bill Char'                                                     
DC@TRNT  DC    C'Transactn Typ'                                                 
DC@COMM  DC    C'Commercial'                                                    
DC@EMPR  DC    C'Employer'                                                      
DC@EMPE  DC    C'Employee'                                                      
DC@BATR  DC    C'Batch Ref'                                                     
DC@BNUM  DC    C'Bill Number'                                                   
DC@SEQN  DC    C'Sequence'                                                      
DC@DRNG  DC    C'Day Range'                                                     
DC@GRPL  DC    C'Group Ledger'                                                  
DC@GRPC  DC    C'Group Code'                                                    
DC@RPTR  DC    C'Report'                                                        
DC@EXPT  DC    C'Expense Type'                                                  
DC@OBNO  DC    C'Ori Bill No.'                                                  
DC@OBDA  DC    C'Ori Bill Dte'                                                  
DC@RBDA  DC    C'Rev Bill Dte'                                                  
DC@UNTL  DC    C'Unit/List'                                                     
DC@LIST  DC    C'List'                                                          
DC@LVL1  DC    C'Level'                                                         
DC@CURR  DC    C'Current'                                                       
DC@YTD   DC    C'Ytd'                                                           
DC@YRLY  DC    C'Yearly'                                                        
DC@DATA  DC    C'Data Types'                                                    
DC@BUDT  DC    C'Budget Type'                                                   
DC@COMP  DC    C'Comparison'                                                    
DC@EXCP  DC    C'Exceptn List'                                                  
DC@CLDV  DC    C'Client/Div'                                                    
DC@OFDP  DC    C'Off/Dept'                                                      
DC@APRO  DC    C'A-Project'                                                     
DC@BDIV  DC    C'B-Division'                                                    
DC@CDPT  DC    C'C-Department'                                                  
DC@OPT7  DC    C'Option 7'                                                      
DC@DSDP  DC    C'D-Sub Dpt/Prj'                                                 
DC@ETSK  DC    C'E-Task'                                                        
DC@CAUL  DC    C'Contra U/L'                                                    
DC@XJOB  DC    C'Expense Jobs'                                                  
DC@SELT  DC    C'Select'                                                        
DC@CNTR  DC    C'Contra'                                                        
DC@RFMT  DC    C'Report Format'                                                 
DC@NAR1  DC    C'Narrative'                                                     
DC@DLYU  DC    C'Daily Units'                                                   
DC@BATG  DC    C'Batch Group'                                                   
DC@SRUN  DC    C'Single Run U/L'                                                
DC@CAF2  DC    C'CA Filter 2'                                                   
DC@RMNT  DC    C'Report Month'                                                  
DC@MOAR  DC    C'MOA Range'                                                     
DC@NUMB  DC    C'Number'                                                        
DC@OPT9  DC    C'Option 9'                                                      
DC@OPTA  DC    C'Option 10'                                                     
DC@MINA  DC    C'Minimum Amt'                                                   
DC@TMON  DC    C'Trans. Month'                                                  
DC@TYR   DC    C'Trans. Year'                                                   
DC@CCNV  DC    C'Currency Conv'                                                 
DC@CLI3  DC    C'Client'                                                        
DC@AGYF  DC    C'Agency Filter'                                                 
DC@MEF2  DC    C'Media Filter'                                                  
DC@BUF1  DC    C'Budget Filter'                                                 
DC@LIVF  DC    C'Live Forms'                                                    
DC@PRD2  DC    C'Product'                                                       
DC@FBUD  DC    C'From Budget'                                                   
DC@TBUD  DC    C'To Budget'                                                     
DC@WCFT  DC    C'Workcode Filt'                                                 
DC@UNN1  DC    C'Union'                                                         
DC@FCAU  DC    C'From Contr UL'                                                 
DC@UNN2  DC    C'Union'                                                         
DC@CAF3  DC    C'CA Filter 3'                                                   
DC@TCAU  DC    C'To Contra UL'                                                  
DC@DPT1  DC    C'Department'                                                    
DC@TASK  DC    C'Task'                                                          
DC@RPCD  DC    C'Replacement Cd'                                                
DC@ATTR  DC    C'Attribute'                                                     
DC@CAF4  DC    C'CA Filter 4'                                                   
DC@CAF5  DC    C'CA Filter 5'                                                   
DC@DATE  DC    C'Date Type'                                                     
DC@STAG  DC    C'Start Aging'                                                   
DC@ESTN  DC    C'Estimate Num'                                                  
DC@ENAG  DC    C'End Aging'                                                     
DC@BUF2  DC    C'Budget Types'                                                  
DC@USRF  DC    C'User Field'                                                    
DC@MMOS  DC    C'Media MOS'                                                     
DC@OFF1  DC    C'Office'                                                        
DC@OFF2  DC    C'Office'                                                        
DC@JOBG  DC    C'Job Group'                                                     
DC@WCTY  DC    C'Workcode Type'                                                 
DC@FRMT  DC    C'Format'                                                        
DC@TISD  DC    C'Title St Dte'                                                  
DC@SHRS  DC    C'Std Hrs Bud'                                                   
DC@SRAT  DC    C'Std Rte Bud'                                                   
DC@TPCT  DC    C'Target Prcnt'                                                  
DC@SALR  DC    C'Salary'                                                        
DC@LVL2  DC    C'Level'                                                         
DC@ASTD  DC    C'Actvty Start'                                                  
DC@AEND  DC    C'Actvty End'                                                    
DC@CLI4  DC    C'Client'                                                        
DC@MNTH  DC    C'Month'                                                         
DC@CLIA  DC    C'Client Acct'                                                   
DC@OFFC  DC    C'Office'                                                        
DC@OPT8  DC    C'Option 8'                                                      
DC@NAR2  DC    C'Narrative'                                                     
DC@INC2  DC    C'Increment %'                                                   
DC@RVSL  DC    C'Reversals'                                                     
DC@STAT  DC    C'Status'                                                        
DC@INCF  DC    C'Include Flags'                                                 
DC@CYCM  DC    C'Cycle Month'                                                   
DC@OVRH  DC    C'Overhead'                                                      
DC@MTHD  DC    C'Method'                                                        
DC@PYRL  DC    C'Payroll Code'                                                  
DC@BKT2  DC    C'Bucket Type'                                                   
DC@CLI5  DC    C'Client'                                                        
DC@CALP  DC    C'Calndr Period'                                                 
DC@DDUE  DC    C'Due Days'                                                      
DC@PRNC  DC    C'Person Code'                                                   
DC@DPT2  DC    C'Department'                                                    
DC@LRUN  DC    C'Live Run'                                                      
DC@MEDA  DC    C'Media'                                                         
DC@ARUN  DC    C'Alternate Run'                                                 
DC@DJOB  DC    C'Draft Jobs'                                                    
DC@APPO  DC    C'App Option'                                                    
DC@MISS  DC    C'Miss Time Stat'                                                
DC@TERM  DC    C'Term Employees'                                                
DC@OVER  DC    C'Overdue Time'                                                  
DC@WAIT  DC    C'Awaiting Apprv'                                                
DC@DOWN  DC    C'Download'                                                      
DC@STRM  DC    C'Start Month'                                                   
DC@TIMT  DC    C'Time Type'                                                     
DC@REPV  DC    C'Report View'                                                   
DC@REVO  DC    C'Reversal?'                                                     
DC@REVR  DC    C'Reversal Ref#'                                                 
DC@FUNT  DC    C'FROM Unit'                                                     
DC@TUNT  DC    C'TO Unit'                                                       
DC@FLDG  DC    C'FROM Ledger'                                                   
DC@TLDG  DC    C'TO Ledger'                                                     
DC@FACT  DC    C'FROM Account'                                                  
DC@TACT  DC    C'TO Account'                                                    
DC@FWRK  DC    C'FROM Workcode'                                                 
DC@TWRK  DC    C'TO Workcode'                                                   
DC@FMOA  DC    C'FROM MOA Range'                                                
DC@TMOA  DC    C'TO MOA'                                                        
DC@FTIM  DC    C'FROM Time Type'                                                
DC@TTIM  DC    C'TO Time Type'                                                  
DC@FPER  DC    C'FROM Period'                                                   
DC@FHRS  DC    C'FROM HOURS'                                                    
DC@FAMT  DC    C'FROM AMOUNT'                                                   
DC@FCNT  DC    C'FROM 1R Contra'                                                
DC@TOLR1 DC    C'Tolerance %%'                                                  
DC@TOLR2 DC    C'Tolerance $$'                                                  
DC@VNDL  DC    C'Vendor Level'                                                  
DC@FRM99 DC    C'Form Type'                                                     
DC@NALQ  DC    C'N/A'                                                           
DC@CKDF  DC    C'Check Flag'                                                    
*DC@CKDF  DC    C'For Future Use'                                               
         EJECT                                                                  
MOAVTBL  DS    0C                                                               
         DC    AL1(06,27),C'YNL+'                                               
         DC    AL1(06,31),C'YNL+'                                               
         DC    AL1(06,162),C'YNL+'                                              
         DC    AL1(03,165),C'M'    MMTH/YR ONLY                                 
         DC    AL1(0)                                                           
*                                                                               
SORTVTBL DS    0C                                                               
         DC    AL1(15,73),C'AF123456789TV'                                      
         DC    AL1(13,75),C'AF123456789'                                        
         DC    AL1(03,77),C'A'                                                  
         DC    AL1(0)                                                           
*                                                                               
       ++INCLUDE ACREQWORK                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'094ACREQ00   02/17/21'                                      
         END                                                                    
