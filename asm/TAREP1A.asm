*          DATA SET TAREP1A    AT LEVEL 219 AS OF 04/22/15                      
*PHASE T7031AB,*                                                                
*INCLUDE TAPPGUAR                                                               
*INCLUDE TALIM                                                                  
*INCLUDE DLFLD                                                                  
         TITLE 'T7031A - HOLDING FEE NOTIFICATION REPORT'                       
T7031A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 BTLNQ,T7031A,RA,R6                                               
         LR    RE,RC                                                            
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(SPOOL DSECT)                            
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING THD,R7              R7=A(LOCAL W/S)                              
         ST    RE,ABTAPREC                                                      
         EJECT                                                                  
*              ROUTINE TO CONTROL REPORT PROCESSING                             
*                                                                               
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
*                                                                               
         CLI   MODE,PRINTREP       RECOGINIZE ONLY PRINT REPORT MODE            
         BNE   XIT                                                              
*                                                                               
         BAS   RE,INIT             INITIALIZE                                   
*                                                                               
******** BRAS  RE,CRNTEST          CERNO TEST MESSAGE                           
******** BE    XIT                                                              
*                                                                               
         GOTOR AGYFLIST            IF AGENCY IS AN FLIST, SET UP AGYTAB         
         GOTOR ANYFLIST            SET UP TABLES FOR ANY OTHER FLIST            
*                                                                               
         BAS   RE,SELECT           SELECT HOLDING FEE POINTERS                  
*                                                                               
         L     R0,=AL4(NAGY*AGYLNQ)                                             
         GOTOR MYTRACE2,DMCB,=C'AGYTAB',(R0),A(AGYTAB)                          
*                                                                               
         L     R0,=AL4(NFAGY*FAGYLNQ)                                           
         GOTOR MYTRACE2,DMCB,=C'FAGYTAB',(R0),A(FAGYTAB)                        
*                                                                               
         L     R0,=AL4(NCLI*FCLLNQ)                                             
         GOTOR MYTRACE2,DMCB,=C'CLITAB',(R0),A(CLITAB)                          
*                                                                               
         L     R0,=AL4(NPRD*FPRLNQ)                                             
         GOTOR MYTRACE2,DMCB,=C'PRDTAB',(R0),A(PRDTAB)                          
*                                                                               
         L     R0,=AL4(NCOM*FCOLNQ)                                             
         GOTOR MYTRACE2,DMCB,=C'COMTAB',(R0),A(COMTAB)                          
*                                                                               
         L     R0,=AL4(NOFF*FOFLNQ)                                             
         GOTOR MYTRACE2,DMCB,=C'OFFTAB',(R0),A(OFFTAB)                          
*                                                                               
         BAS   RE,GETSORT          GET SORTED RECORDS/PRINT NOTICES             
*                                                                               
         BRAS  RE,CLOSTAPE         CLOSE EMS TAPE                               
*                                                                               
         BRAS  RE,CLOSEMQ          CLOSE MQ                                     
         BRAS  RE,CLOSEWF          CLOSE WEB FILE                               
         B     XIT                                                              
         EJECT                                                                  
*              INITIALIZATION ROUTINES                                          
*                                                                               
INIT     NTR1                                                                   
         ZAP   THFNO,=P'0'                                                      
         ZAP   THDSKCNT,=P'0'                                                   
         MVC   THEMAIL,SPACES                                                   
         MVC   THLEMAIL,SPACES                                                  
*                                                                               
         L     R3,ATWA                                                          
         USING T703FFD,R3                                                       
         L     R1,TWAMASTC                                                      
         USING MASTD,R1                                                         
         MVC   THWRITE,MCWRITE     SET WRITE SWITCH                             
         MVC   THRERUN,MCRERUN     SET RERUN SWITCH                             
         MVC   THCOMMIT,MCCOMMIT   SET COMMIT COUNTER                           
         DROP  R1,R3                                                            
         SPACE 1                                                                
         MVC   CMCOUNT,THCOMMIT    SET NUMBER OF HFEES PER COMMIT               
         SPACE 1                                                                
         USING COMFACSD,R1                                                      
         L     R1,SYSPARMS                                                      
         L     R1,16(R1)                                                        
         MVC   MYGETRET,CGETRET                                                 
         DROP  R1                                                               
         SPACE 1                                                                
*        CLI   ACTEQU,ACTDOWN                                                   
*        BE    IN10                                                             
IN05     MVC   SPECS,=A(MYSPECS)   SET A(SPECS) FOR HEADINGS                    
         LA    R1,HDHOOK           SET A(HEADLINE) HOOK                         
         ST    R1,HEADHOOK                                                      
         LA    R1,MDHOOK           SET A(MIDLINE) HOOK                          
         ST    R1,MIDHOOK                                                       
         L     R4,ABOX             R4=A(BOX AREA)                               
         USING BOXD,R4                                                          
         TM    THSTAT,THSUMM       COLS FOR SUMMARY SET ALREADY                 
         BO    IN10                                                             
         MVC   BOXHOOK,=A(BXHOOK)  SET A(BOX) HOOK                              
         MVI   RCSUBPRG,0          SET PROG FOR SPECS OF NON-SUMMARIES          
         B     XIT                                                              
*                                                                               
IN10     MVI   RCSUBPRG,1          SET PROG FOR SPECS OF SUMMARIES              
         CLI   RECNUM,HD                                                        
         BNE   *+8                                                              
         MVI   RCSUBPRG,2                                                       
         B     XIT                                                              
         EJECT                                                                  
*              SELECT QUALIFIED CAST HOLDING FEE POINTERS                       
*                                                                               
SELECT   NTR1                                                                   
         XC    TGAGY,TGAGY         SET TO GET FIRST AGENCY BREAK                
         XC    TGCOM,TGCOM         AND FIRST COMMERCIAL BREAK                   
*                                                                               
         GOTOR GETCOM              IF COMM IS POS FLIST, SETS 1ST COMM          
*                                  IN LIST AS TIFCOM                            
*                                                                               
SELA     XR    R3,R3               R3=L'COMPARE FOR READS                       
         OC    TIFCOM,TIFCOM                                                    
         BZ    *+8                                                              
         LA    R3,TLCAHCOM+L'TLCAHCOM-TLCAPD-1                                  
*                                                                               
SEL1     XC    KEY,KEY                                                          
         LA    R2,KEY              BUILD INITIAL KEY FOR HLD POINTERS           
         USING TLCAPD,R2                                                        
         MVI   TLCAPCD,TLCAHCDQ                                                 
         MVC   TLCAHCOM,TIFCOM     SET COMMERCIAL FILTER IF AROUND              
         B     *+10                                                             
SEL2     MVC   TLCAHSRT,HEXFFS     SET TO SKIP NEXT COMMERCIAL                  
         GOTO1 HIGH                                                             
         B     SEL6                                                             
*                                                                               
SEL4     GOTO1 SEQ                 GET NEXT                                     
*                                                                               
SEL6     EX    R3,*+8              L'COMPARE SET ABOVE BASED ON FILTERS         
         B     *+10                                                             
         CLC   TLCAPCD(0),KEYSAVE  TEST STILL HAVE GOOD KEY                     
         BE    SEL7                                                             
*                                                                               
         GOTOR GETNCOM             IF COMM IS A POSFLIST, GET NEXT COMM         
         BNE   XIT                 IF NO MORE COMM'S IN LIST, EXIT              
         B     SEL1                OR IF COMM IS NOT POS FLIST, EXIT            
*                                                                               
SEL7     CLC   TLCAHCOM,TGCOM      IF COMMERCIAL CHANGE                         
         BE    SEL8                                                             
         NI    THOPTS,ALL-THSPLIT                                               
         OC    TLCAHSRT,TLCAHSRT   AND THIS IS NOT POINTER FROM COMML           
         BNZ   SEL2                SKIP TO NEXT COMMERCIAL                      
*                                                                               
         BAS   RE,AGYRULES         GET AGY HFN RULES/TP OFFICE                  
         BNE   SEL2                REJECTED - SKIP TO NEXT COMMERCIAL           
*                                                                               
         BAS   RE,COMSET           SET COMMERCIAL DATA FOR SORT KEY             
         BNE   SEL2                REJECTED - SKIP TO NEXT COMMERCIAL           
         BAS   RE,CHKSPLIT         ELSE CHECK IF SPLIT CYCLE                    
         B     SEL12               GET NEXT HLD FEE PTR                         
*                                                                               
SEL8     L     RE,ATWA                                                          
         USING T703FFD,RE                                                       
         LA    RF,SHFCOMH                                                       
         GOTOR NEGFLIST            IF COMMERCIAL IS A NEGATIVE FLIST            
         BNE   SEL9                                                             
         DROP  RE                                                               
         GOTOR GETNGCO             IF COMMERCIAL IS IN COMTAB                   
         BE    SEL2                SKIP TO NEXT COMMERCIAL                      
*                                                                               
SEL9     OC    TLCAHSRT,TLCAHSRT   THIS SHOULD BE CAST MEMBER                   
         BNZ   *+6                                                              
         DC    H'0'                TERRIBLE PROBLEM - PTRS OUT OF SYNC          
*                                                                               
         BRAS  RE,ACTFLT           FILTER OUT ACTRA PERFS FOR 2404A             
         BNE   SEL12                                                            
*                                                                               
         BAS   RE,CYCCALC          CALC. CYCLE START FOR CAST MEMBER            
         BAS   RE,DATEFLT          FILTER ON REQUESTED PERIOD                   
**********************************************************************          
*&&DO                                                                           
         BE    SEL10               ** NO LONGER GENERATING HOLDING              
         CLI   THCOTYPE,CTYSEAS2   FEES ONCE A CYCLE IS MISSED **               
         BE    SEL12                                                            
         MVC   DUB(3),TCPCYCS      REJECTED, SO TRY NEXT CYCLE                  
         BRAS  RE,NXTSTART                                                      
         BAS   RE,DATEFLT          FILTER AGAIN                                 
*&&                                                                             
**********************************************************************          
         BNE   SEL12               GIVE UP IF STILL NOT OK                      
*                                                                               
SEL10    BAS   RE,PUTSORT          WRITE OUT SORT RECORD                        
*                                                                               
SEL12    B     SEL4                GET NEXT HOLDING FEE POINTER                 
         EJECT                                                                  
*              ROUTINE CHECKS IF THE CURRENT COMM'L HAS SPLIT HF CYCLES         
*                                                                               
         USING TLCAPD,R2           R2=A(COMML HOLDING FEE POINTER)              
CHKSPLIT NTR1                                                                   
         CLI   RECNUM,HD           ONLY FOR HFDISK                              
         BNE   XIT                                                              
         MVC   THKEY,KEY           SAVE CURRENT HLD FEE KEY                     
         GOTO1 SEQ                 GET NEXT POINTER                             
         CLC   TLCAHCOM,TGCOM                                                   
         BNE   CHKSPX              STOP IF COMM'L CHANGE                        
         BAS   RE,CYCCALC                                                       
         MVC   THSVCYCS,TCPCYCS    SAVE HF CYCLE START                          
         MVC   DUB(3),TCPCYCS                                                   
         BRAS  RE,NXTSTART         GET NEXT CYCLE AND SAVE IT                   
         MVC   THSVCYC2,TCPCYCS                                                 
CHKSP1   GOTO1 SEQ                                                              
         CLC   TLCAHCOM,TGCOM                                                   
         BNE   CHKSPX              STOP IF COMM'L CHANGE                        
         BAS   RE,CYCCALC                                                       
         CLC   THSVCYCS,TCPCYCS    KEEP LOOKING IF SAME CYCLE START             
         BE    CHKSP1                                                           
         BL    CHKSP4                                                           
         MVC   DUB(3),TCPCYCS      GET NEXT IF SAVED CYCLE IS LATER             
         BRAS  RE,NXTSTART                                                      
         CLC   THSVCYCS,TCPCYCS    KEEP LOOKING IF SAME CYCLE START             
         BE    CHKSP1                                                           
         B     CHKSP8                                                           
CHKSP4   CLC   THSVCYC2,TCPCYCS    CHECK SAVED NEXT CYCLE                       
         BE    CHKSP1                                                           
CHKSP8   OI    THOPTS,THSPLIT      SET SPLIT CYCLE STATUS                       
*                                                                               
CHKSPX   MVC   KEY,THKEY                                                        
         GOTO1 HIGH                RESTORE READ SEQUENCE                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GETS AGENCY HOLDING FEE RULES AND TP OFFICE              
*                                                                               
         USING TLCAPD,R2           R2=A(COMML HOLDING FEE POINTER)              
AGYRULES NTR1                                                                   
         OC    TIFAGY,TIFAGY       IF AGENCY FILTER DEFINED                     
         BZ    *+14                                                             
         CLC   TIFAGY,TLCAHAGY     CHECK IT                                     
         BNE   NO                                                               
*                                                                               
         GOTOR CHKNAGY             CHECK IF AGENCY IS A NEGATIVE FLIST          
         BE    NO                  IF SO, SKIP AGENCY IF IN TABLE               
*                                                                               
         BAS   RE,GETAGY           IF IN TABLE ALREADY                          
         BE    AGYRX               THEN SKIP                                    
*                                                                               
         L     RE,ATWA                                                          
         USING T703FFD,RE                                                       
         CLI   SHFAGY+1,C'-'                                                    
         BE    AGYR10                                                           
         CLI   SHFAGY,C'@'         IF AGENCY IS A POSITIVE FLIST                
         BE    NO                  AGENCY MUST BE IN TABLE OR SKIP              
         DROP  RE                                                               
*                                                                               
         USING AGYD,R3             R3=A(NEXT SLOT IN AGENCY TABLE)              
*                                                                               
AGYR10   MVC   TGAGY,TLCAHAGY      SET TO GET AGENCY RECORD                     
         MVC   THKEY,TLCAPKEY      SAVE HOLDING FEE KEY                         
*                                                                               
         GOTO1 NEWALL,DMCB,('TLAYCDQ',0),0,0,=C'AGENCY RULES'                   
*                                                                               
         MVC   TLCAPKEY,THKEY      RESTORE HOLDING FEE KEY                      
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ      GET AGENCY DETAILS ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R4                                                         
         MVC   AGYHLDS,TAAYHLDS    SAVE HOLDING FEE STATUS                      
         MVC   AGYOFF,TAAYTPOF     OFFICE                                       
         MVC   AGYTPC,TAAYTPC      TPC STAFF                                    
*                                                                               
         MVC   THHLAPEB,TAAYHLDS   SAVE WEB HOLDING FEE STATUS                  
         MVC   THAGYSRL,TAAYSGNS   SIGNATORY FEE CALC RULE                      
*                                                                               
         GOTOR SETPEND             SET PERIOD END DATE FOR THIS AGENCY          
         MVC   AGYLAST,THCYCE      SAVE IT                                      
*                                                                               
         GOTO1 HIGH                RE-READ HOLDING FEE PTR FOR SEQ READ         
*                                                                               
*                                  R3=A(AGYTAB ENTRY FOR THIS AGENCY)           
AGYRX    TM    AGYHLDS,TAAYHSNO    TEST AGENCY DOESN'T WANT NOTICES             
         BO    NO                                                               
         TM    THSTAT,THSECOND     IF WE'RE RUNNING SECOND NOTICES              
         BZ    *+12                                                             
         TM    AGYHLDS,TAAYHS2D    TEST AGENCY DOESN'T WANT THEM                
         BO    NO                                                               
*                                                                               
         OC    TIFOFF,TIFOFF       IS OFFICE FILTER DEFINED?                    
         BNZ   AGYRX20                                                          
         BRAS  RE,FILTOFF          NO, SEE IF OFFICE IS A FLIST                 
         BNE   NO                  IF SO, FILTER IT                             
         B     *+14                                                             
AGYRX20  CLC   TIFOFF,AGYOFF       YES, FILTER BY OFFICE                        
         BNE   NO                                                               
*                                                                               
         MVC   TGOFF,AGYOFF        SAVE TALENT PARTNERS OFFICE FOR SORT         
         MVC   THTPC,AGYTPC             TPC STAFF                               
         MVC   THCYCE,AGYLAST      SET PERIOD END DATE                          
*                                                                               
         GOTO1 DATCON,DMCB,(1,THCYCE),(0,WORK)  CALCULATE START DATE            
         GOTO1 ADDAY,DMCB,WORK,DUB,-183                                         
         GOTO1 DATCON,DMCB,(0,DUB),(1,THCYCS)                                   
*                                                                               
         CLC   AGYALPHA,=C'D3'     IF RUNNING ON FQA                            
         BNE   YES                                                              
         MVC   THCYCS,=X'A00101'   START DATE IS JAN01/00                       
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE GETS (NEXT) AGENCY ENTRY IN AGENCY TABLE                 
*                                                                               
         USING TLCAPD,R2           R2=A(COMML HOLDING FEE POINTER)              
GETAGY   DS    0H                                                               
         L     R3,=A(AGYTAB)       R3=A(TABLE)                                  
         USING AGYD,R3                                                          
GETA2    CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                (INCREASE NAGY)                              
         CLI   0(R3),0             NEXT SLOT                                    
         BE    GETAX                                                            
         CLC   TLCAHAGY,AGYAGY     ELSE MATCH ON AGENCY                         
         BE    GETA4               WE'VE SEEN THIS AGENCY ALREADY               
         LA    R3,AGYNEXT                                                       
         B     GETA2               KEEP ON LOOKING                              
*                                                                               
GETA4    MVC   TGAGY,AGYAGY        SET AGENCY                                   
*                                                                               
         MVC   THHLAPEB,AGYHLDS    SAVE WEB HOLDING FEE STATUS                  
*                                                                               
         CR    RE,RE               RETURN CC EQ - READ AGENCY ALREADY           
         BR    RE                  RETURNING R3=A(ENTRY FOR THIS AGY)           
*                                                                               
GETAX    MVC   AGYAGY,TLCAHAGY     BUILD NEW ENTRY                              
         LTR   RE,RE               RETURN CC NE - NEED TO READ AGENCY           
         BR    RE                  RETURNING R3=A(NEXT SLOT IN TABLE)           
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE FILTERS COMMERCIAL AND EXTRACTS DATA FOR SORT            
*                                                                               
         USING TLCAPD,R2           R2=A(COMML HOLDING FEE POINTER)              
COMSET   NTR1                                                                   
         TM    TLDRSTAT-TLDRD(R2),X'40'  TEST COPIED (TREAT AS DELETED)         
         BO    NO                                                               
*                                                                               
         OC    TIFCLI,TIFCLI       IF CLIENT FILTER DEFINED                     
         BZ    *+14                                                             
         CLC   TIFCLI,TLCAHCLI     CHECK IT                                     
         BNE   NO                                                               
         L     RE,ATWA                                                          
         USING T703FFD,RE                                                       
         LA    RF,SHFCLIH                                                       
         GOTOR POSFLIST            POSITIVE FLIST?                              
         BNE   COMS3                                                            
         GOTOR GETCLI              IF CLIENT IS A POSITIVE FLIST                
         BNE   NO                  IT MUST BE IN THE TABLE                      
         B     COMS5                                                            
COMS3    GOTOR NEGFLIST            NEGATIVE FLIST?                              
         BNE   COMS5                                                            
COMS4    GOTOR GETCLI              IF CLIENT IS A NEGATIVE FLIST                
         BE    NO                  IT MUST NOT BE IN THE TABLE                  
         DROP  RE                                                               
*                                                                               
COMS5    MVC   TGCLI,TLCAHCLI      SAVE CLIENT                                  
*                                                                               
*                                                                               
         OC    TIFPRD,TIFPRD       IF PRODUCT FILTER DEFINED                    
         BZ    *+14                                                             
         CLC   TIFPRD,TLCAHPRD     CHECK IT                                     
         BNE   NO                                                               
*                                                                               
         L     RE,ATWA                                                          
         USING T703FFD,RE                                                       
         LA    RF,SHFPRDH                                                       
         GOTOR POSFLIST            POSITIVE FLIST?                              
         BNE   COMS7                                                            
         GOTOR GETPRD              IF PRODUCT IS A POSITIVE FLIST               
         BNE   NO                  IT MUST BE IN THE TABLE                      
         B     COMS8                                                            
COMS7    GOTOR NEGFLIST            NEGATIVE FLIST?                              
         BNE   COMS8                                                            
         DROP  RE                                                               
         GOTOR GETPRD              IF PRODUCT IS A NEGATIVE FLIST               
         BE    NO                  IT MUST NOT BE IN THE TABLE                  
*                                                                               
COMS8    MVC   TGPRD,TLCAHPRD      SAVE PRODUCT                                 
*                                                                               
         MVC   TGCOM,TLCAHCOM      SAVE INTERNAL COMMERCIAL NUMBER              
         MVC   THFCYC,TLCAHDTE     AND COMMERCIAL 1ST FIXED CYCLE DATE          
*                                                                               
         GOTO1 GETREC              GET COMMERCIAL RECORD                        
         GOTOR MYTRACE,DMCB,=C'COMML SET'                                       
*                                                                               
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS EL.                   
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
*                                                                               
         MVC   TGCID,TACOCID       SAVE COMMERCIAL ID                           
         MVC   THCOTYPE,TACOTYPE   COMMERCIAL TYPE                              
         MVC   THCOSTA2,TACOSTA2   SECOND STATUS                                
         MVC   THCOCTYP,TACOCTYP   AND ACTRA TYPE                               
*                                                                               
         XC    TGCLG,TGCLG         CLEAR CLIENT GROUP                           
         CLI   RECNUM,HD           IF NOT HFDISK                                
         BE    *+10                                                             
         MVC   TGCLG,TACOCLG       SAVE CLIENT GROUP                            
         OC    TIFCLG,TIFCLG       IF CLIENT GROUP DEFINED                      
         BZ    COMS10                                                           
         CLC   TIFCLG,TACOCLG      CHECK IT                                     
         BNE   NO                                                               
         B     COMS20                                                           
*                                                                               
COMS10   OC    TIFAGY,TIFAGY       ELSE IF AGENCY FILTER DEFINED                
         BZ    COMS20                                                           
         CLC   TGCLG,=CL6'COKE'    SKIP IT IF CLIENT GROUP COKE                 
         BE    NO                                                               
*                                                                               
COMS20   BRAS  RE,QUALHFU          QUALIFY FOR HFUPDATE?                        
         BNE   NO                                                               
         BRAS  RE,QUALRER          QUALIFT FOR RERUN?                           
         BNE   NO                                                               
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE CALCULATES NEXT CYCLE START FOR CAST MEMBER              
*                                                                               
         USING TLCAPD,R2           R2=A(CAST HOLDING FEE POINTER)               
CYCCALC  NTR1                                                                   
         MVC   TCPCYCS,TLCAHNXT    SET NEW START TO LAST CYCLE NOTIFIED         
*                                                                               
         TM    THSTAT,THSECOND     IF THIS IS 2ND NOTICES THEN GET OUT          
         BO    CYCX                                                             
         CLI   RECNUM,HS           IF THIS IS SUMMARY THEN GET OUT              
         BE    CYCX                                                             
         CLI   THRERUN,C'Y'        IF THIS IS A RERUN THEN GET OUT              
         BE    CYCX                                                             
         CLI   THCOTYPE,CTYSEAS2   IF THIS IS A SEASONAL USE COMM'L             
         BE    CYC2                INFO                                         
         OC    TLCAHDTE,TLCAHDTE   IF APPLICABLE DATE NOT DEFINED               
         BZ    CYC2                CONTINUE                                     
******** OC    TIFCOM,TIFCOM       IF COMMERCIAL FILTER SPECIFIED               
******** BZ    *+14                                                             
******** CLC   TLCAHDTE,TLCAHNXT   AND IF APPLIC DATE IS LE LAST NOTIFD         
******** BNH   CYCX                THEN DONE                                    
*                                                                               
         OC    TLCAHDTE,TLCAHDTE   IF APPLICABLE DATE NOT DEFINED               
         BNZ   *+14                                                             
CYC2     MVC   DUB(3),THFCYC       SET TO CALC. 13W/3M FROM COMML FFC           
         B     CYC6                (THIS MUST BE FIRST HLD PAYMENT)             
*                                                                               
         CLC   TLCAHDTE,TLCAHFFC   IF APPLICABLE DATE IS CAST FFC               
         BNE   *+14                                                             
CYC4     MVC   DUB(3),TLCAHDTE     SET TO CALC. 13W/3M FROM COMML FFC           
         B     CYC6                (THIS MUST BE FIRST HLD PAYMENT)             
*                                                                               
         MVC   TCPCYCS,TLCAHDTE    ELSE THIS MUST BE NEXT START                 
         B     CYCX                (BASED ON TACREND+1 FROM LAST HLD)           
*                                                                               
CYC6     BRAS  RE,NXTSTART         CALCULATE NEXT START DATE                    
*                                                                               
CYCX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE FILTERS HOLDING FEE POINTERS BY DATE                     
*                                                                               
         USING TLCAPD,R2           R2=A(CAST HOLDING FEE POINTER)               
DATEFLT  NTR1                                                                   
         OC    THPSTRT(6),THPSTRT  TEST PERIOD REQUESTED                        
         BZ    DFLT2                                                            
         CLI   THCOTYPE,CTYSEAS2   AND COMMERCIAL IS NOT SEAONAL                
         BE    DFLT1                                                            
         CLC   TCPCYCS,THPSTRT     NEXT START MUST FALL WITHIN IT               
         BL    NO                                                               
         CLC   TCPCYCS,THPEND                                                   
         BH    NO                                                               
         B     DFLTX               WITHIN PERIOD - RETURN CC EQ                 
*                                                                               
         USING GETRETD,R3                                                       
DFLT1    LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
****     OI    GRDFLAG,GRDFTAL                                                  
         OI    GRDFLAG,GRDFTPU                                                  
         GOTO1 DATCON,DMCB,(1,TCPCYCS),(3,GRDIDY)                               
         MVC   GRDHRS,=Y(11*24)                                                 
         GOTO1 MYGETRET,(R3)                                                    
         GOTOR UNIOND,DMCB,MYGETRET,(R3),ELEM                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(3,GRDODY),(1,WORK)                                  
         CLC   THPSTRT,WORK                                                     
         BH    NO                                                               
         CLC   THPEND,WORK                                                      
         BL    NO                                                               
         B     DFLTX                                                            
         DROP  R3                                                               
*                                                                               
DFLT2    OC    TIFCOM,TIFCOM       IF COMML FILTER REQ., IGN AGY RULES          
         BNZ   DFLTX                                                            
*                                                                               
         CLC   TCPCYCS,THCYCS      IF NEXT START BEFORE AGY START DATE          
         BL    NO                  REJECT                                       
         CLC   TCPCYCS,THCYCE      OR AFTER AGENCY END DATE                     
         BH    NO                  REJECT                                       
*                                                                               
         TM    THSTAT,THSECOND     IF THIS IS NOT SECOND NOTICES                
         BO    DFLT6                                                            
         CLI   RECNUM,HS           THEN IF THIS IS SUMMARY                      
         BE    *+12                                                             
         CLI   THRERUN,C'Y'        OR THIS IS A RERUN                           
         BNE   DFLT4                                                            
         CLC   TLCAHMRK,TGTODAY1   THEN MUST BE MARKED TODAY                    
         BE    DFLTX                                                            
         B     NO                  ELSE REJECT                                  
*                                                                               
DFLT4    TM    THCOSTA2,TACOCHHF   IF COMM'L/CAST CHANGED SINCE LAST            
         BO    DFLTX               NOTICE, ACCEPT ALL CAST                      
*                                                                               
         CLC   TCPCYCS,TLCAHNXT    IF WE'VE HAD A NOTICE FOR CYCLE              
         BE    NO                  THEN REJECT                                  
         B     DFLTX                                                            
*                                                                               
DFLT6    CLC   TLCAHDTE,TCPCYCS    FOR 2ND NOTICES, IF NEW CYCLE'S BEEN         
         BH    NO                  PAID THEN REJECT                             
*                                                                               
DFLTX    B     YES                                                              
         EJECT                                                                  
*              ROUTINE WRITES RECORDS TO SORTER                                 
*                                                                               
         USING TLCAPD,R2           R2=A(CAST HOLDING FEE POINTER)               
PUTSORT  NTR1                                                                   
         TM    THSTAT,THSORTNG     IS SORT ACTIVE YET                           
         BO    PUTS2                                                            
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
*                                                                               
         OI    THSTAT,THSORTNG     SORT IS ACTIVE                               
*                                                                               
PUTS2    LA    R3,THSRTREC         R3=A(SORT RECORD)                            
         USING SORTD,R3                                                         
         MVI   SORTSTCG,0          CHECK IF CLIENT GROUP COKE                   
         CLC   TGCLG,=CL6'COKE'                                                 
         BE    *+8                                                              
         MVI   SORTSTCG,1                                                       
         MVC   SORTOFF,TGOFF       TP OFFICE                                    
         MVC   SORTTPC,THTPC       TPC STAFF                                    
         MVC   SORTAGY,TGAGY       AGENCY                                       
         MVC   SORTCLI,TGCLI       CLIENT                                       
         MVC   SORTPRD,TGPRD       PRODUCT                                      
*                                                                               
         BRAS  RE,PRCEMAIL                                                      
         MVC   AIO,AIO1                                                         
         MVC   SORTMAIL,THEMAIL                                                 
         MVC   SORTCID,TGCID       COMMERCIAL ID                                
         MVC   SORTCYCS,TCPCYCS    NEXT CYCLE START                             
         MVC   SORTSRT,TLCAHSRT    CAST SORT KEY                                
         MVC   SORTSSN,TLCAHSSN    SOCIAL SECURITY NUMBER                       
         MVC   SORTCOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         MVI   SORTSTAT,0                                                       
         TM    THOPTS,THSPLIT      SET SPLIT CYCLE IF NECESSARY                 
         BZ    *+8                                                              
         OI    SORTSTAT,SORTSTSP                                                
         MVC   SORTDA,TLCAPKEY+TLDRDA-TLDRD  D/A OF CAST RECORD                 
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',(R3)  WRITE OUT SORT RECORD                  
         GOTOR MYTRACE2,DMCB,=C'PUT',SORTLNQ,(R3)                               
         B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              ROUTINE PROCESSES RECORDS FROM SORT AND GENERATES REPORT         
*======================================================================         
GETSORT  NTR1                                                                   
         TM    THSTAT,THSORTNG     DON'T BOTHER IF SORT NOT ACTIVE              
         BZ    XIT                                                              
         MVI   TGOFF,X'FF'         FORCE ALL CONTROL BREAKS INITIALLY           
*                                                                               
         XC    HFCOUNT,HFCOUNT                                                  
         XC    LSTUPDCM,LSTUPDCM                                                
         NI    THSTAT3,X'FF'-THDLRCVR-THPRICOM-THEXPIRE                         
*                                                                               
         LA    R3,THSRTREC         R3=A(SORT RECORD)                            
         USING SORTD,R3                                                         
GETS10   GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   RF,15,4(R1)                                                      
         BZ    GETS40                                                           
         MVC   THSRTREC,0(RF)      MOVE SORT RECORD TO LOCAL AREA               
         GOTOR MYTRACE2,DMCB,=C'GET',SORTLNQ,(R3)                               
*                                                                               
         CLC   SORTKEY(SORTNEW),THLSTSRT  IF HIGH LEVEL KEY CHANGED             
         BE    GETS30                                                           
         CLI   TGOFF,X'FF'         AND THIS ISN'T FIRST TIME                    
         BE    GETS30                                                           
*                                                                               
         TM    THSTAT,THSUMM       IF SUMMARY OR DISK REPORT                    
         BZ    GETS20                                                           
         BAS   RE,NOTICE           PROCESS LAST NOTICE TO ACCUM TOTAL           
         BAS   RE,PRSUM            PRINT A SUMMARY LINE                         
         B     GETS25                                                           
*                                                                               
GETS20   BAS   RE,NOTICE           ELSE PRINT LAST NOTICE                       
GETS25   DS    0H                                                               
*                                                                               
GETS30   CLC   SORTMAIL,SPACES                                                  
         BNH   GETS35                                                           
         TM    THOPTS,THBDE        BDE OPTION ON?                               
         BZ    GETS35                                                           
         TM    THSTAT2,THBDEPQ                                                  
         BO    GETS33                                                           
         BRAS  RE,BDEPRTQ          ONCE ONLY                                    
         OI    THSTAT2,THBDEPQ                                                  
*                                                                               
GETS33   CLC   THLEMAIL,SORTMAIL                                                
         BE    GETS35                                                           
         MVC   THLEMAIL,SORTMAIL                                                
         MVC   THEMAIL,SORTMAIL                                                 
         BRAS  RE,BDEFOOT                                                       
         BRAS  RE,BDEHDR                                                        
*                                                                               
GETS35   MVC   KEY+TLDRDA-TLDRD(L'TLDRDA),SORTDA  SET D/A OF CAST REC.          
         MVC   AIO,AIO2                           USE I/O2                      
         MVI   RDUPDATE,C'Y'       NOT READY FOR WEB YET                        
         GOTO1 GETREC                             GET CAST RECORD               
         MVC   AIO,AIO1                           RESTORE DEFAULT I/O           
*                                                                               
         BAS   RE,QUALIFY          DETERMINE IF CAST RECORD QUALIFIES           
         BNE   GETS10                                                           
*                                  HANDLE CONTROL BREAK PROCESSING              
*                                                                               
         CLC   SORTOFF,TGOFF       OFFICE CHANGE                                
         BE    *+8                                                              
         BAS   RE,NEWOFF                                                        
*                                                                               
         CLC   SORTAGY,TGAGY       AGENCY CHANGE                                
         BE    *+8                                                              
         BAS   RE,NEWAGY                                                        
*                                                                               
         CLC   SORTCLI,TGCLI       CLIENT CHANGE                                
         BE    *+8                                                              
         BAS   RE,NEWCLI                                                        
*                                                                               
         CLC   SORTPRD,TGPRD       PRODUCT CHANGE                               
         BE    *+8                                                              
         BAS   RE,NEWPRD                                                        
*                                                                               
         CLC   SORTCID,TGCID       COMMERCIAL CHANGE                            
         BE    *+8                                                              
         BAS   RE,NEWCOM                                                        
*                                                                               
         CLC   SORTCYCS,TCPCYCS    CYCLE START CHANGE                           
         BE    *+8                                                              
         BAS   RE,NEWHFN           INITIALIZE HOLDING FEE NOTIFICATION          
*                                                                               
         BRAS  RE,SVSEASIN         SV ADDITIONAL SEASONAL INFO ON CAST          
*                                                                               
         BAS   RE,NEWCAST          PROCESS NEW CAST REC/ADD TO CASTTAB          
*                                                                               
         NI    THOPTS,ALL-THSPLIT                                               
         TM    SORTSTAT,SORTSTSP   SAVE SPLIT CYCLE FLAG                        
         BZ    *+8                                                              
         OI    THOPTS,THSPLIT                                                   
         MVC   THLSTSRT,SORTREC    SAVE THIS SORT RECORD AS LAST                
         B     GETS10              GET NEXT RECORD FROM SORTER                  
         SPACE 2                                                                
GETS40   TM    THSTAT,THSUMM       DONE - IF SUMMARY OR DISK REPORT             
         BZ    GETS50                                                           
         BAS   RE,NOTICE           PROCESS LAST NOTICE TO ACCUM TOTAL           
         BAS   RE,PRSUM            PRINT LAST LINE                              
         BAS   RE,PRAGYTOT         PRINT LAST AGY/CLI TOTAL                     
*                                                                               
         CLI   RECNUM,HD           IF HLD FEE DISK                              
         BNE   *+8                                                              
         BAS   RE,PUTFOOTR         WRITE OUT FOOTER RECORD                      
         SPACE                                                                  
         TM    THOPTS,THTRACE      AND IF TRACING, TOTALS AS WELL               
         BZ    GETSX                                                            
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         EDIT  (P4,THFNO),(7,P),COMMAS=YES                                      
         MVC   P+9(19),LTRECS                                                   
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         B     GETSX                                                            
*                                                                               
GETS50   BAS   RE,NOTICE           ELSE PRINT LAST NOTICE                       
*                                                                               
GETSX    GOTO1 SORTER,DMCB,=C'END'                                              
         XI    THSTAT,THSORTNG     SORT NO LONGER ACTIVE                        
         CLI   ACTEQU,ACTDOWN                                                   
         BNE   XIT                                                              
         GOTOR PREPD                                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DETERMINES IF CAST RECORD QUALIFIES                      
*                                                                               
         USING SORTD,R3            R3=A(SORT RECORD)                            
QUALIFY  NTR1                                                                   
         MVI   ELCODE,TACAELQ      GET CAST DETAILS EL.                         
         L     R4,AIO2             AIO2=A(CAST RECORD)                          
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         USING TACAD,R4            R4=A(CAST DETAILS EL.)                       
         MVC   THGRTCD,TACAGUA                                                  
         OC    TACALAST,TACALAST   IF LAST SERVICE DATE EXISTS                  
         BZ    *+14                                                             
         CLC   SORTCYCS,TACALAST   THEN REJECT IF CYCLE START AFTER IT          
         BH    NO                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 ASAVPTRS,DMCB,THPTRS  SAVE CAST POINTERS                         
         MVC   AIO,AIO1                                                         
*                                                                               
         MVI   ELCODE,TAHFELQ      GET HOLDING FEE DETAILS EL.                  
QUAL2    L     R4,AIO2                                                          
         BAS   RE,GETEL                                                         
         BE    QUAL4                                                            
         XC    ELEMENT(TAHFLNQ),ELEMENT  NOT FOUND - BUILD ONE                  
         LA    R4,ELEMENT                                                       
         USING TAHFD,R4            R4=A(HOLDING FEE DETAILS EL.)                
         MVI   TAHFEL,TAHFELQ                                                   
         MVI   TAHFLEN,TAHFLNQ                                                  
         MVC   AIO,AIO2                                                         
         GOTO1 ADDELEM             ADD IT TO RECORD                             
         MVC   AIO,AIO1                                                         
         B     QUAL2               AND TRY AGAIN                                
*                                                                               
QUAL4    DS    0H                                                               
* NO-OP  TM    TAHFSTAT,TAHFSNO    EXCLUDE THIS CAST MEMBER FROM NOTICE         
* NO-OP  BO    NO                                                               
         TM    THSTAT,THSECOND     IF WE'RE RUNNING SECOND NOTICES              
         BZ    *+12                                                             
         BAS   RE,SNQUAL           HANDLE QUAL. RULES UNIQUE TO THEM            
         B     *+8                                                              
         BAS   RE,HFQUAL           ELSE HANDLE QUAL. RULES FOR REG HFN          
         BNE   NO                                                               
         MVC   AIO,AIO2            CC EQ - RECORD HAS BEEN CHANGED              
*                                                                               
         TM    THSTAT,THSUMM       DON'T WRITE RECS IF SUMMARY OR DISK          
         BO    QUALX                                                            
         BRAS  RE,HFPUTREC         WRITE BACK THE RECORD                        
*                                                                               
         BRAS  RE,UPDCHK           IF UPDATING FILE                             
         BNE   QUALXX                                                           
         XR    RF,RF                                                            
         TM    THOPTS,THTRACE      IF LOCAL TRACE SET                           
         BZ    *+8                                                              
         LA    RF,X'10'            SET TO TRACE                                 
         GOTO1 AADDPTRS,DMCB,((RF),THPTRS)  UPDATE PASSIVE POINTERS             
         GOTOR MYTRACE,DMCB,=C'CAST'                                            
         B     QUALXX                                                           
*                                                                               
QUALX    GOTOR MYTRACE,DMCB,=C'CAST'                                            
QUALXX   MVC   AIO,AIO1            RESTORE I/O AREA                             
         B     YES                                                              
         EJECT                                                                  
*              QUALIFICATION RULES FOR HOLDING FEE NOTICE/SUMMARY               
*                                                                               
         USING TAHFD,R4            R4=A(HOLDING FEE DETAILS EL.)                
HFQUAL   NTR1                                                                   
         CLI   RECNUM,HS           IF THIS IS NOT SUMMARY                       
         BE    HFQX                                                             
*                                  * CAST QUALIFIES - MARK IT *                 
*        CLC   TAHFNXTS,SORTCYCS                                                
*        BE    *+10                                                             
         MVC   TAHFMDTE,TGTODAY1   MARK WITH TODAY'S DATE                       
         MVC   TAHFNXTS,SORTCYCS   SET NEXT START IN ELEMENT                    
         XC    TAHF2DTE,TAHF2DTE   CLEAR 2ND NOTICE DATE                        
HFQX     B     YES                 RETURN CC EQ TO WRITE BACK RECORD            
         SPACE 3                                                                
*              QUALIFICATION RULES FOR SECOND NOTICE/SUMMARY                    
*                                                                               
         USING TAHFD,R4            R4=A(HOLDING FEE DETAILS EL.)                
SNQUAL   NTR1                                                                   
         CLI   RECNUM,SS           IF THIS IS SUMMARY                           
         BE    *+12                                                             
         CLI   THRERUN,C'Y'        OR THIS IS A RERUN                           
         BNE   SNQ2                                                             
         CLC   TAHF2DTE,TGTODAY1   THEN MUST BE MARKED TODAY                    
         BE    SNQX                                                             
         B     NO                                                               
SNQ2     CLC   TAHFMDTE,TGTODAY1   IF SENT FIRST NOTICE TODAY REJECT            
         BE    NO                                                               
         OC    TAHF2DTE,TAHF2DTE   IF ALREADY SENT 2ND NOTICE REJECT            
         BNZ   NO                                                               
*                                  * CAST QUALIFIES - MARK IT *                 
         MVC   TAHF2DTE,TGTODAY1   MARK SECOND NOTICE SENT TODAY                
*                                                                               
SNQX     B     YES                 RETURN CC EQ TO WRITE BACK RECORD            
         EJECT                                                                  
*              PROCESS NEW OFFICE                                               
*                                                                               
         USING SORTD,R3            R3=A(SORT RECORD)                            
NEWOFF   NTR1                                                                   
         MVC   TGOFF,SORTOFF       SAVE NEW OFFICE                              
         GOTO1 NEWALL,DMCB,('TLOFCDQ',THOFFNM),THOFFAD,0,=C'OFFICE'             
*                                                                               
         GOTO1 SQUASHER,DMCB,THOFFAD,120  SQUASH ADDRESS FOR PRINTING           
*                                                                               
         MVI   TGAGY,X'FF'         SET TO RE-VALIDATE AGENCY                    
         B     XIT                                                              
         SPACE 3                                                                
*              PROCESS NEW AGENCY                                               
*                                                                               
NEWAGY   NTR1                                                                   
         MVC   TGAGY,SORTAGY       SAVE NEW AGENCY                              
         MVI   THAGYST,0           CLEAR AGENCY STATUS                          
         GOTO1 NEWALL,DMCB,('TLAYCDQ',THAGYNM),THAGYAD,THAGYATT,       X        
               =C'AGENCY'                                                       
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ      GET AGENCY DETAILS ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R4                                                         
         MVC   THTPC,TAAYTPC       TPC STAFF                                    
         MVC   THLCYC,=C'(3M) '    ASSUME 3 MONTH CYCLES                        
         TM    TAAYSTAT,TAAYS13W                                                
         BZ    *+10                                                             
         MVC   THLCYC,=C'(13W)'    AGENCY USES 13 WEEK CYCLES                   
         TM    TAAYHLDS,TAAYHSCO   IF WANT AGY COMM ON TOTALS                   
         BZ    *+8                                                              
         OI    THAGYST,THASCOMM    SET BIT IN LOCAL AGENCY STATUS               
*                                                                               
         MVC   TGEMP,TGTPEMP       SET DEFAULT EMPLOYER                         
         XC    TGBTDATA,TGBTDATA   CLEAR BILLING TYPE DATA                      
*                                                                               
         MVI   TGHNDRLS,0                                                       
         BAS   RE,BILLRULS         EXTRACT BILLING RULES                        
*                                                                               
         MVC   THAGYEMP,TGEMP      SAVE AGENCY LEVEL EMPLOYER                   
         MVC   THAGYBTY,TGBTYPE                      BILLING TYPE               
         MVC   THAGYBPO,TGBTYPO                      POOL TYPE                  
         MVC   THAGYBST,TGBTSTAT                     STATUS                     
         MVC   THAGYBRT,TGBSRC                       RATES                      
         MVC   THAGYHRL,TGHNDRLS                     HANDLING RULE              
*                                                                               
         USING TAAYD,R4                                                         
         L     R4,AIO              SAVE PAPER? ELECTRONIC? BOTH?                
         MVI   ELCODE,TAAYELQ      STATUS                                       
         BAS   RE,GETEL            IF NOT SET, SET AS PAPER ONLY                
         BNE   NAGY10                                                           
         MVC   THHLAPEB,TAAYHLDS   SAVE WEB HOLDING FEE STATUS                  
         DROP  R4                                                               
*                                                                               
NAGY10   MVI   TGCLI,X'FF'         SET TO RE-VALIDATE CLIENT                    
*                                                                               
         MVC   THEMAIL,SPACES                                                   
*                                                                               
*        BRAS  RE,GETEMAIL                                                      
         BRAS  RE,GETSIGN                                                       
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS NEW CLIENT                                               
*                                                                               
         USING SORTD,R3            R3=A(SORT RECORD)                            
NEWCLI   NTR1                                                                   
         ZAP   THAGYTOT,=P'0'      CLEAR AGENCY/CLIENT HFEE TOTAL               
         MVC   TGCLI,SORTCLI       SAVE NEW CLIENT                              
         GOTO1 NEWALL,DMCB,('TLCLCDQ',THCLINM),THCLIAD,THCLIATT,       X        
               =C'CLIENT'                                                       
*                                                                               
         MVC   TGEMP,THAGYEMP      SET AGENCY LEVEL EMPLOYER                    
         MVC   TGBTYPE,THAGYBTY                     BILLING TYPE                
         MVC   TGBTYPO,THAGYBPO                     POOL TYPE                   
         MVC   TGBTSTAT,THAGYBST                    STATUS                      
         MVC   TGBSRC,THAGYBRT                      RATES                       
         MVC   TGHNDRLS,THAGYHRL                    HANDLING RULE               
         MVC   THHLDPEB,THHLAPEB                    PRINT? ELECTRONIC?          
*                                                                               
         BAS   RE,BILLRULS         EXTRACT BILLING RULES                        
*                                                                               
         USING TACID,R4                                                         
         L     R4,AIO              IF PAPER? ELECTRONIC? BOTH?                  
         MVI   ELCODE,TACIELQ      SET AT CLIENT LEVEL                          
         BAS   RE,GETEL            SAVE IT                                      
         BNE   NCLI10                                                           
         TM    TACISTAT,TACIHSPO+TACIHSEL+TACIHSPE+TACIHSNC                     
         BZ    NCLI10                                                           
         MVC   THHLDPEB,TACISTAT                                                
         DROP  R4                                                               
*                                                                               
NCLI10   BAS   RE,GTCNTRL          GET CONTROL RECORD                           
*                                                                               
         CLC   THEMP,TGEMP         IF EMPLOYER HAS CHANGED, READ IT             
         BE    NCLIX                                                            
         GOTO1 NEWALL,DMCB,('TLEMCDQ',THEMPNM),0,0,=C'EMPLOYER'                 
         MVC   THEMP,TGEMP                                                      
*                                                                               
NCLIX    MVI   TGPRD,X'FF'         SET TO RE-VALIDATE PRODUCT                   
*        BRAS  RE,GETEMAIL                                                      
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS NEW PRODUCT                                              
*                                                                               
NEWPRD   NTR1                                                                   
         CLI   SORTSTCG,0          IF CGROUP COKE                               
         BNE   *+10                                                             
         ZAP   THAGYTOT,=P'0'      CLEAR AGY/CLI/PRD HFEE TOTAL                 
*                                                                               
         MVC   TGPRD,SORTPRD       SAVE NEW PRODUCT CODE                        
         XC    THPRDNM,THPRDNM                                                  
         XC    THPRDAD,THPRDAD                                                  
         XC    THPRDATT,THPRDATT                                                
*                                                                               
         OC    TGPRD,TGPRD         MAY NOT HAVE A PRODUCT                       
         BZ    NPRDX                                                            
         GOTO1 NEWALL,DMCB,('TLPRCDQ',THPRDNM),THPRDAD,THPRDATT,       X        
               =C'PRODUCT'                                                      
*                                                                               
NPRDX    MVI   TGCID,X'FF'         SET TO RE-VALIDATE COMMERCIAL                
*        BRAS  RE,GETEMAIL                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE EXTRACTS BILLING RULES DETAILS                           
*                                                                               
BILLRULS NTR1                                                                   
         MVI   ELCODE,TABRELQ      GET BILLING RULES ELEMENT                    
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         B     BRUL200                                                          
BRUL100  BAS   RE,NEXTEL                                                        
BRUL200  BNE   BRUL900                                                          
*                                                                               
         USING TABRD,R4                                                         
         TM    TABRSTAT,TABRSACP                                                
         BO    BRUL100                                                          
         OC    TABROEOR,TABROEOR   IF EMPLOYER OVERRIDE DEFINED,                
         BZ    *+10                                                             
         MVC   TGEMP,TABROEOR      MOVE TO GLOBAL STORAGE                       
*                                                                               
         CLI   TABRTYPE,0          IF BILLING TYPE DEFINED                      
         BE    BRUL900                                                          
         GOTO1 BTYPVAL,DMCB,TABRTYPE  VALIDATE IT                               
*                                                                               
         TM    TABRSTAT,TABRSSRC   IF BILL TYPE DEFINED & NOT USING SRC         
         BO    *+10                                                             
         MVC   TGBSRC,TABRRATE     USE RATES AS DEFINED IN ELEMENT              
*                                                                               
         CLI   TABRHRLS,0          IF HANDLING RULE IS DEFINED                  
         BE    BRUL900                                                          
         MVC   TGHNDRLS,TABRHRLS   SAVE HANDLING RULE                           
*                                                                               
BRUL900  BRAS  RE,ADDLBR           ADDITIONAL BILLING RULES                     
BRULX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GETS THE CONTROL RECORD AND SAVES SOME INFO              
         SPACE                                                                  
GTCNTRL  NTR1                                                                   
         XC    THEPRATE,THEPRATE                                                
         MVI   THEPCOMM,0                                                       
         GOTO1 RECVAL,DMCB,TLCTCDQ,(X'20',0)                                    
         L     R4,AIO                                                           
         MVI   ELCODE,TAEPELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAEPD,R4                                                         
         MVC   THEPCOMM,TAEPCOMM   SAVE COMMISSION CALC BASIS                   
         MVC   THEPRATE,TAEPRATE   AND RATE                                     
         CLI   TGBTYPE,20                                                       
         BNE   XIT                                                              
         MVC   THEPRATE,=H'750'                                                 
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS NEW COMMERCIAL                                           
*                                                                               
         USING SORTD,R3            R3=A(SORT RECORD)                            
NEWCOM   NTR1                                                                   
         MVC   TGCID,SORTCID       SAVE NEW COMMERCIAL ID                       
         MVC   TGCOM,SORTCOM                INTERNAL COMMERCIAL NO.             
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 NEWALL,DMCB,('TLCOCCDQ',THCOMNM),0,0,=C'COMMERCIAL'              
*                                                                               
         CLC   TGPRD,SPACES        IF DON'T HAVE PRODUCT CODE                   
         BH    NCOM2                                                            
         GOTO1 CHAROUT,DMCB,TAFNELQ,0,TAFNTPRD  LOOK FOR PRD NAME HERE          
         MVC   THPRDNM,TGNAME                                                   
*                                                                               
NCOM2    MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS EL.                   
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         GOTO1 CTYPVAL,DMCB,TACOTYPE                                            
         MVC   THTACO,0(R4)        SAVE IN OWN STORAGE                          
*                                                                               
         MVC   THCOTYPE,TACOTYPE   SAVE COMMERCIAL TYPE                         
         MVC   THCOSPCY,TACOSPCY   SAVE SPLIT CYCLE INDICATOR                   
         MVC   THCOSTA2,TACOSTA2   SAVE STATUS 2                                
         OI    THCOSPCY,C' '                                                    
*                                                                               
         LA    RE,THTACO                                                        
         ST    RE,TCATACO          SAVE ADDR. FOR RATE CALC. MODULE             
         USING TACOD,R4                                                         
         MVI   HALF,UHLD           SET USE TYPE TO PAY                          
         CLI   TACOTYPE,CTYSPAN    SPANISH COMMERCIALS                          
         BNE   *+8                                                              
         MVI   HALF,USHL           PAY DIFFERENT USE TYPE                       
         CLI   TACOTYPE,CTYADD     ADDENDUM COMMERCIALS                         
         BNE   *+8                                                              
         MVI   HALF,UADH           PAY DIFFERENT USE TYPE                       
         MVI   HALF+1,0                                                         
         GOTO1 USEVAL,DMCB,(X'80',HALF),HALF+1  SET GLOBAL USE VALUES           
*                                                                               
         GOTO1 MEDVAL,DMCB,TACOMED SET GLOBAL MEDIA VALUES                      
*                                                                               
*                                  EXTRACT SOME DATA FOR PRINTING               
         GOTO1 DATCON,DMCB,(1,TACOAIR),(8,THCOAIR)  1ST AIR DATE                
         GOTO1 (RF),(R1),(1,TACOFCYC),(8,THCOFCYC)  1ST FIXED CYCLE             
         GOTO1 (RF),(R1),(1,TACOFCYC),(20,THFCYC20)                             
         GOTO1 (RF),(R1),(1,TACOEXP),(8,THCOEXP)    EXPIRATION DATE             
         GOTO1 (RF),(R1),(1,TACOEXP),(20,THEXP20)                               
         EDIT  (1,TACOSEC),(3,THCOLEN),ALIGN=LEFT   COMML LENGTH                
         MVC   THCOVDTE,TACOVDTE                                                
*                                                                               
         XC    THCO2AIR,THCO2AIR                                                
         OC    TACOSAIR,TACOSAIR                                                
         BZ    NCOM5                                                            
         GOTO1 DATCON,DMCB,(1,TACOSAIR),(8,THCO2AIR)                            
*                                                                               
NCOM5    XC    THATTNM,THATTNM     CLEAR ATTENTION REC. AREA                    
         XC    THATTAD,THATTAD                                                  
         XC    THATTATT,THATTATT                                                
         MVC   TGATT,TACOATT       SET ATTENTION CODE                           
         OC    TGATT,TGATT         IF ATTENTION RECORD DEFINED                  
         BZ    NCOM6                                                            
         MVC   AIO,ATIA            GET IT (USE ALT. I/O AREA)                   
         GOTO1 NEWALL,DMCB,('TLATCDQ',THATTNM),THATTAD,THATTATT,       X        
               =C'ATTENTION'                                                    
         MVC   AIO,AIO1            (RESTORE DEFAULT I/O AREA)                   
*                                                                               
NCOM6    MVC   THLID,SPACES        LOOK FOR LIFT                                
                                                                                
         MVI   ELCODE,TALFELQ      LOOK FOR LIFT                                
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   NCOM7                                                            
         USING TALFD,R4                                                         
         MVC   THLID,TALFLID                       LIFT ID                      
         EDIT  (1,TALFSEC),(3,THLILEN),ALIGN=LEFT  LIFT LENGTH                  
         B     NCOM8                                                            
*                                                                               
         USING TAVRD,R4                                                         
NCOM7    L     R4,AIO              ELSE LOOK FOR VERSION 2                      
         MVI   ELCODE,TAVRELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   NCOM8                                                            
         OI    TCPAYST2,TCHASVER                                                
         BAS   RE,NEXTEL                                                        
         BNE   NCOM8                                                            
         CLI   TAVRVERS,2                                                       
         BNE   NCOM8                                                            
         MVC   THLID,TAVRCID                       VERSION 2 ID                 
         EDIT  (1,TAVRSEC),(3,THLILEN),ALIGN=LEFT  VERSION 2 LENGTH             
         DROP  R4                                                               
                                                                                
NCOM8    GOTO1 GETTACS,DMCB,('TACSTYPF',THFIDATA),TCFLMDTE  FILM DATA           
         GOTO1 (RF),(R1),('TACSTYPR',THREDATA),TCRECDTE  RECORD DATA            
*                                                                               
         MVC   TCPCYCS,HEXFFS      SET CYCLE START CHANGE                       
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE EXTRACTS STUDIO DATA FROM COMMERCIAL RECORD              
*                                                                               
GETTACS  NTR1                                                                   
         MVC   BYTE,0(R1)          BYTE=TYPE EQUATE                             
         LM    R2,R3,0(R1)         R2=A(AREA IN W/S)                            
*                                  R3=A(AREA FOR DATE FOR RATE CALC)            
*                                                                               
         MVC   0(THFILNQ,R2),SPACES  PRE-CLEAR W/S                              
         XC    0(3,R3),0(R3)                                                    
*                                                                               
         MVI   ELCODE,TACSELQ      SET STUDIO ELEMENT CODE                      
         GOTO1 GETL,DMCB,(1,BYTE)  GET ELEMENT                                  
         BNE   GTACSX                                                           
         L     R4,TGELEM           R4=A(STUDIO ELEMENT)                         
         USING TACSD,R4                                                         
         MVC   0(3,R3),TACSDATE    SET DATE FOR RATE CALC MODULE                
         GOTO1 DATCON,DMCB,(1,TACSDATE),(8,(R2))     DATE                       
         MVC   8(L'TACSSTUD,R2),TACSSTUD             STUDIO                     
         OC    8(L'TACSSTUD,R2),SPACES                                          
         MVC   8+L'TACSSTUD(L'TACSCITY,R2),TACSCITY  CITY                       
         OC    8+L'TACSSTUD(L'TACSCITY,R2),SPACES                               
*                                                                               
GTACSX   B     XIT                                                              
         EJECT                                                                  
*              COMMON ROUTINE TO HANDLE NEW HIGH LEVEL RECORDS                  
*                                                                               
NEWALL   NTR1                                                                   
         LM    R2,R4,0(R1)         R2=A(NAME)  R3=A(ADDR)  R4=A(ATTN)           
         L     R0,12(R1)           R0=(L'LIT, A(LIT) FOR TRACE)                 
         ZIC   RF,0(R1)            RF=RECORD CODE                               
         SLL   R2,8                                                             
         SRL   R2,8                SQUEEZE OUT HOB                              
*                                                                               
         GOTO1 RECVAL,DMCB,(RF),(X'20',0)  READ RECORD                          
         BE    NALL6                                                            
         CLI   KEYSAVE,TLW4CDQ     IF W4 RECORD NOT FOUND                       
         BE    NO                  RETURN CC NE                                 
         CLI   KEYSAVE,TLATCDQ     IF ATTENTION RECORD NOT FOUND                
         BE    NO                  RETURN CC NE                                 
*                                                                               
         CLI   KEYSAVE,TLPRCDQ     DON'T DIE ON PRODUCT RECORD                  
         BE    NALL6                                                            
         DC    H'0'                ELSE DIE                                     
*                                                                               
NALL6    LTR   R2,R2               IF WE HAVE A(NAME IN W/S)                    
         BZ    NALL8                                                            
         GOTO1 CHAROUT,DMCB,TANAELQ,0      GET NAME                             
         MVC   0(L'THNAMES,R2),TGNAME      SAVE IT                              
*                                                                               
NALL8    LTR   R3,R3                       IF A(ADDRESS IN W/S PASSED)          
         BZ    NALL10                                                           
         XC    BLOCK(8),BLOCK              INITIALIZE TEMPORARY AREA            
         MVI   BLOCK,L'THADDRS+8                                                
         GOTO1 CHAROUT,DMCB,TAADELQ,BLOCK  EXTRACT ADDRESS                      
         MVC   0(L'THADDRS,R3),BLOCK+8     SAVE IN LOCAL W/S                    
         OC    0(L'THADDRS,R3),SPACES                                           
*                                                                               
NALL10   LTR   R4,R4               IF WE HAVE A(ATTENTION NAME IN W/S)          
         BZ    NALLX                                                            
         GOTO1 CHAROUT,DMCB,TAFNELQ,0,TAFNTATT  GET ATTENTION NAME              
         MVC   0(L'THATTNMS,R4),TGNAME          SAVE IT                         
*                                                                               
NALLX    GOTOR MYTRACE,DMCB,(R0)   PRINT TRACE IF REQUESTED                     
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE INITIALIZES VARIOUS DATA FOR NOTICE PRINTING             
*                                                                               
         USING SORTD,R3            R3=A(SORT RECORD)                            
NEWHFN   NTR1                                                                   
         MVC   TCPCYCS,SORTCYCS    SAVE NEW CYCLE START                         
*                                                                               
         GOTO1 DATCON,DMCB,(1,TCPCYCS),(8,WORK) CONVERT TO MMMDD/YY             
         MVI   WORK+8,C'-'                                                      
         MVC   WORK+9(5),THLCYC    SET L'CYCLE                                  
*                                                                               
         LA    R3,BLOCK            R3=A(OUTPUT BLOCK FROM PERVAL)               
         USING PERVALD,R3                                                       
         GOTO1 PERVAL,DMCB,(14,WORK),('PVIN1DYL',(R3))  GET CYCLE END           
         MVC   TCPCYCE,PVALPEND    SAVE CYCLE END DATE                          
*                                                                               
         TM    THSTAT,THSUMM       IF NOT SUMMARIES OR DISK                     
         BO    *+8                                                              
         MVI   FORCEHED,C'Y'       START ON NEW PAGE                            
         ZAP   THPAGE,=P'1'        INITIALIZE TO PAGE 1                         
         XC    THHFEE,THHFEE       CLEAR HFEE AMT WITH GRT APPLIED              
         NI    THSTAT,ALL-THPRNTED-THDSK2ND                                     
         NI    THOPTS,ALL-THCAGUA                                               
         MVC   THANXTCS,=A(CASTTAB)                                             
*                                                                               
         XC    THTOTS(THTOTLNQ),THTOTS  CLEAR LOCAL TOTAL FIELDS                
         B     YES                                                              
         EJECT                                                                  
*              PROCESS NEW CAST RECORD                                          
*                                                                               
         USING SORTD,R3            R3=A(SORT RECORD)                            
NEWCAST  NTR1                                                                   
         XC    TCCAST(TCCSTLNQ),TCCAST  INIT INDIV. CAST AREA                   
         LA    RE,TCD                                                           
         AHI   RE,TCCSTBRK-TCD                                                  
         XC    0(L'TCCSTBRK,RE),0(RE)                                           
*                                                                               
         L     R4,AIO2                                                          
         ST    R4,TCACAST          SAVE A(RECORD) FOR CALC.                     
         USING TLCAD,R4                                                         
         GOTO1 CATVAL,DMCB,TLCACAT  SET CATEGORY DETAILS                        
*                                                                               
         MVI   ELCODE,TACAELQ      GET CAST DETAILS EL.                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R4            R4=A(CAST DETAILS EL.)                       
         GOTO1 SETOV2,DMCB,(R4),TCACAST,TGUSCDE                                 
*                                                                               
         GOTO1 UNIVAL,DMCB,TACAUN   SET UNION DETAILS                           
         GOTO1 YRVAL,DMCB,TACAYEAR  AND YEAR DETAILS                            
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETOV1,DMCB,TGUSCDE,TCOV1  1ST OVERSCALE RATE                    
         MVC   AIO,AIO1                                                         
         CLI   0(R1),X'FF'         IF IT'S AN AMOUNT                            
         BNE   NCAS10                                                           
         MVC   TCPAY,TCOV1         SAVE AS PAYMENT                              
         OI    TCINPUT,TCINPAY     SET WE HAVE OVERRIDE                         
         OI    TCCASTST,TCCAOVAM                                                
         XC    TCOV1,TCOV1         AND CLEAR RATE                               
*                                                                               
NCAS10   MVC   TCCADBL,TACADBL     N'DOUBLES                                    
         MVC   TCOV2,TACAOV2       2ND OVERSCALE RATE                           
         MVC   TCCASTAT,TACASTAT   STATUS BYTE                                  
         MVC   TCCAONOF,TACAONOF   ON/OFF CAMERA                                
         MVC   TCCAFCYC,TACAFCYC   FIRST FIXED CYCLE                            
         MVC   TCCAFRST,TACAFRST   FIRST SERVICES                               
         DROP  R4                                                               
*                                                                               
         OI    TCPAYST,TCHLDLR     SET CALLED FROM HOLDING FEES STATUS          
         MVC   TGTHREE,TGYRCDE                                                  
         GOTO1 TASYSCLC,DMCB,(RC),TCD,SYSCOMM  GO CALCULATE RATES               
         MVC   TGYRCDE,TGTHREE                                                  
*                                                                               
         LA    RE,TCD                                                           
         TM    TCRTRN-TCD(RE),TCRTDLR                                           
         BZ    NCAS20              IF PERF IS COVERED BY DEALER CYCLE           
         OI    THSTAT3,THDLRCVR    WILL NEED TO PRINT MESSAGE                   
*                                                                               
         USING TACAD,R4                                                         
NCAS20   L     R4,TCACAST                                                       
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   NCAS30                                                           
         OC    TACAEXP,TACAEXP     IF CAST HAS AN EXPIRATION DATE               
         BZ    NCAS40                                                           
         CLC   TACAEXP,TCPCYCE     AND IT IS EARLIER THAN CYCLE                 
         BH    NCAS30              END DATE                                     
         OI    THSTAT3,THEXPIRE    SET TO PRINT EXPIRY WARNING                  
         B     NCAS40                                                           
                                                                                
         USING TACOD,R1                                                         
NCAS30   LA    R1,THTACO                                                        
         OC    TACOEXP,TACOEXP     IF COMMERCIAL HAS AN EXPIRATION              
         BZ    NCAS40              DATE                                         
         CLC   TACOEXP,TCPCYCE     AND IT IS EARLIER THAN CYCLE                 
         BH    NCAS40              END DATE                                     
         OI    THSTAT3,THEXPIRE    SET TO PRINT EXPIRY WARNING                  
         DROP  R1                                                               
                                                                                
NCAS40   BRAS  RE,PUTCAST          ADD CAST MEMBER TO CAST TABLE                
*                                                                               
NCASX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS PRINTING OF HOLDING FEE NOTICE                  
*                                                                               
NOTICE   NTR1                                                                   
         XC    THSURTOT,THSURTOT       SUI SURCHARGE TOTAL                      
         NI    THSTAT2,X'FF'-THGRTEXP                                           
*                                                                               
         TM    THSTAT2,THEMS       IF EMS FLAG IS SET,                          
         BNO   NOT3                                                             
         MVC   SPECS,=A(MYSPECS)   RESET A(SPECS) FOR HEADINGS                  
         MVC   HEADHOOK,=A(HDHOOK)                                              
         MVC   MIDHOOK,=A(MDHOOK)                                               
         NI    THSTAT2,X'FF'-THEMS RESET EMS FLAG                               
NOT3     L     R2,=A(CASTTAB)      R2=A(CAST TABLE)                             
         USING CASTD,R2                                                         
         BAS   RE,CNTPAGES         COUNT N'PAGES TO BE PRINTED                  
         BNE   NOTX                                                             
*                                                                               
         BRAS  RE,OPENMQ           OPEN MQ                                      
*                                                                               
         BRAS  RE,CALCDUE          CALCULATE DUE DATE                           
*                                                                               
         BRAS  RE,HFORUSCK         IF HOLDING FEE OR HOLDING FEE UPDATE         
         BNE   NOT5                                                             
         CLI   TGBTYPE,TABRTYE     BILLING TYPE E (EMS),                        
         BNE   NOT5                                                             
         OI    THSTAT2,THEMS       SET FOR EMS, HF, OVERNIGHT                   
         GOTOR OPENTAPE            OPEN TAPE IF NOT OPENED                      
         XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
         XC    MIDHOOK,MIDHOOK                                                  
         BRAS  RE,PUTDOCHD         PUT DOC ID HEADERS TO WEB FILE               
         BRAS  RE,BLDHEAD          PUT HEADINGS TO DATASET                      
         GOTOR PUTTAPE             PUT RECORD TO EMS TAPE/WEB FILE              
         BRAS  RE,BLDMHEAD         PUT MORE HEADINGS TO DATASET                 
         GOTOR PUTTAPE             PUT RECORD TO EMS TAPE/WEB FILE              
         B     NOT5A                                                            
*                                                                               
NOT5     TM    THSTAT,THSUMM       IF THIS IS NOT A SUMMARY                     
         BO    NOT5A                                                            
         TM    THHLDPEB,TAAYHSEL   AND AGENCY/CLIENT IS SET UP                  
         BZ    NOT5A               FOR ELECTRONIC ONLY                          
         BRAS  RE,FAKEHEAD         MUST FAKE OUT HEADHOOK                       
*                                                                               
NOT5A    BAS   RE,GETCAST          GET CAST MEMBER FROM CAST TABLE              
         BNE   NOT8                W4 RECORD NOT FOUND - SKIP                   
         TM    THSTAT2,THEMS       HFEE EMS OVERNIGHT                           
         BO    NOT7                SKIP THIS                                    
*                                                                               
         CLI   LINE,LQDTLTOT-1     IF WE'VE REACHED TOP OF DTL TOTALS           
         BE    NOT6                                                             
         CLI   LINE,LQDTLTOT-2     OR WE'VE REACHED ONE BEFORE DTL TOTS         
         BNE   *+16                                                             
         CLI   CASTCORP,0          AND THIS ENTRY HAS A CORPORATION             
         BE    *+8                                                              
NOT6     BAS   RE,CONTINUE         SET TO CONTINUE ON NEXT PAGE                 
*                                                                               
NOT7     BAS   RE,DETAIL           PRINT A DETAIL LINE                          
*                                                                               
NOT8     LA    R2,CASTNEXT         BUMP TO NEXT CAST TABLE ENTRY                
*                                                                               
         CLI   0(R2),0             STOP IF LOGICAL END OF TABLE                 
         BE    *+12                                                             
         CLI   0(R2),X'FF'         OR PHYSICAL END OF TABLE                     
         BNE   NOT5                                                             
         TM    THSTAT,THPRNTED     DONE - IF PRINTED DETAIL,                    
         BZ    NOT10                                                            
         TM    THSTAT2,THEMS       HFEE, EMS, OVERNIGHT                         
         BNO   NOT9                                                             
         GOTOR CALCEMS             CALC EMS TOTALS AND PUT TO DATASET           
         B     *+8                                                              
NOT9     BAS   RE,TOTALS           PRINT TOTALS                                 
         NI    THSTAT3,X'FF'-THDLRCVR-THPRICOM-THEXPIRE                         
*                                                                               
NOT10    TM    THSTAT,THSUMM       IF SUMMARY OR DISK, DO NOT CLEAR             
         BNO   NOTX                CAST TABLE                                   
         CLI   RECNUM,HD           IF HLD FEE DISK                              
         BNE   XIT                                                              
         TM    THOPTS,THCAGUA      AND HAVE CAST WITH GRT CODE                  
         BZ    XIT                                                              
         MVC   THHFEE,TGDUB        SAVE HFEE AMT WITH GRT APPLIED               
         OI    THSTAT,THDSK2ND     SET 2ND LOOP FLAG                            
         XI    THOPTS,THCAGUA      CLEAR FLAG TO PREVENT ENDLESS LOOP           
         XC    THTOTS(THTOTLNQ),THTOTS  CLEAR LOCAL TOTAL FIELDS                
         L     R2,=A(CASTTAB)                                                   
         B     NOT5                                                             
*                                                                               
NOTX     L     RE,=A(CASTTAB)      RE-INITIALIZE CAST TABLE                     
         L     RF,=AL4(NCAST*CASTLNQ)                                           
         XCEF                                                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE SETS N'PAGES TO BE PRINTED                               
*                                                                               
         USING CASTD,R2            R2=A(CAST TABLE)                             
CNTPAGES NTR1                                                                   
         XR    R1,R1               R1=N'PAGES                                   
         LA    RF,NCASTPG          RF=MAX N'CAST/PAGE (START NEW PAGE)          
*                                                                               
CNTP2    CLI   0(R2),0             STOP IF LOGICAL END OF TABLE                 
         BE    CNTP6                                                            
         CLI   0(R2),X'FF'         OR PHYSICAL END OF TABLE                     
         BE    CNTP6                                                            
         CLM   RF,1,=AL1(NCASTPG)  IF NO ROOM LEFT ON PAGE                      
         BNE   *+10                                                             
         LA    R1,1(R1)            BUMP PAGE COUNT NOW                          
         XR    RF,RF               AND CLEAR N'ENTRIES FOR PAGE                 
*                                                                               
         LA    RF,1(RF)            BUMP COUNT OF N'ENTRIES THIS PAGE            
*                                                                               
         CLI   CASTCORP,0          TEST CORP CODE DEFINED                       
         BE    CNTP4                                                            
         CLM   RF,1,=AL1(NCASTPG)  IF NO ROOM FOR EXTRA LINE                    
         BNE   *+12                                                             
         LA    R1,1(R1)            BUMP PAGE COUNT NOW                          
         LA    RF,1                AND RESET N'ENTRIES FOR PAGE TO ONE          
*                                                                               
         LA    RF,1(RF)            NEED AN ADDITIONAL LINE FOR CORP             
*                                                                               
CNTP4    LA    R2,CASTNEXT         BUMP TO NEXT ENTRY IN TABLE                  
         B     CNTP2                                                            
*                                                                               
CNTP6    STC   R1,THNPAGES         SAVE N'PAGES                                 
*                                                                               
         LTR   R1,R1               ANYTHING IN TABLE                            
         BNZ   YES                                                              
         B     NO                                                               
         EJECT                                                                  
*              ROUTINE GETS A CAST MEMBER FROM CAST TABLE                       
*                                                                               
         USING CASTD,R2            R2=A(NEXT ENTRY)                             
GETCAST  NTR1                                                                   
         MVI   THCASTAT,0                                                       
         MVC   TGSSN,CASTSSN       SET TO GET W4 NAME                           
         MVC   CASTPAYI,CASTSPAY   RESET PAYMENT AMOUNT IN CASE DISK            
         MVC   CASTPNH,CASTSVPH          PNH AMOUNT                             
*                                                                               
         GOTO1 NEWALL,DMCB,('TLW4CDQ',THSSNNM),0,0,=C'W4'  GET W4 NAME          
         BNE   NO                                                               
*                                                                               
         MVI   ELCODE,TAW4ELQ      GET W4 DETAILS EL.                           
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAW4D,R4            R4=A(W4 DETAILS EL.)                         
         MVC   TCW4TYPE,TAW4TYPE   SAVE W4 TYPE                                 
         XC    THCRPID,THCRPID     CLEAR LOCAL CORP ID                          
         DROP  R4                                                               
*                                                                               
         XC    CASTSOR,CASTSOR                                                  
         MVI   ELCODE,TAWHELQ      GET WITHHOLDING ELEMENT                      
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         B     *+8                                                              
GETC2A   BAS   RE,NEXTEL                                                        
         BNE   GETC2B                                                           
         USING TAWHD,R4                                                         
         CLC   TAWHEMP,TGEMP       MATCH ON EMPLOYER                            
         BNE   GETC2A                                                           
         CLC   TAWHUNIT,=C'FD '    SKIP IF FEDERAL                              
         BE    GETC2A                                                           
         CLI   TAWHUNIT+2,C' '     FIND STATE OF RESIDENCE                      
         BNE   GETC2A                                                           
         MVC   CASTSOR,TAWHUNIT    STATE OF RESIDENCE                           
         DROP  R4                                                               
*                                                                               
GETC2B   GOTO1 CHAROUT,DMCB,TANUELQ,0,TANUTGST  IF GST# ON W4                   
         BNE   *+8                                                              
         OI    THCASTAT,THCASGST   SET STATUS BIT                               
*                                                                               
         MVI   ELCODE,TATIELQ      SET TO LOOK UP CORP ID ELEMENT               
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   GETC3                                                            
         USING TATID,R4                                                         
         OC    TATIID,TATIID       SEE CORP ID NUMBER                           
         BZ    GETC3                                                            
         OI    THCASTAT,THCASCRP                                                
*                                                                               
GETC3    CLI   CASTCORP,0          IF CORP CODE DEFINED                         
         BE    GETC6                                                            
         MVI   TCW4TYPE,TAW4TYCO   SET CORP W4 TYPE                             
*                                                                               
         MVI   ELCODE,TATIELQ      SET TO LOOK UP CORP ID ELEMENT               
         MVI   HALF,TATITYCO       SCAN FOR CORP ID ELEMENT TYPE                
         MVC   HALF+1(1),CASTCORP  AND FOR THIS CORP CODE                       
         GOTO1 GETL,DMCB,(2,HALF)                                               
         BNE   GETC6                                                            
         L     R4,TGELEM           R4=A(TAX ID EL. FOR CORP)                    
         USING TATID,R4                                                         
         MVC   THCRPID,TATIID      SET CORP ID NUMBER IN LOCAL W/S              
*                                                                               
         MVC   TGSSN,TATIID        SET ALSO IN GLOBAL FOR READ                  
         GOTO1 NEWALL,DMCB,('TLW4CDQ',THCRPNM),0,0,=C'CORP'  GET DTLS           
         BNE   NO                                                               
*                                                                               
GETC6    CLI   TCW4TYPE,TAW4TYCO   IF THIS IS CORPORATION                       
         BE    GETC7                                                            
         CLI   TCW4TYPE,TAW4TYCA   OR CANADIAN                                  
         BE    GETC7                                                            
         CLI   TCW4TYPE,TAW4TYFO   OR FOREIGNER                                 
         BNE   *+16                                                             
GETC7    MVC   CASTPAYC,CASTPAYI   PAYMENT AMOUNT S/B FOR CORP                  
         XC    CASTPAYI,CASTPAYI                                                
*                                                                               
         BRAS  RE,SETGUA           SET GUARANTEE VARIABLES                      
*                                                                               
         TM    THSTAT,THDSK2ND                                                  
         BO    GETC8                                                            
         OC    CASTINR,CASTINR     SKIP IF HAVE I&R FOR CANADIAN CAUSE          
*                                  TAPPGUAR GIVES ERROR IF HAVE MDED            
         BNZ   GETC8                                                            
         OC    CASTGUA,CASTGUA     IF GUARANTEE CODE ASSIGNED                   
         BZ    GETC8                                                            
         TM    CASTSTAT,CASTNAGU   AND USE IS APPLYING TO GUARANTEE             
         BO    GETC8                                                            
         BAS   RE,APPLY            APPLY GUARANTEE                              
*                                                                               
GETC8    BRAS  RE,TNHCALC          CALCULATE TAX AND HANDLING                   
         B     YES                                                              
*              ROUTINE APPLIES GUARANTEE AGAINST PAYMENT                        
*                                                                               
         USING CASTD,R2            R2=A(CAST TABLE ENTRY)                       
APPLY    NTR1                                                                   
         L     R3,AIO2             R3=A(BUILD DUMMY CHECK RECORD)               
         ST    R3,AIO                                                           
         USING TLCKD,R3                                                         
         XC    TLCKKEY(60),TLCKKEY                                              
         MVC   TLCKLEN,DATADISP                                                 
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT          BUILD DUMMY CAST DETAILS EL.                 
         USING TACAD,R4                                                         
         MVI   TACAEL,TACAELQ                                                   
         MVI   TACALEN,TACALNQ                                                  
         MVC   TACAGUA,CASTGUA     SET GUARANTEE CODE                           
         MVC   TACACORP,CASTCORP   AND CORP CODE                                
         GOTO1 ADDELEM                                                          
*                                                                               
         XC    ELEMENT,ELEMENT     BUILD DUMMY PAYMENT DETAILS EL.              
         USING TAPDD,R4                                                         
         MVI   TAPDEL,TAPDELQ                                                   
         MVI   TAPDLEN,TAPDLNQ                                                  
         MVC   TAPDUSE,TGUSCDE     USE CODE                                     
         MVC   TAPDTYPE,TGUSTYP    AND TYPE                                     
         MVC   TAPDCYCS(6),TCPCYCS CYCLE DATES                                  
         MVC   TAPDPAYI,CASTPAYI   PAYMENT AMOUNTS                              
         MVC   TAPDPAYC,CASTPAYC                                                
         MVC   TAPDSPNH,CASTSPNH   SUBJ. TO P&H                                 
         MVC   TAPDPNH,CASTPNH     P&H                                          
         MVC   TAPDW4TY,TCW4TYPE   W4 TYPE                                      
         GOTO1 ADDELEM                                                          
*                                                                               
         OC    THCRPID,THCRPID     IF WE HAVE CORP ID                           
         BZ    *+10                                                             
         MVC   TGSSN,CASTSSN       SET CAST SSN IN GLOBAL FOR GUAR.             
*                                                                               
         MVI   BYTE,X'84'          SET DRAFT MODE/CMNT IN CHECK REC.            
         TM    THOPTS,THTRACE                                                   
         BZ    *+8                                                              
         OI    BYTE,X'40'          SET TRACE MODE                               
*                                                                               
         GOTO1 =V(TAPPGUAR),DMCB,(RC),(BYTE,(R3)),(X'80',0),SYSCOMM             
*                                                                               
         CLI   0(R1),X'FE'         TEST CHANGES MADE                            
         BNE   APPLX                                                            
         LR    R4,R3                                                            
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CASTPAYI,TAPDPAYI   RESET PAYMENT AMOUNTS IN TABLE               
         MVC   CASTPAYC,TAPDPAYC                                                
         MVC   CASTPNH,TAPDPNH     P&H MAY ALSO HAVE CHANGED                    
         MVC   CASTAPPL,TAPDGUAR   SAVE GUARANTEE CREDITS                       
*                                                                               
APPLX    OC    THCRPID,THCRPID     IF WE HAVE CORP ID                           
         BZ    *+10                                                             
         MVC   TGSSN,THCRPID       RESTORE IT TO GLOBAL SSN                     
*                                                                               
         MVC   AIO,AIO1            RESTORE DEFAULT I/O AREA                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE HANDLES PAGE CONTINUATION BREAKS                         
*                                                                               
CONTINUE NTR1                                                                   
         LA    R0,LQDTLTOT         SKIP TO DETAIL TOTAL LINE                    
         BAS   RE,SKIPLINE         (NEED TO DO 1ST DUE TO BOXES BUG)            
         LA    R0,LQTOTAL          SKIP TO GRAND TOTAL LINE                     
         BAS   RE,SKIPLINE                                                      
*                                                                               
         LA    R3,P                R3=A(PRINT LINE)                             
         USING LINED,R3                                                         
         MVC   LINPAYC(13),=C'* CONTINUED *'                                    
         BAS   RE,PRNTIT                                                        
*                                                                               
         MVI   FORCEHED,C'Y'       SET TO START ON NEW PAGE                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PRINTS CAST DETAIL LINE                                  
*                                                                               
         USING CASTD,R2            R2=A(NEXT ENTRY)                             
DETAIL   NTR1                                                                   
         LA    R3,P                R3=A(PRINT LINE)                             
         USING LINED,R3                                                         
         MVC   LINSSN,CASTSSN      S/S NUMBER                                   
         CLI   TGBTYPE,TABRTYE     BILLING TYPE E (EMS)                         
         BE    DTL1                                                             
         XC    LINSSN,LINSSN                                                    
         GOTO1 SSNPACK,DMCB,CASTSSN,LINSSN  CONVERT SSN TO PID                  
         OC    LINSSN,SPACES                                                    
DTL1     MVC   LINNAME,THSSNNM     NAME                                         
*                                                                               
         OC    THCRPID,THCRPID     IF CORP. CODE DEFINED                        
         BZ    DTL2                                                             
         MVC   LINCRPID,THCRPID    DISPLAY CORP ID NUMBER                       
         CLI   TGBTYPE,TABRTYE     BILLING TYPE E (EMS)                         
         BE    DTL1A                                                            
         XC    LINCRPID,LINCRPID                                                
         GOTO1 SSNPACK,DMCB,THCRPID,LINCRPID  CONVERT CORP SSN TO PID           
         OC    LINCRPID,SPACES                                                  
DTL1A    MVC   LINCRPNM,THCRPNM    AND NAME                                     
         MVC   LINCRPLT,=C'CRP'    INDICATE CORPORATION UNDER OV FIELD          
*                                                                               
DTL2     LA    RF,TGDUB                                                         
         TM    CASTOV1,X'80'       IS THIS A PERCENT SCALE?                     
         BNO   DTL3                                                             
         NI    CASTOV1,X'FF'-X'80' YES                                          
         MVI   0(RF),C'%'                                                       
         LA    RF,1(RF)                                                         
         EDIT  (4,CASTOV1),(5,(RF)),2                                           
         B     DTL3A                                                            
DTL3     EDIT  (4,CASTOV1),(6,(RF)),2                                           
DTL3A    MVC   LINOV1,TGDUB        1ST OVERSCALE RATE                           
*                                                                               
         EDIT  (4,CASTOV2),(6,TGDUB),2                                          
         MVC   LINOV2,TGDUB        2ND OVERSCALE RATE                           
*                                                                               
         MVC   LINCAT,CASTCAT      CATEGORY                                     
         MVC   LINCAM,CASTCAM      CAMERA                                       
*                                                                               
         OC    CASTAGT,CASTAGT     AGENT                                        
         BZ    DTL4                                                             
         GOTO1 TRNSAGT,DMCB,(X'40',CASTAGT),LINAGT                              
*                                                                               
DTL4     MVC   LINSOW,CASTSOW      STATE CODE                                   
         MVC   LINLIFT,CASTLIFT    LIFT STATUS                                  
         MVC   LINDBL,CASTDBL      N'DOUBLES                                    
*                                                                               
*        OC    CASTFRST,CASTFRST   FIRST SERVICE DATE                           
*        BZ    DTL6                                                             
*        GOTO1 DATCON,DMCB,(1,CASTFRST),(8,LINFRST)                             
*                                                                               
         OC    CASTEXP,CASTEXP     EXPIRATION DATE                              
         BZ    DTL6                                                             
         GOTO1 DATCON,DMCB,(1,CASTEXP),(8,LINEXP)                               
*                                                                               
DTL6     MVC   LINUNI,CASTUNI      UNION                                        
         MVC   LINYR,CASTYR        YEAR                                         
         MVC   LINGUA,CASTGUA      GUARANTEE CODE                               
         L     R1,CASTINR                                                       
         A     R1,THINR            ADD I&R TO TOTAL I&R                         
         ST    R1,THINR                                                         
         L     R1,CASTINR                                                       
         A     R1,CASTPNH          ADD I&R TO P&H                               
         ST    R1,CASTPNH                                                       
         L     R1,CASTGSTU                                                      
         A     R1,THGSTU           ADD GST TO TOTAL GST                         
         ST    R1,CASTGSTU                                                      
*                                                                               
         LA    RE,CASTAMTS         RE=A(ACCUM IN TABLE)                         
         LA    RF,LINAMTS          RF=A(AREA ON PRINT LINE)                     
         LA    R1,NCASTAMT         R1=N'ACCUMS                                  
         LA    R4,THTOTS           R4=A(TOTALS)                                 
DTL8     EDIT  (4,0(RE)),(12,(RF)),2,MINUS=YES,ZERO=BLANK  ACCUMS               
         A     R0,0(R4)            ADD TO TOTALS                                
         ST    R0,0(R4)                                                         
         LA    RE,4(RE)            BUMP TO NEXTS                                
         LA    RF,12+1(RF)                                                      
         LA    R4,4(R4)                                                         
         BCT   R1,DTL8                                                          
*                                                                               
         TM    THSTAT2,THEMS       IF HFEE, EMS, OVERNIGHT                      
         BNO   DTL10                                                            
         GOTOR BLDPERF             PUT PERFORMER DETAILS TO DATASET             
         GOTOR PUTTAPE             PUT RECORD TO EMS TAPE                       
         MVC   P,SPACES            THEN CLEAR PRINT LINE                        
         MVC   P2,SPACES                                                        
         B     DTL30                                                            
         SPACE 1                                                                
DTL10    BRAS  RE,WEBCHK                                                        
         BNE   DTL20               IF SENDING DATA TO WEB FILE                  
         GOTOR BLDPERF             PUT PERFORMER DETAILS TO DATASET             
         SPACE 1                                                                
DTL20    BAS   RE,PRNTIT           PRINT PERFORMER DETAILS LINE                 
         SPACE 1                                                                
         BRAS  RE,PUTMQP           PUT PERFORMER DETAILS TO WEB FILE            
         SPACE 1                                                                
         USING TLDRD,RE                                                         
DTL30    LA    RE,KEY              REREAD CAST RECORD                           
         MVC   TLDRDA,CASTDA                                                    
         GOTO1 GETREC              IF GENERATING WEB FILE                       
         BRAS  RE,PUTCVER          PUT VERSIONS TO FILE                         
         BRAS  RE,PUTCSUB          PUT SUBSIDIARY COMMERCIALS TO FILE           
         DROP  RE                                                               
         SPACE 1                                                                
         OI    THSTAT,THPRNTED     PUT PERFORMER DETAILS TO WEB FILE            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PRINTS NOTICE TOTALS                                     
*                                                                               
TOTALS   NTR1                                                                   
         CLI   TGBTYPE,TABRTY20                                                 
         BE    *+12                                                             
         TM    THAGYST,THASCOMM    IF WANT AGENCY COMMISSION ON TOTALS          
         BZ    *+8                                                              
         BRAS  RE,COMMCALC         CALCULATE COMMISSION                         
*                                                                               
         BRAS  RE,SFEECALC         CALCULATE SIGNATORY FEE                      
*                                                                               
         LA    R3,P                R3=A(PRINT LINE)                             
         USING LINED,R3                                                         
         LA    R0,LQDTLTOT         R0=LINE NUMBER OF DETAIL TOTALS              
         BAS   RE,SKIPLINE         SKIP TO THAT LINE                            
*                                                                               
         MVC   LINLIT(L'LTTOTALS),LTTOTALS                                      
         LA    RE,THPAYI           RE=A(ACCUM IN TABLE)                         
         LA    RF,LINPAYI          RF=A(AREA ON PRINT LINE)                     
         LA    R1,NCASTAMT-1       R1=N'ACCUMS-1                                
TOT4     EDIT  (4,0(RE)),(12,(RF)),2,MINUS=YES                                  
         LA    RE,4(RE)            BUMP TO NEXTS                                
         LA    RF,12+1(RF)                                                      
         BCT   R1,TOT4                                                          
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT           PRINT DETAIL TOTALS                          
*                                                                               
         ZAP   TGDUB,=P'0'         CLEAR GRAND TOTAL                            
*                                                                               
         OC    TGCTNAME,TGCTNAME                                                
         BZ    TOT5                                                             
         MVC   LINSSN(L'LTCOTYDE),LTCOTYDE                                      
         MVC   LINSSN+L'LTCOTYDE+3(7),TGCTNAME                                  
         CLC   TGCTNAME(7),=CL7'SEASON '        CHANGE TO SEASONAL              
         BNE   *+10                                                             
         MVC   LINSSN+L'LTCOTYDE+3(8),=C'SEASONAL'                              
         CLC   TGCTNAME(7),=CL7'ADDENDM'        CHANGE TO ADDENDUM              
         BNE   *+10                                                             
         MVC   LINSSN+L'LTCOTYDE+3(8),=C'ADDENDUM'                              
         BRAS  RE,PUTMSG                                                        
*                                                                               
TOT5     CLI   TGBTYPE,TABRTYE     IF BILLING TYPE E,                           
         BNE   TOT5A                                                            
         GOTOR CALCEMS             CALCULATE EMS RATES                          
         B     TOT20                                                            
*                                                                               
TOT5A    MVI   MSGSTAT,0                                                        
         MVI   MSGSTAT2,0                                                       
*                                                                               
         MVC   BC10(2),=C'HF'      PRINT OUT UNIQUE HOLDING FEE ID              
         GOTO1 HEXOUT,DMCB,TGCOM,BC10+2,8,0                                     
         GOTO1 DATCON,DMCB,(1,TCPCYCS),(20,BC10+10)                             
*                                                                               
         L     R2,THPAYI           ADD PAYMENT TOTALS TOGETHER                  
         A     R2,THPAYC           AND I&R                                      
         A     R2,THINR                                                         
         MVC   LINLIT(L'LTWAGES),LTWAGES                                        
         BAS   RE,EDITFORC                                                      
*                                                                               
         CLI   TGBTYPE,TABRTY11    TYPE 11 HAS EVERYTHING IN HANDLING           
         BE    TOT8                                                             
         L     R2,THTAXES          TAX AMOUNT                                   
         CLI   TGBTYPE,TABRTY23    TYPE 23, SEPARATE GST                        
         BE    *+8                                                              
         A     R2,THGSTU           PLUS US DOLLAR GST FOR ACTRA                 
         MVC   LINLIT(L'LTTAX),LTTAX                                            
         CLI   TGBTYPE,TABRTY9                                                  
         BE    *+12                                                             
         CLI   TGBTYPE,TABRTY13                                                 
         BNE   TOT6                                                             
         A     R2,THHNDC           TYPES 9,13 ADD IN CORP HANDLING              
         MVC   LINLIT(L'LTTNH),LTTNH          AND CALL IT TAX/HANDLING          
         B     TOT7                                                             
TOT6     CLI   TGBTYPE,TABRTY14                                                 
         BE    TOT7                                                             
         CLI   TGBTYPE,TABRTY15    TYPE 15, ADD IN INDIV HAND                   
         BE    TOT7                                                             
         CLI   TGBTYPE,TABRTY10                                                 
*        BE    TOT7                                                             
*        CLI   TGBTYPE,TABRTY24                                                 
         BNE   *+8                                                              
TOT7     A     R2,THHNDI           TYPES 9,10,13,14 ADD IN INDIV HAND           
         BRAS  RE,TYPE20                                                        
         BAS   RE,EDITTOT                                                       
*                                                                               
TOT8     CLI   TGBTYPE,TABRTY23    TYPE 23, SEPARATE GST                        
         BNE   TOT8A                                                            
         L     R2,THGSTU                                                        
         MVC   LINLIT(L'LTGST2),LTGST2                                          
         BAS   RE,EDITTOT                                                       
*                                                                               
TOT8A    L     R2,THPNH            P&H AMOUNT                                   
         S     R2,THINR            SUBTRACT I&R (ADDED TO WAGES)                
         MVC   LINLIT(L'LTPNH),LTPNH                                            
         BAS   RE,EDITTOT                                                       
*                                                                               
         L     R2,THFICR           FICA CREDITS (S/B FOR TYPE 1 ONLY)           
         MVC   LINLIT(L'LTFICR),LTFICR                                          
         BAS   RE,EDITTOT                                                       
*                                                                               
         CLI   TGBTYPE,TABRTY6     FOR TYPES 6,8                                
         BE    TOT9                                                             
         CLI   TGBTYPE,TABRTY8                                                  
         BE    TOT9                                                             
         CLI   TGBTYPE,TABRTY21    OR TYPE 21                                   
         BNE   TOT10                                                            
TOT9     CVB   R2,TGDUB            SUB-TOTAL                                    
         MVC   LINLIT(L'LTSTOT),LTSTOT                                          
         BAS   RE,EDITFORC                                                      
*                                                                               
TOT10    CLI   TGBTYPE,TABRTY9     FOR TYPES 9,13 HAD EVERYTHING IN TAX         
         BE    TOT12                                                            
         CLI   TGBTYPE,TABRTY13                                                 
         BE    TOT12                                                            
         CLI   TGBTYPE,TABRTY20                                                 
         BE    TOT12                                                            
         L     R2,THHNDC           HANDLING AMOUNT                              
         MVC   LINLIT(L'LTHND),LTHND                                            
         MVC   TGFULL,LTHND                                                     
         CLI   TGBTYPE,TABRTY14                                                 
         BE    TOT10X                                                           
         CLI   TGBTYPE,TABRTY15                                                 
         BE    TOT10X                                                           
         CLI   TGBTYPE,TABRTY10                                                 
         BE    TOT10X                                                           
*        CLI   TGBTYPE,TABRTY24                                                 
*        BE    TOT10X                                                           
         A     R2,THHNDI           TYPE 10,14,15 HAD INDIV. HAND IN TAX         
TOT10X   CLI   TGBTYPE,TABRTY11                                                 
         BNE   TOT11                                                            
         A     R2,THTAXES          TYPE 11 HAS EVERYTHING IN HANDLING           
         A     R2,THGSTU                                                        
         MVC   LINLIT(L'LTTP),LTTP         AND CALLS IT TALENT PARTNERS         
         MVC   TGFULL,LTTP                                                      
TOT11    BAS   RE,EDITTOT                                                       
*                                                                               
TOT12    CLI   TGBTYPE,TABRTY20                                                 
         BE    TOT14                                                            
         L     R2,THCOMM           AGENCY COMMISSION (SEE AGY STATUS)           
         MVC   LINLIT(L'LTCOMM),LTCOMM                                          
         BAS   RE,EDITTOT                                                       
*                                                                               
TOT14    L     R2,THSIGN           SIGNATORY FEE                                
         MVC   LINLIT(L'LTSGNFEE),LTSGNFEE                                      
         BAS   RE,EDITTOT                                                       
*                                                                               
TOT20    TM    THHLDPEB,TAAYHSEL   IF AGENCY/CLIENT IS ELECTRONIC ONLY          
         BZ    TOT40                                                            
TOT30    MVC   P,SPACES                                                         
         BRAS  RE,PRTMSG           PRINT OUT ALL REMAINING MESSAGES             
         BNE   TOT80                                                            
         B     TOT30                                                            
*                                                                               
TOT40    LA    R2,LQTOTAL          IF AGENCY/CLIENT IS NOT ELEC. ONLY           
TOT50    ZIC   RE,LINE                                                          
         SR    R2,RE                                                            
         BNP   TOT70                                                            
TOT60    BRAS  RE,PRTMSG           PRINT OUT ALL REMAINING MESSAGES             
         BNE   TOT70                                                            
         BRAS  RE,MYSPOOL                                                       
         B     TOT60                                                            
TOT70    LA    R0,LQTOTAL          THEN SKIP TO GRAND TOTAL LINE                
         BAS   RE,SKIPLINE                                                      
*                                                                               
TOT80    CVB   R2,TGDUB            GRAND TOTAL                                  
         MVC   LINLIT(L'LTGRAND),LTGRAND                                        
         BAS   RE,EDITFORC                                                      
*                                                                               
         BRAS  RE,PUTTOTS          PUT TOTALS RECORD TO WEB FILE                
*                                                                               
         NI    THSTAT3,X'FF'-THELHDRP                                           
TOTX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              TOTAL PRINTING SUBSIDIARY ROUTINES                               
         SPACE 2                                                                
EDITTOT  DS    0H                  EDIT/PRINT WITH SUPPRESSING ZERO             
         LTR   R2,R2               IF NOTHING TO PRINT                          
         BNZ   EDITFORC                                                         
         MVC   LINLIT,SPACES       CLEAR LITERAL                                
         BR    RE                  AND RETURN                                   
         SPACE 3                                                                
EDITFORC NTR1  ,                   EDIT/PRINT ALWAYS                            
         BRAS  RE,PRTMSG           PRINT OUT SEASONAL MESSAGES                  
*                                                                               
         EDIT  (R2),(12,LINPAYC),2,MINUS=YES,ZERO=NOBLANK                       
         MVC   0(L'RECWMI,R4),LINPAYC                                           
*                                                                               
         CLC   LINLIT(L'LTSTOT),LTSTOT    IF THIS ISN'T SUB-TOTAL               
         BE    EDITF5                                                           
         CLC   LINLIT(L'LTGRAND),LTGRAND  OR GRAND TOTAL                        
         BE    EDITF3                                                           
         AP    TGDUB,DUB                  ADD TO GRAND TOTAL                    
         B     EDITF5                                                           
*                                                                               
EDITF3   AP    THAGYTOT,TGDUB      ADD GRAND TOTAL TO AGY/CLI TOTAL             
         NI    THSTAT3,X'FF'-THELHDRP                                           
*                                                                               
EDITF5   BRAS  RE,PUTBRK           PUT BREAKDOWN REC TO WEB FILE                
         BAS   RE,PRNTIT           PRINT THE LINE                               
         B     XIT                                                              
         EJECT                                                                  
*              PRINT HOLDING FEE SUMMARY LINE                                   
*                                                                               
PRSUM    NTR1                                                                   
         L     R2,=A(CASTTAB)      R2=A(CAST TABLE)                             
         USING CASTD,R2                                                         
         BAS   RE,CNTPAGES         CHECK IF ANYONE REALLY QUALIFIED             
         BNE   PRSX                                                             
*                                                                               
         USING SORTD,R3                                                         
         CLI   RECNUM,HD           SKIP IF HFDISK                               
         BE    PRS5                                                             
         LA    R3,THLSTSRT         R3=A(LAST SORT REC) IS BEING PRINTED         
         MVI   RCSUBPRG,1          INIT SPROG FOR SPECS OF SUMMARIES            
*                                  (MUST DO HERE IN CASE FIRST PAGE)            
         CLI   SORTSTCG,0          IF CGROUP COKE                               
         BNE   PRS1                                                             
         MVI   RCSUBPRG,3          SET IT TO 3                                  
         CLI   RECNUM,SS           AND IF SSUMMARY                              
         BNE   PRS2                                                             
         MVI   RCSUBPRG,5          SET IT TO 5                                  
         B     PRS2                                                             
PRS1     CLI   RECNUM,SS           IF SSUMMARY                                  
         BNE   PRS5                                                             
         MVI   RCSUBPRG,4          SET IT TO 4                                  
         B     PRS5                                                             
         SPACE                                                                  
         USING BOXD,R4             SET UP BOXES FOR CGROUP COKE                 
PRS2     CLI   TGOFF,X'FF'         IF NOT FIRST TIME                            
         BE    PRS3                                                             
         CLI   FORCEHED,C'Y'       AND NOT FIRST LINE ON PAGE                   
         BE    PRS3                                                             
*        CLI   ACTEQU,ACTDOWN                                                   
*        BE    PRS3                                                             
         L     R4,ABOX             R4=A(BOX AREA)                               
         MVI   BOXREQ,C'D'         PRINT DOTTED LINE AFTER EACH LINE            
         OI    THSTAT,THSUMPRT     SET ACTUALLY PRINTING A SUMMARY LINE         
         BAS   RE,PRNTIT                                                        
         XI    THSTAT,THSUMPRT     TURN OFF BIT AFTER PRINTING                  
PRS3     LA    R3,P-8              START PRINTING 8 SPACES TO LEFT              
         B     *+8                                                              
*                                                                               
PRS5     LA    R3,P                R3=A(PRINT LINE)                             
         USING HSLINED,R3                                                       
         MVC   HSCID,TGCID         COMMERCIAL ID                                
         MVC   HSCOTITL,THCOMNM    COMMERCIAL TITLE                             
         MVC   HSFFC,THCOFCYC      FIRST FIXED CYCLE                            
*                                  NEXT CYCLE START                             
         GOTO1 DATCON,DMCB,(1,TCPCYCS),(8,HSDDTE)                               
         MVC   HSEXP,THCOEXP       EXPIRATION DATE                              
         CLI   RECNUM,HD           DON'T PRINT TOTAL FOR HF DISK                
         BE    PRS10                                                            
         CVB   R2,TGDUB            HOLDING FEE TOTAL                            
         EDIT  (R2),(12,HSTOT),2,MINUS=YES,ZERO=NOBLANK                         
PRS10    OI    THSTAT,THSUMPRT     SET ACTUALLY PRINTING A SUMMARY LINE         
         BAS   RE,PRNTIT                                                        
         XI    THSTAT,THSUMPRT     TURN OFF BIT AFTER PRINTING                  
         AP    THFNO,=P'1'         INCREMENT COUNTER                            
*                                                                               
PRSX     CLC   THSRTREC(SORTPG),THLSTSRT  IF AGY/CLIENT CHANGED                 
         BE    PRSX5                                                            
         BAS   RE,PRAGYTOT         PRINT AGY/CLI HOLDING FEE TOTAL              
         MVI   FORCEHED,C'Y'       START ON NEW PAGE                            
         B     PRSX10                                                           
*                                                                               
         USING SORTD,R3                                                         
PRSX5    LA    R3,THLSTSRT         R3=A(LAST SORT REC) IS BEING PRINTED         
         CLI   SORTSTCG,0                  ELSE IF CGROUP COKE                  
         BNE   PRSX10                                                           
         CLC   THSRTREC(SORTPG2),THLSTSRT  TEST IF PRODUCT CHANGED              
         BE    PRSX10                                                           
         BAS   RE,PRAGYTOT         PRINT AGY/CLI/PRD HFEE TOTAL                 
         MVI   FORCEHED,C'Y'       START ON NEW PAGE                            
         DROP  R3                                                               
*                                                                               
PRSX10   L     RE,=A(CASTTAB)      RE-INITIALIZE CAST TABLE                     
         L     RF,=AL4(NCAST*CASTLNQ)                                           
         XCEF                                                                   
         B     XIT                                                              
         EJECT                                                                  
*              PRINT AGENCY/CLIENT HOLDING FEE TOTAL                            
*                                                                               
PRAGYTOT NTR1                                                                   
         CLI   RECNUM,HD           ONLY PRINT TOTAL FOR SUMMARY                 
         BE    XIT                 AND 2ND SUMMARY                              
         CLC   THAGYTOT,=D'0'      DON'T PRINT THE FIRST TIME                   
         BE    XIT                                                              
*                                                                               
         OI    THSTAT,THSUMPRT     SET ACTUALLY PRINTING A SUMMARY LINE         
         USING BOXD,R4                                                          
         L     R4,ABOX             R4=A(BOX AREA)                               
         MVI   BOXREQ,C'D'         PRINT DOTTED LINE BEFORE TOTAL LINE          
         BAS   RE,PRNTIT                                                        
*                                                                               
         USING SORTD,R1                                                         
         LA    R1,THLSTSRT         R3=A(LAST SORT REC) IS BEING PRINTED         
         LA    R3,P                R3=A(PRINT LINE)                             
         CLI   SORTSTCG,0          IF CGROUP COKE                               
         BNE   *+8                                                              
         LA    R3,P-8              START PRINTING 8 SPACES TO THE LEFT          
         USING HSLINED,R3                                                       
         CVB   R2,THAGYTOT                                                      
         EDIT  (R2),(12,HSTOT),2,MINUS=YES,ZERO=NOBLANK                         
         DROP  R1,R3                                                            
         BAS   RE,PRNTIT                                                        
         XI    THSTAT,THSUMPRT     TURN OFF BIT AFTER PRINTING                  
         XC    THAGYTOT,THAGYTOT   DON'T PRINT THIS AGAIN                       
         B     XIT                                                              
         EJECT                                                                  
*              PRINT A LINE                                                     
*                                                                               
PRNTIT   NTR1                                                                   
         TM    THSTAT,THSUMM       HLD FEE SUMMARY OR DISK?                     
         BNO   PRNT2                                                            
         TM    THSTAT,THSUMPRT     TEST ACTUALLY PRINTNG A HF SUMM LINE         
         BO    PRNT1                                                            
         MVC   P1,SPACES           IF NOT, CLEAR PRINT LINES                    
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         MVC   P4,SPACES                                                        
         B     PRNTX                                                            
         SPACE                                                                  
PRNT1    CLI   RECNUM,HD           IF HLD FEE DISK                              
         BNE   PRNT2                                                            
         BRAS  RE,PUTDISK          PUT A RECORD TO DISK                         
         SPACE                                                                  
PRNT2    BRAS  RE,MYSPOOL          PRINT IT                                     
*                                                                               
PRNT3    TM    THSTAT,THHOOKED     IF WE JUST DID HEADLINES                     
         BZ    PRNTX                                                            
         XI    THSTAT,THHOOKED     TURN OFF BIT                                 
*                                                                               
         BAS   RE,CHECKOFF         PRINT CHECK-OFF BOXES                        
*                                                                               
         MVC   P,THP               RESTORE SAVED PRINT LINES                    
         MVC   P2,THP2                                                          
         MVI   FORCEMID,C'Y'       SET TO FORCE MIDLINES                        
         NI    SPOOLIND,X'FF'-SPNSPACE  TURN OFF LINE SKIP SUPPRESS             
         B     PRNT2               GO BACK AND PRINT SAVED LINE                 
*                                                                               
PRNTX    MVI   SPACING,1           RESET SPACING BACK TO 1                      
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE BUMPS TO LINE AT R0                                      
*                                                                               
SKIPLINE NTR1                                                                   
         ZIC   R1,LINE             R1=CURRENT LINE                              
         SR    R0,R1               R0=N'LINES TO SKIP                           
         BNP   XIT                 GET OUT IF ALREADY THERE                     
         STC   R0,SPACING          ELSE SET SPACING                             
         BAS   RE,PRNTIT           AND SKIP TO REQUESTED LINE NUMBER            
         B     XIT                                                              
         EJECT                                                                  
PUTFOOTR NTR1                                                                   
         LA    R3,THDSKREC         R3=A(DISK REC)                               
         USING DISKD,R3                                                         
         MVI   0(R3),C' '          MOVE SPACES TO DISK                          
         MVC   1(DISKLNQ-1,R3),0(R3)                                            
         MVC   DSKTYPE,=C'09'      SET TRAILER TYPE                             
         AP    THDSKCNT,=P'1'      INCREMENT COUNT                              
         EDIT  THDSKCNT,(7,DSKCOUNT),ALIGN=LEFT                                 
         BRAS  RE,HEADFOOT         ADD SOME INFO AND PUT IT TO DISK             
         L     R2,=A(HDDISK)                                                    
         CLI   ACTEQU,ACTDOWN                                                   
         BNE   *+8                                                              
         L     R2,=A(TADOWN)                                                    
         CLOSE ((R2))                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT CHECK-OFF BOXES                                 
*                                                                               
CHECKOFF NTR1                                                                   
         BAS   RE,PRNTIT                                                        
         MVC   P+(132-L'CHKLIT1)/2(L'CHKLIT1),CHKLIT1  CENTER 1ST LIT.          
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
*                                                                               
         LA    RF,CHKLITS          RF=A(1ST LITERAL)                            
         MVC   P1,0*132(RF)        MOVE OTHERS DIRECTLY TO P-LINES              
         MVC   P2,1*132(RF)                                                     
         MVC   P3,2*132(RF)                                                     
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
*                                                                               
         MVC   P1,3*132(RF)                                                     
         MVC   P2,4*132(RF)                                                     
         MVC   P3,5*132(RF)                                                     
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES (HEADHOOK)                                     
*                                                                               
HDHOOK   NTR1                                                                   
         GOTOR MYHDHK                                                           
         B     XIT                                                              
*                                                                               
MDHOOK   NTR1                                                                   
         GOTOR MYMDHK                                                           
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 2                                                                
YES      SR    RC,RC               SET CONDITION CODE                           
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
HEXFFS   DC    6X'FF'                                                           
*                                                                               
LQDTLS   EQU   24                  N'LINES TO TOP OF DETAIL HEADS BOX           
LQDTLS2  EQU   LQDTLS+2            N'LINES TO BOTTOM OF DTL HEADS BOX           
NCASTPG  EQU   19                  N'CAST LINES PER PAGE                        
LQDTLTOT EQU   LQDTLS2+NCASTPG+3   LINE NO. OF DETAIL TOTALS                    
LQTOTAL  EQU   57                  LINE NO. OF GRAND TOTAL                      
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,93,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=135'                                   
*                                                                               
HDDISK   DCB   DDNAME=HDDISK,DSORG=PS,RECFM=FB,LRECL=98,               X        
               BLKSIZE=980,MACRF=PM                                             
*                                                                               
LTLID    DC    C'Lift Id'                                                       
LTLILEN  DC    C'Lift Length'                                                   
LTCYC    DC    C'Cycle Dates'                                                   
*                                                                               
LTTOTALS DC    C'Totals'                                                        
LTCOTYDE DC    C'Commercial Type'                                               
LTWAGES  DC    C'Wages/Misc/I&&R'                                               
LTTAX    DC    C'Payroll Taxes'                                                 
LTTNH    DC    C'Tax/Handling '                                                 
LTPNH    DC    C'P && H Contribution'                                           
LTFICR   DC    C'Fica Credits'                                                  
LTSGNFEE DC    C'Signatory Fee'                                                 
LTSTOT   DC    C'Sub Total'                                                     
LTHND    DC    C'Handling'                                                      
LTTP     DC    C'Talent Partners'                                               
LTCOMM   DC    C'Agency Commission'                                             
LTGRAND  DC    C'Holding Fee Total'                                             
*                                                                               
LTRECS   DC    C'HOLDING FEE NOTICES'                                           
*                                                                               
LTEMSWG  DC    C'Wages/Misc       '                                             
LTEMSF   DC    C'EMS Fee          '                                             
LTGST    DC    C'GST/HST'                                                       
*                                                                               
LTGST2   DC    C'GST/PST (HST)    '                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              LITERALS FOR CHECK-OFF BOXES                                     
*                                                                               
CHKLIT1  DC    C'Holding Fee Notification - This is NOT an Invoice'             
*                                                                               
CHKLITS  DS    0CL132                                                           
*                                                                               
         DC    CL2' ',X'ACBFBC'                                                 
         DC    CL28' Pay/Renew Holding Fee'                                     
         DC    CL2' ',X'ACBFBC'                                                 
         DC    CL30' Release Basic Commercial Id'                               
         DC    CL2' ',X'ACBFBC'                                                 
         DC    CL26' Release Commercial'                                        
         DC    CL2' ',X'ACBFBC'                                                 
         DC    CL28' Release Voice-over Perf(s)'                                
*                                                                               
         DC    CL2' ',X'ABBFBB'                                                 
         DC    CL28' '                                                          
         DC    CL2' ',X'ABBFBB'                                                 
         DC    CL30' Do Not Release Lift Id'                                    
         DC    CL2' ',X'ABBFBB'                                                 
         DC    CL26' Do Not Renew Holding Fee'                                  
         DC    CL2' ',X'ABBFBB'                                                 
         DC    CL28' Retain On-camera(s)'                                       
*                                                                               
         DC    CL33' '                                                          
         DC    CL35' '                                                          
         DC    CL31' '                                                          
         DC    CL33'      Pay/Renew Holding Fee'                                
*                                                                               
         DC    CL2' ',X'ACBFBC'                                                 
         DC    CL28' Do Not Pay / Please Recycle'                               
         DC    CL2' ',X'ACBFBC'                                                 
         DC    CL30' Release Lift Version Id'                                   
         DC    CL2' ',X'ACBFBC'                                                 
         DC    CL26' Release On-camera Perf(s)'                                 
         DC    CL33' '                                                          
*                                                                               
         DC    CL2' ',X'ABBFBB'                                                 
         DC    CL28' Covered by Dealer Use'                                     
         DC    CL2' ',X'ABBFBB'                                                 
         DC    CL30' Do Not Release Basic Comml Id'                             
         DC    CL2' ',X'ABBFBB'                                                 
         DC    CL26' Retain Voice-over(s),'                                     
         DC    32X'BF',C' '                                                     
*                                                                               
         DC    CL33' '                                                          
         DC    CL35' '                                                          
         DC    CL31'      Pay/Renew Holding Fee'                                
         DC    CL33'Authorized Signature       Date'                            
         EJECT                                                                  
*              SPECS FOR HEADLINE PRINTING                                      
*                                                                               
MYSPECS  DS    0H                                                               
         SPROG 0                                                                
         SSPEC H1,90,RUN                                                        
         SSPEC H1,120,C'Page    of'                                             
*                                                                               
         SSPEC H4,55,C'Holding Fee Notification'                                
         SSPEC H5,2,C'Notice for'                                               
         SSPEC H5,55,24X'BF'                                                    
*                                                                               
         SSPEC H9,2,C'Client'                                                   
         SSPEC H10,2,C'Product'                                                 
         SSPEC H11,2,C'Commercial'                                              
         SSPEC H12,2,C'Comml Id'                                                
         SSPEC H13,2,C'Film Date'                                               
         SSPEC H13,25,C'Film Studio'                                            
         SSPEC H13,54,C'Film City'                                              
         SSPEC H14,2,C'Record Date'                                             
         SSPEC H14,25,C'Record Studio'                                          
         SSPEC H14,54,C'Record City'                                            
*                                                                               
         SSPEC H8,48,C'Estimate #'                                              
         SSPEC H8,59,25X'BF'                                                    
         SSPEC H10,52,C'P.O. #'                                                 
         SSPEC H10,59,25X'BF'                                                   
*                                                                               
         SSPEC H3,90,C'Employer of Rec'                                         
         SSPEC H4,90,C'Internal Codes'                                          
         SSPEC H5,90,C'Staff ID'                                                
         SSPEC H6,90,C'1st Air Date'                                            
         SSPEC H7,90,C'1st Fixed Cycle'                                         
         SSPEC H8,90,C'Expiration Date'                                         
         SSPEC H9,90,C'Comml Length'                                            
         SSPEC H10,90,C'Media'                                                  
*                                                                               
*****    SSPEC M1,2,C'S/S Numb  Performer Name    Ov1 Ov2 Cat Cam Agnt'         
         SSPEC M1,2,C'PID       Performer Name    Ov1 Ov2 Cat Cam Agnt'         
         SSPEC M1,51,C'St  L D Expiry   Uni Yr  Guar Guar Applied'              
         SSPEC M1,94,CL37'Gross Wages  Misc Payment     P&&H/I&&R'              
*                                                                               
         SPROG 1,2,3,4,5                                                        
         SSPEC H1,2,RUN                                                         
         SSPEC H1,103,REPORT                                                    
         SSPEC H1,117,PAGE                                                      
         SSPEC H2,103,REQUESTOR                                                 
         SSPEC H4,2,C'Agency'                                                   
         SSPEC H5,2,C'Client'                                                   
*                                                                               
         SPROG 1,4                                                              
         SSPEC H8,27,C'Comm ID      Commercial Title'                           
         SSPEC H8,77,C'FF Cycle Due Date Exp Date    Total    '                 
*                                                                               
         SPROG 2                                                                
         SSPEC H8,27,C'Comm ID      Commercial Title'                           
         SSPEC H8,77,C'FF Cycle Due Date Exp Date'                              
*                                                                               
         SPROG 3,5                                                              
         SSPEC H6,2,C'Product'                                                  
         SSPEC H9,19,C'Comm Id      Commercial Title'                           
         SSPEC H9,69,C'FF Cycle Due Date Exp Date    Total    '                 
         SSPEC H9,109,C'Approve Release'                                        
*                                                                               
         SPROG 1,3                                                              
         SSPEC H1,55,C'Holding Fee Summary Report'                              
         SSPEC H2,55,26X'BF'                                                    
*                                                                               
         SPROG 4,5                                                              
         SSPEC H1,53,C'Holding Fee 2nd Notice Summary'                          
         SSPEC H2,53,30X'BF'                                                    
*                                                                               
         SPROG 2                                                                
         SSPEC H1,56,C'Holding Fee Disk Report'                                 
         SSPEC H2,56,23X'BF'                                                    
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE TAUNIOND                                                       
         EJECT                                                                  
*----------------------------------------------------------------------         
* THIS ROUTINE DETERMINES THE TAXABLE STATE.  USUALLY IT IS THE STATE           
* OF WORK.  IF THE SOW IS NOT DEFINED ON THE EMPLOYER RECORD AND IF THE         
* STATE OF RESIDENCE IS DEFINED ON THE EMPLOYER RECORD THEN THE SOR IS          
* THE TAXABLE STATE FOR PURPOSES OF DISABILITY, UNEMPLOYMENT, ETC.              
* IF NEITHER ARE DEFINED ON THE EMPLOYER RECORD THEN THE ROUTINE                
* ATTEMPTS TO ASSIGN THE DEFAULT TAXABLE STATE FROM THE EMPLOYER                
* RECORD.  THE STATE U.C.# FOR THE TAXABLE STATE IS SET ACCORDINGLY.            
*----------------------------------------------------------------------         
*                                                                               
         USING CASTD,R2                                                         
SETTAXBL NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   AIO,AIO2            SEARCH EMPLOYER RECORD FOR TAX ID            
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'A4',TGEMP)                                
         BNE   SETTXX                                                           
                                                                                
         MVC   TGFULL,CASTSOW      STATE OF WORK                                
         BAS   RE,GETSTATE         LOOK UP STATE OF WORK ON EMP RECORD          
         BE    SETTXX              IF FOUND - DONE                              
*                                                                               
         MVC   TGFULL,CASTSOR      STATE OF RESIDENCE                           
         BAS   RE,GETSTATE         LOOK UP STATE OF WORK ON EMP RECORD          
         BNE   SETT10              IF FOUND                                     
         MVC   TMUNIT,CASTSOR      SET TAXABLE STATE IS STATE OF RES.           
         B     SETTXX                                                           
*                                                                               
SETT10   L     R4,AIO2                                                          
         MVI   ELCODE,TAEDELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   SETTXX                                                           
         USING TAEDD,R4                                                         
         MVC   TMUNIT,TAEDDST       IF NOT USE DEFAULT STATE                    
         DROP  R4                                                               
*                                                                               
SETTXX   MVC   AIO,AIO1                                                         
         J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*        FIND STATE IN EMPLOYER RECORD                                          
*                      TGFULL - STATE                                           
*                                                                               
GETSTATE NTR1                                                                   
         MVI   ELCODE,TATIELQ      ELEMENT FOR STATE OF WORK                    
         MVI   FULL,TATITYUN                                                    
         MVC   FULL+1(3),TGFULL                                                 
         GOTO1 GETL,DMCB,(4,FULL)  FIRST TRY TO FIND UNEMPLOYMENT ID            
         JE    YES                                                              
*                                                                               
         MVI   FULL,TATITYTX       THEN TRY TO FIND TAX ID                      
         GOTO1 GETL,DMCB,(4,FULL)                                               
         JE    YES                 SET CC                                       
         J     NO                                                               
         EJECT                                                                  
*                                  TGDUB=HFEE TOTAL                             
PUTDISK  NTR1  BASE=*,LABEL=*                                                   
         CP    THDSKCNT,=P'0'      IF DISK NOT OPENED YET                       
         JH    PUTDSK5                                                          
         L     R2,=A(HDDISK)                                                    
         CLI   ACTEQU,ACTDOWN                                                   
         JNE   *+8                                                              
         L     R2,=A(TADOWN)                                                    
         OPEN  ((R2),OUTPUT)                                                    
         LTR   RF,RF                                                            
         JZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,PUTHDR           WRITE OUT HEADER RECORD                      
*                                                                               
PUTDSK5  LA    R3,THDSKREC         R3=A(DISK RECORD)                            
         USING DISKD,R3                                                         
         MVI   0(R3),C' '          MOVE SPACES TO DISK                          
         MVC   1(DISKLNQ-1,R3),0(R3)                                            
         MVC   DSKTYPE,=C'02'      TYPE WITH 1 HLD FEE                          
         TM    THOPTS,THSPLIT                                                   
         JZ    *+10                                                             
         MVC   DSKTYPE,=C'03'      OR TYPE WITH SPLIT HLD FEES                  
         MVC   DSKISCII,TGCID                                                   
         MVC   DSKTITLE,THCOMNM                                                 
         MVC   DSKSEC(3),THCOLEN                                                
         GOTO1 DATCON,DMCB,(1,TCPCYCS),(20,DSKHFDUE)  HLD FEE DUE DATE          
         MVC   DSKFFC,THFCYC20     FIRST FIXED CYCLE                            
         MVC   DSKEXP,THEXP20      EXP DATE                                     
         EDIT  (P8,TGDUB),(10,DSKNOGRT),2,ZERO=NOBLANK                          
         OC    THHFEE,THHFEE                                                    
         JNZ   *+10                                                             
         MVC   THHFEE,TGDUB                                                     
         EDIT  (P8,THHFEE),(10,DSKHFEE),2,ZERO=NOBLANK                          
*                                                                               
         L     R2,=A(HDDISK)                                                    
         CLI   ACTEQU,ACTDOWN                                                   
         JNE   *+8                                                              
         L     R2,=A(TADOWN)                                                    
         PUT   (R2),(R3)           PUT IT TO DISK                               
         AP    THDSKCNT,=P'1'      INCREMENT COUNT                              
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE ADDS A CAST MEMBER TO CAST TABLE                         
*---------------------------------------------------------------------          
         USING SORTD,R3            R3=A(SORT RECORD)                            
PUTCAST  NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TACAELQ      GET CAST DETAILS EL.                         
         L     R4,AIO2                                                          
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R4            R4=A(CAST DETAILS EL.)                       
*                                                                               
         L     R2,THANXTCS         R2=A(NEXT ENTRY)                             
         USING CASTD,R2                                                         
         CLI   0(R2),X'FF'         END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   CASTSSN,SORTSSN     S/S NUMBER                                   
         MVC   CASTSEQ,SORTSRT+4   INPUT SEQUENCE NUMBER                        
         MVC   CASTDA,SORTDA       DISK ADDRESS                                 
         MVC   CASTCORP,TACACORP   CORPORATION CODE                             
         MVC   CASTOV1,TCOV1       OVERSCALE RATE                               
         MVC   CASTOV2,TACAOV2     2ND OVERSCALE RATE                           
         MVC   CASTCAT,TGCAT       CATEGORY                                     
         MVC   CASTCAM,TACAONOF    CAMERA                                       
         MVC   CASTAGT,TACANCDE    AGENT                                        
         MVC   CASTSOW,TACAUNIT    STATE CODE                                   
         TM    TACASTAT,TACASTLF   TEST ON LIFT                                 
         BZ    *+8                                                              
         MVI   CASTLIFT,C'Y'                                                    
         TM    TACASTAT,TACASTLO   TEST ONLY ON LIFT                            
         BZ    *+8                                                              
         MVI   CASTLIFT,C'O'                                                    
         MVC   CASTDBL,TACADBL     N'DOUBLES                                    
         MVC   CASTFRST,TACAFRST   FIRST SERVICE DATE                           
         MVC   CASTUNI,TGUNCDE     UNION                                        
         MVC   CASTLCL,TACALOCL    LOCAL                                        
         MVC   CASTYR,TGYRCDE      YEAR                                         
         MVC   CASTGUA,TACAGUA     GUARANTEE CODE                               
         MVC   CASTPAYI,TCPAY      PAYMENT AMOUNT - WILL ADJUST LATER           
         MVC   CASTSPAY,TCPAY      SAVED PAYMENT AMOUNT FOR DISK OPTION         
         MVC   CASTSPNH,TCSUBPNH   SUBJ. TO P&H                                 
         MVC   CASTPNH,TCPNH       P&H                                          
         MVC   CASTSVPH,TCPNH      SAVED P&H FOR DISK OPTION                    
         MVC   CASTINR,TCINR       I&R                                          
*                                                                               
         CLI   TGBTYPE,TABRTYE     IF NOT BILLING TYPE E                        
         BNE   PCAST05                                                          
         TM    WHEN,X'20'          OR IF BILL TYPE E BUT RUNNING SOON           
         BZ    PCAST10                                                          
PCAST05  MVC   CASTEXP,TACAEXP     REPLACE FIRST SERVICES DATE WITH             
         OC    TACAEXP,TACAEXP     EXPIRATION DATE                              
         BNZ   PCAST10                                                          
         OC    THCOEXP,THCOEXP                                                  
         BZ    PCAST10                                                          
         GOTO1 DATVAL,DMCB,THCOEXP,DUB                                          
         GOTO1 DATCON,DMCB,(0,DUB),(1,CASTEXP)                                  
*                                                                               
PCAST10  OC    CASTGUA,CASTGUA     IF HAVE GUARANTEE CODE                       
         BZ    *+8                                                              
         OI    THOPTS,THCAGUA      SET FLAG                                     
*                                                                               
         LA    R2,CASTNEXT         SET A(NEXT SLOT)                             
         ST    R2,THANXTCS                                                      
         XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------------         
*        SETUP FOR HST CALCULATIONS                                             
*----------------------------------------------------------------------         
GETPSTR  NTR1  BASE=*,LABEL=*                                                   
         L     R5,0(R1)               CANADIAN PROVINCE                         
         MVC   TGGSTRAT,=AL4(GSTRT)   DEFAULT 5.00% GST                         
         NI    TGPRVST,X'FF'-TGPSTHST-TGPSTQUE                                  
         XC    TGPSTRAT,TGPSTRAT      NO PST                                    
*                                                                               
         MVC   AIO,AIO2               GET SYSTEM RECORD FOR CAN PROV            
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'20',0)                                    
         BE    *+6                    RATE                                      
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO1                                                         
         L     R4,AIO2                                                          
         USING TAPCD,R4                                                         
         MVI   ELCODE,TAPCELQ         GET CANADIAN PROVINCE ELEMENT             
         BRAS  RE,GETEL                                                         
         BNE   GETPX                                                            
         MVC   TGGSTRAT,TAPCGSTR      GET GST RATE                              
         CLC   0(3,R5),=C'   '        ANY PROVINCE?                             
         BNH   GETPX                  NO, LEAVE                                 
*                                                                               
         ZIC   R1,TAPCNSUB                                                      
         LA    R3,TAPCPROV                                                      
E1       USING TAPCPROV,R3                                                      
GETP100  CLC   E1.TAPCPROV,0(R5)      MATCH ON PROVINCE                         
         BE    GETP200                                                          
         AHI   R3,TAPCSLNQ            BUMP TO NEXT PROVINCE                     
         BCT   R1,GETP100                                                       
         B     GETPX                  LEAVE, PROVINCE NOT IN TABLE              
*                                                                               
GETP200  MVC   TGPRVST,E1.TAPCSTAT    SET HST INDICATOR                         
         TM    TGPRVST,TGPSTHST       FOR NOW, ONLY CALC PST WHEN HST           
         BZ    GETPX                                                            
         MVC   TGPSTRAT,E1.TAPCPSTR   SET PST RATE                              
         CLC   0(3,R5),=C'QC '        QUEBEC                                    
         BE    GETP210                                                          
         CLC   0(3,R5),=C'PE '        OR PRINCE EDWARD ISLAND                   
         BNE   GETPX                                                            
GETP210  OI    TGPRVST,TGPSTQUE       USE QUEBEC CALCULATION                    
*                                                                               
GETPX    XIT1                                                                   
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              SET ADDITIONAL BILLING RULES FOR OFFICE O                        
*                                                                               
ADDLBR   NTR1  BASE=*,LABEL=*                                                   
         MVI   ADRBTYPE,0                                                       
         XC    ADRFUTA(L'TABRRATE),ADRFUTA                                      
*                                                                               
         L     R4,AIO              GET BILLING RULES ELEMENT                    
         USING TABRD,R4                                                         
         MVI   ELCODE,TABRELQ                                                   
         BRAS  RE,GETEL                                                         
         B     ADDLBR7                                                          
ADDLBR5  BRAS  RE,NEXTEL                                                        
ADDLBR7  BNE   ADDLBRX                                                          
         TM    TABRSTAT,TABRSACP   SPECIAL ACOPY BILLING RULES                  
         BZ    ADDLBR5                                                          
*                                                                               
         MVC   ADRBTYPE,TABRTYPE                                                
         MVC   ADRFUTA(L'TABRRATE),TABRFUTA   BILLING RATES                     
ADDLBRX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CALCS NEXT START DATE BASED ON PWOS DATE IN DUB          
*                                                                               
NXTSTART NTR1  BASE=*,LABEL=*                                                   
         GOTO1 DATCON,DMCB,(1,DUB),(8,WORK)  BUILD INPUT TO PERVAL              
         MVI   WORK+8,C'-'                                                      
         MVC   WORK+9(5),THLCYC                                                 
*                                                                               
         LA    R3,BLOCK            R3=A(OUTPUT BLOCK FROM PERVAL)               
         USING PERVALD,R3                                                       
         GOTO1 PERVAL,DMCB,(14,WORK),(R3)  RESOLVE PERIOD EXPRESSION            
*                                                                               
         MVC   TCPCYCS,PVALPEND    RETURNS NEXT START IN END DATE               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CALCULATE NOTICE'S DUE DATE                           
         SPACE 1                                                                
CALCDUE  NTR1  BASE=*,LABEL=*                                                   
         USING GETRETD,R2                                                       
         LA    R2,WORK                                                          
         XC    WORK,WORK                                                        
****     OI    GRDFLAG,GRDFTAL+GRDFHLOK+GRDFBACK                                
         OI    GRDFLAG,GRDFTPU+GRDFHLOK+GRDFBACK                                
         GOTO1 DATCON,DMCB,(1,TCPCYCS),(3,GRDIDY)                               
         MVC   GRDHRS,=Y(1*24)                                                  
         MVI   GRDITM,1                                                         
         GOTO1 MYGETRET,(R2)                                                    
         GOTOR UNIOND,DMCB,MYGETRET,(R2),ELEM                                   
         BE    *+6                 TP HOLIDAY                                   
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(3,GRDODY),(20,SVDUEDT)                              
         DROP  R2                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO FILTER OUT ACTRA PERFORMERS IF COMMERCIAL             
*              IS SET UP AS ACTRA TYPE 2404A                                    
         SPACE 1                                                                
ACTFLT   NTR1  BASE=*,LABEL=*                                                   
         CLI   THCOCTYP,CCTY04A    IF COMMERCIAL IS ACTRA TYPE 2404A            
         BE    AF10                                                             
         CLI   THCOCTYP,CCTY2404   OR 2404                                      
         BNE   AFYES                                                            
         SPACE 1                                                                
AF10     GOTO1 GETREC              GET CAST RECORD                              
         SPACE 1                                                                
         USING TACAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   AFYES                                                            
         CLC   TACAUN,=C'ACT'      IF UNION IS ACTRA                            
         BE    AFNO                                                             
         CLC   TACALOCL,=C'CAN'    OR LOCAL IS CAN                              
         BE    AFNO                FILTER OUT THIS PERFORMER                    
         SPACE 1                                                                
         CLI   THCOCTYP,CCTY2404   IF ACTRA TYPE IS 2404                        
         BNE   AFYES                                                            
         CLC   TACAUN,=C'NON'      ALSO FILTER OUT NON UNION                    
         BE    AFNO                                                             
         DROP  R4                                                               
         SPACE 1                                                                
AFYES    XR    RC,RC                                                            
AFNO     LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SEE IF COMMERCIAL QUALIFIES FOR HFUPDATE              
         SPACE 1                                                                
         USING TLCAPD,R2                                                        
QUALHFU  NTR1  BASE=*,LABEL+*                                                   
         CLI   RECNUM,HC           ONLY EXECUTE ROUTINE IF RUNNING              
         BNE   QHFUYES             HFUPDATE REPORT                              
         CLI   THRERUN,C'Y'        AND NOT DOING A RERUN                        
         BE    QHFUYES                                                          
         TM    THCOSTA2,TACOCHHF   COMM'L/CAST MUST BE CHANGED                  
         BZ    QHFUNO                                                           
         SPACE 1                                                                
         MVC   THHLDPEB,THHLAPEB   DEFAULT TO AGENCY RULES                      
         SPACE 1                                                                
         OC    TGCLI,TGCLI         IF CLIENT IS ON COMMERCIAL                   
         BZ    QHFU20                                                           
         MVC   THKEY,TLCAPKEY      SAVE HOLDING FEE KEY AND READ CLIENT         
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'A4',TGCLI)                                
         BNE   QHFU10                                                           
         SPACE 1                                                                
         USING TACID,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACIELQ      IF PAPER? ELECTRONIC? BOTH?                  
         BRAS  RE,GETEL            SET AT CLIENT LEVEL                          
         BNE   QHFU10              SAVE IT                                      
         TM    TACISTAT,TACIHSPO+TACIHSEL+TACIHSPE+TACIHSNC                     
         BZ    QHFU10                                                           
         MVC   THHLDPEB,TACISTAT                                                
         DROP  R4                                                               
         SPACE 1                                                                
QHFU10   MVC   TLCAPKEY,THKEY      RESTORE HOLDING FEE KEY                      
         GOTO1 HIGH                AND RESTORE READ SEQUENCE                    
         SPACE 1                                                                
QHFU20   TM    THHLDPEB,TAAYHSPO+TAAYHSNC PO AND NC AGENCIES/CLIENTS            
         BNZ   QHFUNO                     DO NOT QUALIFY FOR HFUPDATE           
         SPACE 1                                                                
QHFUYES  XR    RC,RC                                                            
QHFUNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R2                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SEE IF COMMERCIAL QUALIFIES FOR RERUN                 
         SPACE 1                                                                
QUALRER  NTR1  BASE=*,LABEL+*                                                   
         CLI   THRERUN,C'Y'        ONLY EXECUTE ROUTINE IF RUNNING              
         BNE   QRERYES             RERUN                                        
         SPACE 1                                                                
         CLC   AGYALPHA,=C'D3'     UNLESS RUNNING ON FQA                        
         BE    *+12                                                             
         TM    THCOSTA2,TACOLHFS   ALWAYS IGNORE LAST SOONS                     
         BO    QRERNO                                                           
         SPACE 1                                                                
         CLI   RECNUM,HC           IF RERUNNING HOLDING FEE UPDATE              
         BNE   QRER10                                                           
         TM    THCOSTA2,TACOHFUP   ONLY PICK UP HOLDING FEES                    
         BZ    QRERNO              THAT WERE PROCESSED BY HFUPDATE              
         B     QRERYES                                                          
         SPACE 1                                                                
QRER10   TM    THCOSTA2,TACOHFUP   IF RERUNNING REGULAR HOLDING FEES            
         BO    QRERNO              DO NO PICK UP HFUPDATES                      
QRERYES  XR    RC,RC                                                            
QRERNO   LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
PUTHDR   NTR1  BASE=*,LABEL=*                                                   
         LA    R3,THDSKREC         R3=A(DISK REC)                               
         USING DISKD,R3                                                         
         MVI   0(R3),C' '          MOVE SPACES TO DISK                          
         MVC   1(DISKLNQ-1,R3),0(R3)                                            
         MVC   DSKTYPE,=C'01'      SET HEADER TYPE                              
         BRAS  RE,HEADFOOT                                                      
         AP    THDSKCNT,=P'1'      INCREMENT COUNT                              
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
HEADFOOT NTR1  BASE=*,LABEL=*                                                   
         GOTO1 DATCON,DMCB,(0,TGTODAY0),(20,DSKTODAY)                           
         L     R2,=A(HDDISK)                                                    
         CLI   ACTEQU,ACTDOWN                                                   
         BNE   *+8                                                              
         L     R2,=A(TADOWN)                                                    
         PUT   (R2),(R3)           PUT IT TO DISK                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS GUARANTEE VARIABLES                             *         
*        ON ENTRY ... R2=A(CURRENT CAST TABLE ENTRY)                  *         
***********************************************************************         
                                                                                
         USING CASTD,R2            R2=A(NEXT ENTRY)                             
SETGUA   NTR1  BASE=*,LABEL=*                                                   
         OC    CASTGUA,CASTGUA     EXECUTE ROUTINE IS CAST MEMBER               
         JZ    XIT                 IS ATTACHED TO GUARANTEE                     
                                                                                
         USING TLGUD,R4                                                         
         LA    R4,KEY                                                           
         XC    KEY,KEY             READ THE GUARANTEE RECORD                    
         MVI   TLGUCD,TLGUCDQ                                                   
         MVC   TLGUSSN,CASTSSN                                                  
         MVC   TLGUGUA,CASTGUA                                                  
         XC    TLGUGUA,=X'FFFFFFFF'                                             
         GOTO1 HIGH                                                             
         CLC   TLGUKEY,KEYSAVE                                                  
         JNE   XIT                                                              
         DROP  R4                                                               
                                                                                
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
                                                                                
         USING TAGUD,R4                                                         
         L     R4,AIO2             GET GUARANTEE DETAILS ELEMENT                
         MVI   ELCODE,TAGUELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   SG20                                                             
                                                                                
         CLC   TAGUCOM,TGCOM       IF NOTICE IS FOR PRIMARY COMMERCIAL          
         JNE   SG10                                                             
         OI    THSTAT3,THPRICOM    SET PRIMARY COMMERCIAL INDICATOR             
                                                                                
SG10     OC    TAGUEND,TAGUEND     IF LARGE OVERSCALE GUARANTEE                 
         JZ    SG20                SAVE GUARANTEE EXPIRATION DATE               
         MVC   CASTEXP,TAGUEND                                                  
                                                                                
         CLC   CASTEXP,TCPCYCE     IF EXPIRATION DATE IS LATER                  
         JNL   SG20                THAN HOLDING FEE CYCLE                       
         OI    THSTAT2,THGRTEXP    WE NEED TO GIVE MESSAGE                      
         DROP  R4                                                               
                                                                                
         USING TAGXD,R4                                                         
SG20     L     R4,AIO2             IF EXCLUDED USES ELEMENT IS                  
         MVI   ELCODE,TAGXELQ      FOUND ...                                    
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
                                                                                
         ZIC   R0,TAGXLEN                                                       
         SHI   R0,TAGXLNQ          R0=# OF EXCLUDED USES                        
         LA    R4,TAGXUSE          R4=A(LIST OF EXCLUDED USES)                  
         DROP  R4                                                               
                                                                                
SG30     CLC   TGUSEQU,0(R4)       IF CURRENT USE IS EXCLUDED ...               
         JE    SG40                                                             
         LA    R4,L'TAGXUSE(R4)                                                 
         BCT   R0,SG30                                                          
         J     SGX                                                              
                                                                                
SG40     OI    CASTSTAT,CASTNAGU   ... SET CAST STATUS                          
         DROP  R2                                                               
                                                                                
SGX      MVC   AIO,AIO1                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE GETS BILLING YTD FOR CURRENT SSN                         
*                                                                               
GETYTD   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,BLOCK            R2=A(YTD PARAMETER BLOCK)                    
         USING TYD,R2                                                           
         XC    0(TYLNQ,R2),0(R2)   CLEAR TAYTD PARAMETER BLOCK                  
         MVC   TYASYSIO,TASYSIO    A(SYSIO)                                     
         MVC   TYCUR,TMCURR        SET CURRENCY                                 
         MVC   TYEMP,TMEMP         EMPLOYER                                     
         MVC   TYSSN,TMSSN         SOCIAL SECURITY NUMBER                       
         MVC   TYPEND,TGTODAY1     AS OF DATE                                   
         MVC   TYTRACE,TMTRACE     TRACE OPTION                                 
         OI    TYSTAT,TYSTBILL     SET WANT BILLING YTD                         
*                                                                               
         GOTO1 TGTAYTD,DMCB,(RC),SYSCOMM,(R2)  GET YTD DATA                     
*                                                                               
         GOTOR MYTRACE2,DMCB,=C'TAYTD BLOCK',TYLNQ,(R2)                         
*                                                                               
         MVC   TMYTD(TYBYLNQ),TYBYTD  SET YTD AMOUNTS IN TALIM BLOCK            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE HANDLES RECORD TRACES                                    
*                                                                               
MYTRACE  NTR1  BASE=*,LABEL=*                                                   
         TM    THOPTS,THTRACE      TEST TRACE ENABLED                           
         BZ    MTX                                                              
         L     R2,0(R1)            A(LITERAL)                                   
         ZIC   R3,0(R1)            L'LITERAL                                    
         GOTO1 TRACE,DMCB,AIO,0,(R2),(R3)                                       
MTX      XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 3                                                                
*              ROUTINE HANDLES DATA TRACES                                      
*                                                                               
MYTRACE2 NTR1  BASE=*,LABEL=*                                                   
         TM    THOPTS,THTRACE      TEST TRACE ENABLED                           
         BZ    MT2X                                                             
         LM    R2,R4,0(R1)         A(LITERAL), L(DATA), A(DATA)                 
         ZIC   RF,0(R1)            L'LITERAL                                    
         GOTO1 TRACE,DMCB,(R4),(R3),(R2),(RF)                                   
MT2X     XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SAVE ADDITIONAL SEASONAL INFORMATION                  
*              ON CAST RECORD                                                   
         SPACE 1                                                                
         USING SORTD,R3                                                         
SVSEASIN NTR1  BASE=*,LABEL=*                                                   
         TM    THSTAT,THSUMM                                                    
         BO    SSIX                                                             
         CLI   THCOTYPE,CTYSEAS2                                                
         BNE   SSIX                                                             
         SPACE 1                                                                
         MVC   KEY+TLDRDA-TLDRD(L'TLDRDA),SORTDA                                
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         USING TAHFD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAHFELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   SSIX                                                             
         SPACE 1                                                                
         USING GETRETD,R2                                                       
         LA    R2,WORK                                                          
         XC    WORK,WORK                                                        
****     OI    GRDFLAG,GRDFTAL                                                  
         OI    GRDFLAG,GRDFTPU                                                  
         GOTO1 DATCON,DMCB,(1,TAHFNXTS),(3,GRDIDY)                              
         MVC   GRDHRS,=Y(11*24)                                                 
         GOTO1 MYGETRET,(R2)                                                    
         GOTOR UNIOND,DMCB,MYGETRET,(R2),ELEM                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(3,GRDODY),(1,TAHFHDTE)                              
         DROP  R2,R4                                                            
         SPACE 1                                                                
         BRAS  RE,HFPUTREC         WRITE BACK THE RECORD                        
         GOTOR MYTRACE,DMCB,=C'SEA CAST'                                        
SSIX     MVC   AIO,AIO1                                                         
         XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PRINT OUT HOLDING FEE MESSAGES                        
         SPACE 1                                                                
         USING LINED,R3                                                         
PRTMSG   NTR1  BASE=*,LABEL=*                                                   
         TM    THSTAT,THSUMM       NEVER PRINT MESSAGES ON SUMMARIES            
         BO    PMNO                                                             
         CLC   LINSSN,SPACES       ONLY ATTEMPT TO PRINT MESSAGE                
         BNE   PMNO                IF THERE IS ROOM FOR IT                      
         SPACE 1                                                                
         MVC   LINSSN(L'RECMSG),SPACES                                          
         SPACE 1                                                                
         CLI   THCOTYPE,CTYSEAS2   PRINT MESSAGES FOR TYPE H COMML'S            
         BNE   PM30                                                             
         SPACE 1                                                                
         TM    MSGSTAT,SEA1PRT     PRINT FIRST LINE FOR TYPE H                  
         BO    PM10                                                             
         OI    MSGSTAT,SEA1PRT                                                  
         MVC   LINSSN(L'LTSEA11),LTSEA11                                        
         OC    THCO2AIR,THCO2AIR                                                
         BZ    PM130                                                            
         MVC   LINSSN(L'LTSEA21),LTSEA21                                        
         B     PM130                                                            
         SPACE 1                                                                
PM10     TM    MSGSTAT,SEA2PRT     PRINT SECOND LINE FOR TYPE H                 
         BO    PM20                                                             
         OI    MSGSTAT,SEA2PRT                                                  
         OC    THCO2AIR,THCO2AIR                                                
         BZ    PM120                                                            
         XC    SVDUEDT,SVDUEDT                                                  
         MVC   LINSSN(L'LTSEA22),LTSEA22                                        
         B     PM130                                                            
         SPACE 1                                                                
PM20     TM    MSGSTAT,SEA3PRT     PRINT THIRD LINE FOR TYPE H                  
         BO    PM30                                                             
         OI    MSGSTAT,SEA3PRT                                                  
         OC    THCO2AIR,THCO2AIR                                                
         BZ    PM30                                                             
         MVC   LINSSN(L'LTSEA23),LTSEA23                                        
         MVC   LINSSN+L'LTSEA23(L'LTSEA24),LTSEA24                              
         B     PM130                                                            
         SPACE 1                                                                
PM30     TM    MSGSTAT,SPCYPRT     PRINT SPLIT CYCLE INDICATOR                  
         BO    PM40                                                             
         OI    MSGSTAT,SPCYPRT                                                  
         CLI   THCOSPCY,C'Y'                                                    
         BNE   PM40                                                             
         MVC   LINSSN(L'LTSPCY),LTSPCY                                          
         B     PM130                                                            
         SPACE 1                                                                
PM40     TM    MSGSTAT,EXPIRE1     LAST CYCLE BEFORE MPU #1                     
         BO    PM50                                                             
         OI    MSGSTAT,EXPIRE1                                                  
         TM    THSTAT3,THEXPIRE                                                 
         BZ    PM50                                                             
         MVC   LINSSN(L'LTEXP1),LTEXP1                                          
         B     PM130                                                            
         SPACE 1                                                                
PM50     TM    MSGSTAT2,EXPIRE2    LAST CYCLE BEFORE MPU #2                     
         BO    PM60                                                             
         OI    MSGSTAT2,EXPIRE2                                                 
         TM    THSTAT3,THEXPIRE                                                 
         BZ    PM60                                                             
         MVC   LINSSN(L'LTEXP2),LTEXP2                                          
         B     PM130                                                            
         SPACE 1                                                                
PM60     TM    MSGSTAT,FCSTPEND    FINAL CAST PENDING                           
         BO    PM70                                                             
         OI    MSGSTAT,FCSTPEND                                                 
         OC    THCOVDTE,THCOVDTE                                                
         BNZ   PM70                                                             
         MVC   LINSSN(L'LTFCPEND),LTFCPEND                                      
         B     PM130                                                            
         SPACE 1                                                                
PM70     TM    MSGSTAT,GRTEXPR     GUARANTEE EXPIRING                           
         BO    PM80                                                             
         OI    MSGSTAT,GRTEXPR                                                  
         TM    THSTAT2,THGRTEXP                                                 
         BZ    PM80                                                             
         MVC   LINSSN(L'LTGRTEXP),LTGRTEXP                                      
         B     PM130                                                            
         SPACE 1                                                                
PM80     TM    MSGSTAT2,DLRCOVR    DEALER CYCLE                                 
         BO    PM90                                                             
         OI    MSGSTAT2,DLRCOVR                                                 
         TM    THSTAT3,THDLRCVR                                                 
         BZ    PM90                                                             
         MVC   LINSSN(L'LTDLRCVR),LTDLRCVR                                      
         B     PM130                                                            
         SPACE 1                                                                
PM90     TM    MSGSTAT2,PERCYCID   PER CYCLE ID                                 
         BO    PM100                                                            
         OI    MSGSTAT2,PERCYCID                                                
         TM    THCOSTA2,TACOPCYC                                                
         BZ    PM100                                                            
         MVC   LINSSN(L'LTPERCYC),LTPERCYC                                      
         OI    MSGSTAT2,PRICOMML                                                
         B     PM130                                                            
         SPACE 1                                                                
PM100    TM    MSGSTAT2,PRICOMML   PRIMARY COMMERCIAL                           
         BO    PM110                                                            
         OI    MSGSTAT2,PRICOMML                                                
         TM    THSTAT3,THPRICOM                                                 
         BZ    PM110                                                            
         MVC   LINSSN(L'LTPRICOM),LTPRICOM                                      
         B     PM130                                                            
         SPACE 1                                                                
         USING GETRETD,R2                                                       
PM110    TM    MSGSTAT,DUEDATE     DUE DATE                                     
         BO    PMNO                                                             
         OI    MSGSTAT,DUEDATE                                                  
         CLI   THCOTYPE,CTYSEAS2                                                
         BE    PMNO                                                             
PM120    MVC   LINSSN(L'LTSEA12),LTSEA12                                        
         LA    R2,WORK                                                          
         XC    WORK,WORK                                                        
***      OI    GRDFLAG,GRDFTAL+GRDFHLOK+GRDFBACK                                
         OI    GRDFLAG,GRDFTPU+GRDFHLOK+GRDFBACK                                
         GOTO1 DATCON,DMCB,(1,TCPCYCS),(3,GRDIDY)                               
         MVC   GRDHRS,=Y(1*24)                                                  
         MVI   GRDITM,1                                                         
         GOTO1 MYGETRET,(R2)                                                    
         GOTOR UNIOND,DMCB,MYGETRET,(R2),ELEM                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(3,GRDODY),(8,LINSSN+L'LTSEA12)                      
         GOTO1 DATCON,DMCB,(3,GRDODY),(20,SVDUEDT)                              
         B     PM130                                                            
         SPACE 1                                                                
         CLC   AGYALPHA,=C'D3'      IF ON FQA                                   
         BNE   PM130                                                            
         CLC   TGCOM,=X'00071EA2'   AND COMMERCIAL IS RCOMML17                  
         BE    PM130                                                            
         CLC   TGCOM,=X'00071ED8'   H33.2.1.35                                  
         BE    PM130                                                            
         CLC   TGCOM,=X'00071EED'   H33.2.1.56                                  
         BE    PM130                                                            
         CLC   TGCOM,=X'00071F30'   H33.2.1.70                                  
         BE    PM130                                                            
         CLC   TGCOM,=X'00071EB5'   H33.2.6.27                                  
         BE    PM130                                                            
         CLC   TGCOM,=X'00071F89'   H33.2.1.79                                  
         BE    PM130                                                            
         CLC   TGCOM,=X'00071F3D'   OR C33.6.1                                  
         BE    PM130                                                            
         MVC   LINSSN+L'LTSEA12(8),=C'DEC31/07'                                 
         DROP  R2                                                               
         SPACE 1                                                                
PM130    BRAS  RE,PUTMSG                                                        
         SPACE 1                                                                
PMYES    XR    RC,RC                                                            
PMNO     LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
LTSEA11  DC    C'Hold for use during next season'                               
LTSEA12  DC    C'Due Date: '                                                    
         SPACE 1                                                                
LTSEA21  DC    C'2 Consecutive seasons have passed'                             
LTSEA22  DC    C'Must renegotiate to use for additional seasons'                
LTSEA23  DC    C'This is only to advise to renegotiate, '                       
LTSEA24  DC    C'not a holding fee'                                             
         SPACE 1                                                                
LTSPCY   DC    C'Split Holding Fee Cycle'                                       
         SPACE 1                                                                
LTEXP1   DC    C'Check performer''s MPU Expiry Date.'                           
LTEXP2   DC    C'Rate may be subject to renegotiation.'                         
         SPACE 1                                                                
LTLCMPU  DC    C'Last Cycle before MPU'                                         
         SPACE 1                                                                
LTFCPEND DC    C'Final Cast Pending'                                            
         SPACE 1                                                                
LTGRTEXP DC    C'Current guarantee expiring - See date above'                   
         SPACE 1                                                                
LTDLRCVR DC    C'Includes performer(s) covered by Dealer cycle'                 
         SPACE 1                                                                
LTPERCYC DC    C'Holding Fee for a Per Cycle Guarantee'                         
         SPACE 1                                                                
LTPRICOM DC    C'Primary Commercial for a Per Cycle Guarantee'                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              SPECIAL ROUTINE FOR PRINTING OUT BILLING TYPE 20                 
*              TALENT MASTERS CHARGE                                            
*                                                                               
TYPE20   NTR1  BASE=*,LABEL=*                                                   
         CLI   TGBTYPE,TABRTY20    IF BILLING TYPE 20                           
         BNE   T20X                                                             
*                                                                               
         USING LINED,R3                                                         
         A     R2,THCOMM                  ADD MASTERS CHARGE                    
         A     R2,THHNDC                                                        
         A     R2,THHNDI                                                        
*                                                                               
         MVC   LINSSN(L'LTTLMAST),LTTLMAST                                      
         CLC   TGAGY,=CL6'0585'           IF GRUPO GALLEGOS,                    
         BNE   TYP2010                                                          
         MVC   LINSSN(L'LTTLLUNA),LTTLLUNA   PRINT LUNA P&T                     
         L     R1,THCOMM                  AND PRINT OUT MASTER CHARGE           
         LA    RF,LINSSN+25               SEPARATELY                            
         EDIT  THCOMM,(11,LINSSN+25),2,FLOAT=-,ZERO=NOBLANK                     
         B     TYP2020                                                          
TYP2010  L     R1,THCOMM                  AND PRINT OUT MASTER CHARGE           
         LA    RF,LINSSN+15               SEPARATELY                            
         EDIT  THCOMM,(11,LINSSN+15),2,FLOAT=-,ZERO=NOBLANK                     
*                                                                               
TYP2020  MVC   LINLIT,LTTLMAST                                                  
         CLC   TGAGY,=CL6'0585'           IF GRUPO GALLEGOS,                    
         BNE   *+10                                                             
         MVC   LINLIT,LTTLLUNA            PRINT LUNA P&T                        
         MVC   LINPAYC,LINSSN+15                                                
         BRAS  RE,PUTMSG                  PUT MESSAGE REC TO WEB DISK           
         MVC   LINLIT(L'LTTAX20),LTTAX20                                        
         MVC   LINPAYC,SPACES                                                   
         DROP  R3                                                               
T20X     XIT1  REGS=(R2)                                                        
*                                                                               
LTTAX20  DC    CL25'Payroll Tax && Handling'                                    
LTTLMAST DC    CL25'Talent Masters'                                             
LTTLLUNA DC    CL25'Luna Productions && Talent'                                 
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CALCULATES AGENCY COMMISSION                             
*                                                                               
COMMCALC NTR1  BASE=*,LABEL=*                                                   
         CLI   THEPCOMM,TAEPCNO    IF DON'T CALCULATE                           
         BE    CCALCX                                                           
         CLI   THEPCOMM,TAEPCCLI   OR CALC AT CLIENT LEVEL ONLY                 
         BE    CCALCX              THEN DON'T BOTHER                            
         SPACE                                                                  
         L     R1,THPAYI           R1=AMOUNT TO CALC COMMISSION ON              
         A     R1,THPAYC              PAYMENT AMOUNT                            
         CLI   TGBTYPE,TABRTY20    IF TYPE 20                                   
         BNE   CCALC10                                                          
         A     R1,THINR            ADD I&R                                      
         B     CCALC20                                                          
CCALC10  CLI   THEPCOMM,TAEPCPAY   BASE COMMISSION ON PAY ONLY                  
         BE    *+8                                                              
         A     R1,THPNH            PLUS P&H                                     
         CLI   THEPCOMM,TAEPCHND                                                
         BE    *+16                                                             
         CLI   THEPCOMM,TAEPCTNH                                                
         BNE   *+16                                                             
         A     R1,THTAXES          ADD IN TAX                                   
         A     R1,THHNDI           AND HANDLING                                 
         A     R1,THHNDC                                                        
CCALC20  LH    R0,THEPRATE         R0=COMMISSION RATE                           
         MR    R0,R0                                                            
         D     R0,=F'5000'                                                      
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         ST    R1,THCOMM           SAVE IN THCOMM                               
CCALCX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CALCULATES SIGNATORY FEE                                 
         SPACE                                                                  
SFEECALC NTR1  BASE=*,LABEL=*                                                   
         SPACE                                                                  
         OC    THSGNINF,THSGNINF   IF AGENCY IS SET UP FOR SIGNATORY            
         BZ    SFCX                FEE                                          
                                                                                
         L     R1,THPAYI           R1=AMOUNT TO CALC SIGNATORY FEE ON           
         A     R1,THPAYC                                                        
         CLI   THAGYSRL,1          WAGE ONLY                                    
         BE    SFC100                                                           
                                                                                
         CLI   THAGYSRL,0          PNH                                          
         BE    SFC010                                                           
         CLI   THAGYSRL,3                                                       
         BE    SFC010                                                           
         CLI   THAGYSRL,5                                                       
         BE    SFC010                                                           
         CLI   THAGYSRL,6                                                       
         BNE   SFC020                                                           
SFC010   A     R1,THPNH                                                         
                                                                                
SFC020   CLI   THAGYSRL,0          TAXES                                        
         BE    SFC025                                                           
         CLI   THAGYSRL,2                                                       
         BE    SFC025                                                           
         CLI   THAGYSRL,5                                                       
         BE    SFC025                                                           
         CLI   THAGYSRL,7                                                       
         BNE   SFC030                                                           
SFC025   A     R1,THGSTU                                                        
         A     R1,THTAXES                                                       
                                                                                
SFC030   CLI   THAGYSRL,0          HANDLING                                     
         BE    SFC035                                                           
         CLI   THAGYSRL,4                                                       
         BE    SFC035                                                           
         CLI   THAGYSRL,6                                                       
         BE    SFC035                                                           
         CLI   THAGYSRL,7                                                       
         BNE   SFC040                                                           
SFC035   A     R1,THHNDI                                                        
         A     R1,THHNDC                                                        
                                                                                
SFC040   DS    0H                                                               
*        A     R1,THFICR           NOT IN THE SPECS                             
*        A     R1,THCOMM                                                        
*        A     R1,THAPPL                                                        
                                                                                
*        LH    R0,THSGNRTE         R0=SIGNATORY FEE RATE                        
SFC100   ZICM  R0,THSGNRTE,2                                                    
         MR    R0,R0                                                            
         D     R0,=F'5000'                                                      
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         SPACE                                                                  
*        C     R1,THSGNCAP                                                      
         ZICM  RE,THSGNCAP,4                                                    
         CR    R1,RE                                                            
         BNH   SFC500                                                           
         ZICM  R1,THSGNCAP,4                                                    
SFC500   ST    R1,THSIGN           SAVE IN THCOMM                               
SFCX     XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              HEADLINE ROUTINES (HEADHOOK)                                     
*                                                                               
MYHDHK   NTR1  BASE=*,LABEL=*                                                   
         TM    THSTAT,THSUMM       SEPARATE ROUTINES FOR SUMMARY & DISK         
         BO    HDH14                                                            
         MVC   H1+15(L'THOFFNM),THOFFNM TALENT PARTNERS OFFICE NAME             
         MVC   H2+15(L'THOFFAD),THOFFAD                        ADDRESS          
*                                                                               
         EDIT  THPAGE,(2,H1+124)   CURRENT PAGE                                 
         EDIT  THNPAGES,(2,H1+130),ALIGN=LEFT TOTAL N'PAGES                     
         AP    THPAGE,=P'1'        BUMP PAGE NUMBER                             
*                                                                               
         LA    R1,THAGYNM          PRINT AGENCY NAME                            
         CLI   THATTNM,C' '                                                     
         BNH   *+8                                                              
         LA    R1,THATTNM          THERE'S AN ATTENTION NAME OVERRIDE           
         MVC   H4+14(L'THNAMES),0(R1)                                           
*                                                                               
         LA    RF,TH1STAD          SELECT WHICH ADDRESS TO PRINT                
         LA    R0,THNADDS          R0=N'CHOICES                                 
HDH6     CLI   0(RF),C' '          IF ADDRESS PRESENT AT THIS LEVEL             
         BNH   *+6                                                              
         LR    R2,RF               USE IT                                       
         LA    RF,L'THADDRS(RF)                                                 
         BCT   R0,HDH6                                                          
         GOTO1 CHOPPER,DMCB,(120,(R2)),(30,H5+14),(C'P',4)                      
         L     RE,8(R1)            RE=N'LINES USED                              
         MH    RE,=H'132'                                                       
         LA    RE,H5(RE)           RE=A(NEXT AVAILABLE LINE)                    
*                                                                               
         LA    RF,TH1STATT         SELECT WHICH ATTN NAME TO PRINT              
         LA    R0,THNATTS          R0=N'CHOICES                                 
         B     *+12                                                             
HDH8     CLI   0(RF),C' '          IF ATTN NAME PRESENT AT THIS LEVEL           
         BNH   *+6                                                              
         LR    R2,RF               USE IT                                       
         LA    RF,L'THATTNMS(RF)                                                
         BCT   R0,HDH8                                                          
         CLC   0(L'THATTNMS,R2),SPACES  IF WE ACTUALLY HAVE A NAME              
         BNH   HDH9                                                             
         MVC   14(5,RE),=C'ATTN:'       MOVE OUT TAG                            
         LA    R1,H8                    IF ON SAME LINE AS ESTIMATE #           
         CR    R1,RE                                                            
         BNE   *+14                                                             
         MVC   20(L'THATTNMS-10,RE),0(R2) CAN'T DISPLAY ENTIRE NAME             
         B     *+10                                                             
         MVC   20(L'THATTNMS,RE),0(R2)    ELSE DISPLAY ENTIRE ATTN NAME         
*                                                                               
HDH9     MVC   H9+14(L'THCLINM),THCLINM     CLIENT NAME                         
         MVC   H10+14(L'THPRDNM),THPRDNM    PRODUCT NAME                        
         MVC   H11+14(L'THCOMNM),THCOMNM    COMMERCIAL NAME                     
         MVC   H12+14(L'TGCID),TGCID        COMMERCIAL ID                       
*                                                                               
         MVC   H13+14(8),THFIDATE           FILM DATE                           
         MVC   H13+39(L'THFISTUD),THFISTUD       STUDIO                         
         MVC   H13+66(L'THFICITY),THFICITY       CITY                           
*                                                                               
         MVC   H14+14(8),THREDATE           RECORD DATE                         
         MVC   H14+39(L'THRESTUD),THRESTUD         STUDIO                       
         MVC   H14+66(L'THRECITY),THRECITY         CITY                         
*                                                                               
         MVC   H3+106(26),THEMPNM  EMPLOYER NAME                                
*                                                                               
         MVC   H4+106(6),TGAGY     AGENCY,                                      
         MVC   H4+113(6),TGCLI     CLIENT,                                      
         MVC   H4+120(6),TGPRD     PRODUCT CODES                                
*                                                                               
         MVC   H5+106(8),THTPC     TPC STAFF                                    
         MVC   H6+106(8),THCOAIR   FIRST AIR DATE                               
         MVC   H7+106(8),THCOFCYC  FIRST FIXED CYCLE DATE                       
         MVC   H8+106(8),THCOEXP   EXPIRATION DATE                              
         MVC   H9+106(L'THCOLEN),THCOLEN     LENGTH                             
         MVC   H10+106(L'TGMENAME),TGMENAME  MEDIA NAME                         
*                                                                               
         LA    RF,H12                                                           
         CLC   THLID,SPACES                IF LIFT EXISTS                       
         BE    HDH12                                                            
         MVC   H11+89(L'LTLID),LTLID       TAG FOR                              
         MVC   H11+106(L'THLID),THLID          LIFT ID                          
         MVC   H12+89(L'LTLILEN),LTLILEN   TAG FOR                              
         MVC   H12+106(L'THLILEN),THLILEN      LIFT LENGTH                      
         LA    RF,H14                                                           
*                                                                               
HDH12    MVC   89(L'LTCYC,RF),LTCYC        TAG FOR CYCLE DATES                  
*                                                                               
         CLI   THCOTYPE,CTYSEAS2   IF SEASONAL COMMERCIAL                       
         BNE   HDH13                                                            
         TM    THHLAPEB,TAAYHSEL   AND NOT CALCULATING CYCLE DATES              
         BO    HDH13               IN FAKEHEAD ...                              
         ST    RF,TGFULL                                                        
*                                                                               
         USING GETRETD,R2                                                       
         LA    R2,WORK                                                          
         XC    WORK,WORK                                                        
****     OI    GRDFLAG,GRDFTAL                                                  
         OI    GRDFLAG,GRDFTPU                                                  
         GOTO1 DATCON,DMCB,(1,TCPCYCS),(3,GRDIDY)                               
         MVC   GRDHRS,=Y(11*24)                                                 
         GOTO1 MYGETRET,(R2)                                                    
         GOTOR UNIOND,DMCB,MYGETRET,(R2),ELEM                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(3,GRDODY),(1,TCPCYCS)                               
         DROP  R2                                                               
*                                                                               
         USING PERVALD,R2                                                       
         GOTO1 DATCON,DMCB,(1,TCPCYCS),(8,WORK)                                 
         MVI   WORK+8,C'-'                                                      
         MVC   WORK+9(5),THLCYC                                                 
         LA    R2,BLOCK                                                         
         GOTO1 PERVAL,DMCB,(14,WORK),('PVIN1DYL',(R2))                          
         MVC   TCPCYCE,PVALPEND                                                 
         DROP  R2                                                               
*                                                                               
         L     RF,TGFULL                                                        
*                                                                               
HDH13    GOTO1 DATCON,DMCB,(X'11',TCPCYC),(8,106(RF))     CYCLE DATES           
*                                                                               
HDH13A   L     R4,ABOX             SET UP BOXES                                 
         USING BOXD,R4                                                          
         MVC   BOXCOLS,SPACES      SET COLUMNS (FOR CHECK-OFF BOX)              
         MVI   BOXCOLS,C'L'        LEFT                                         
         MVI   BOXCOLS+131,C'R'    RIGHT                                        
*                                                                               
         MVC   BOXROWS,SPACES      SET ROWS                                     
         MVI   BOXROWS+14,C'T'          TOP OF CHECK-OFF BOX                    
         MVI   BOXROWS+LQDTLS,C'M'      TOP OF DETAIL HEADS                     
         MVI   BOXROWS+LQDTLS2,C'M'     BOTTOM OF DETAIL HEADS                  
         MVI   BOXROWS+LQDTLTOT-2,C'M'  TOP OF DETAIL TOTALS                    
         MVI   BOXROWS+LQDTLTOT,C'M'    BOTTOM OF DETAIL TOTALS                 
         MVI   BOXROWS+LQTOTAL-2,C'M'   TOP OF GRAND TOTAL BOX                  
         MVI   BOXROWS+LQTOTAL,C'B'     BOTTOM OF NOTICE                        
*                                                                               
         MVI   BOXYORN,C'Y'        INITIALIZE REMAINING FLDS                    
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
*                                                                               
         MVC   THP,P               SAVE CURRENT PRINT LINES                     
         MVC   THP2,P2                                                          
         MVC   P,SPACES            CLEAR THEM-NEED TO PRINT CHECK-OFFS          
         MVC   P2,SPACES                                                        
         MVI   FORCEMID,C'N'       TURN OFF MIDLINES FOR NOW                    
         OI    SPOOLIND,SPNSPACE   SUPPRESS LINE SKIP AFTER HEADS               
         OI    THSTAT,THHOOKED     SET WE JUST HAD HEADLINE HOOK                
         B     HDH20                                                            
*                                                                               
HDH14    MVC   H4+9(L'TGAGY),TGAGY        AGENCY                                
         MVC   H4+17(L'THAGYNM),THAGYNM   AGENCY NAME                           
         MVC   H5+9(L'TGCLI),TGCLI        CLIENT                                
         MVC   H5+17(L'THCLINM),THCLINM   CLIENT NAME                           
*                                                                               
         L     R4,ABOX             SET UP BOXES                                 
         USING BOXD,R4                                                          
         LA    R3,BOXCOLS                                                       
         LA    RE,THLSTSRT                                                      
         USING SORTD,RE                                                         
         CLI   SORTSTCG,0          IF CGROUP COKE                               
         BNE   *+8                                                              
         S     R3,=F'8'            SHIFT OVER 8 SPACES TO LEFT                  
         DROP  RE                                                               
*                                                                               
         USING HSLINED,R3                                                       
         MVC   BOXCOLS,SPACES      SET COLUMNS                                  
         MVI   HSBXL,C'L'                                                       
         MVI   HSBC1,C'C'                                                       
         MVI   HSBC2,C'C'                                                       
         MVI   HSBC3,C'C'                                                       
         MVI   HSBC4,C'C'                                                       
         MVI   HSBXR,C'R'                                                       
         CLI   RECNUM,HD           RESET FOR SUMMARY AND SECOND SUMMARY         
         BE    *+12                                                             
         MVI   HSBXR,C'C'                                                       
         MVI   HSBXR3,C'R'                                                      
*                                                                               
         MVI   BOXYORN,C'Y'        INITIALIZE REMAINING FLDS                    
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
*                                                                               
         MVC   BOXROWS,SPACES      SET ROWS                                     
         LA    RE,THLSTSRT                                                      
         USING SORTD,RE                                                         
         CLI   SORTSTCG,0          IF NOT CGROUP COKE                           
         BE    HDH18                                                            
         MVI   BOXROWS+6,C'T'      TOP                                          
         MVI   BOXROWS+8,C'M'      MIDDLE LINE                                  
         B     HDH20                                                            
         DROP  RE                                                               
*                                                                               
HDH18    MVI   BOXROWS+7,C'T'             TOP                                   
         MVI   BOXROWS+9,C'M'             MIDDLE LINE                           
         MVI   HSBC5,C'C'                 RESET COLUMNS                         
         MVI   HSBXR,C'C'                                                       
         MVI   HSBXR3,C'C'                                                      
         MVI   HSBXR2,C'R'                                                      
         OC    TGPRD,TGPRD                IF HAVE PRODUCT CODE                  
         BZ    HDH20                                                            
         MVC   H6+9(L'TGPRD),TGPRD        PRODUCT                               
         MVC   H6+17(L'THPRDNM),THPRDNM   PRODUCT NAME                          
*                                                                               
HDH20    BRAS  RE,WEBCHK                  IF GENERATING WEB FILE                
         BNE   HDHKX                                                            
         CLC   THPAGE,=PL2'2'                                                   
         BH    HDHKX                                                            
         BRAS  RE,PUTHEADS       PUT HF HEADERS TO BACKUP FILE AND MQ           
HDHKX    XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO FAKE OUT HEADHOOK FOR ELECTRONIC ONLY                 
*              AGENCIES AND CLIENTS                                             
         SPACE 1                                                                
FAKEHEAD NTR1  BASE=*,LABEL=*                                                   
         TM    THSTAT3,THELHDRP                                                 
         BNZ   FHEADX                                                           
*                                                                               
         USING GETRETD,R2                                                       
         CLI   THCOTYPE,CTYSEAS2   IF SEASONAL COMMERCIAL                       
         BNE   FHEAD10             CALCULATE SPECIAL CYCLE DATES                
*                                                                               
         LA    R2,WORK                                                          
         XC    WORK,WORK                                                        
****     OI    GRDFLAG,GRDFTAL                                                  
         OI    GRDFLAG,GRDFTPU                                                  
         GOTO1 DATCON,DMCB,(1,TCPCYCS),(3,GRDIDY)                               
         MVC   GRDHRS,=Y(11*24)                                                 
         GOTO1 MYGETRET,(R2)                                                    
         GOTOR UNIOND,DMCB,MYGETRET,(R2),ELEM                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(3,GRDODY),(1,TCPCYCS)                               
         DROP  R2                                                               
*                                                                               
         USING PERVALD,R2                                                       
         GOTO1 DATCON,DMCB,(1,TCPCYCS),(8,WORK)                                 
         MVI   WORK+8,C'-'                                                      
         MVC   WORK+9(5),THLCYC                                                 
         LA    R2,BLOCK                                                         
         GOTO1 PERVAL,DMCB,(14,WORK),('PVIN1DYL',(R2))                          
         MVC   TCPCYCE,PVALPEND                                                 
         DROP  R2                                                               
*                                                                               
FHEAD10  BRAS  RE,PUTHEADS       PUT HF HEADERS TO BACKUP FILE AND MQ           
         OI    THSTAT3,THELHDRP                                                 
FHEADX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              MIDLINE ROUTINES (MIDHOOK)                                       
*                                                                               
MYMDHK   NTR1  BASE=*,LABEL=*                                                   
         TM    THSTAT,THSUMM                                                    
         BO    MDHKX                                                            
         MVC   MID1+1(8),=C'PID     '                                           
         CLI   TGBTYPE,TABRTYE            BILLING TYPE E (EMS)                  
         BNE   MDHKX                                                            
         MVC   MID1+1(8),=C'S/S Numb'     SHOW SS# FOR NOW                      
MDHKX    XIT1                                                                   
         EJECT                                                                  
BDEPRTQ  NTR1  BASE=*,LABEL=*                                                   
         L     R2,ATWA                                                          
         USING T703FFD,R2                                                       
         MVC   BMASTD,TWAMASTC     SAVE ADDRESS OF MASTER                       
         L     R2,TWADCONS                                                      
         DROP  R2                                                               
*                                                                               
         USING TWADCOND,R2                                                      
         MVC   BLOGOC,TLOGOC       SAVE A(LOGOC)                                
         L     R2,BMASTD                                                        
         USING MASTD,R2                                                         
         MVC   BLOGO,MCVLOGO       SAVE A(LOGO)                                 
         MVC   BREMOT,MCVREMOT     SAVE A(REMOTE)                               
*                                                                               
         XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
         MVI   FORCEHED,C'Y'                                                    
         BRAS  RE,MYSPOOL                                                       
*                                                                               
         L     R2,BLOGOC                                                        
         USING LOGOD,R2                                                         
         MVI   LOGOTYPE,C'E'                                                    
         GOTO1 BLOGO,DMCB,(R2)     END REPORT LOGOS                             
*                                                                               
         GOTO1 VPRINT,DMCB,=C'CLOSE'                                            
*                                                                               
BPQ010   L     RF,BMASTD                                                        
         USING MASTD,RF                                                         
         L     R2,BREMOT                                                        
         USING REMOTED,R2                                                       
         MVC   REMOTABF,MCVPQBUF                                                
         MVC   REMOTADM,MCVDMGR                                                 
         MVC   REMOTAOP,MCVPQOPN                                                
         MVC   REMOTDST,MCDESTID                                                
         XC    MCALTREF,MCALTREF                                                
         MVI   REMOTCPY,C'1'                                                    
         MVI   REMOTCLS,C'G'          *** BDE, NEEDS TO BE CLASS G ***          
         MVC   REMOTJID,=C'THF'                                                 
*                                                                               
         MVI   FORCEHED,C'N'                                                    
*        BRAS  RE,MYSPOOL                                                       
         B     YES                                                              
*                                                                               
BMASTD   DS    A                   A(MASTER)                                    
BLOGOC   DS    A                   A(LOGOC)                                     
BLOGO    DS    A                   A(LOGO)                                      
BREMOT   DS    A                   A(REMOTE)                                    
*                                                                               
         DROP  R2,RF                                                            
         EJECT                                                                  
BDEHDR   NTR1  BASE=*,LABEL=*                                                   
*        MVC   THEMAIL,=CL35'GORDON.HO@DONOVANDATA.COM'                         
         CLC   THEMAIL,SPACES         NO EMAIL, SKIP BDE HEADER                 
         BNH   YES                                                              
*                                                                               
         TM    THSTAT2,THBDEHD                                                  
         BO    YES                                                              
         OI    THSTAT2,THBDEHD                                                  
         XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
*                                                                               
         MVC   P(40),=CL40'    *HDR*EDICT=*BDE               W'                 
         BRAS  RE,MYSPOOL          PRINT IT                                     
         MVC   P(30),=CL30'++DDS TATHFTRN'                                      
         BASR  RE,RF                                                            
         MVC   P(40),=CL40'++DDS      RCP '                                     
         MVC   P+15(L'THEMAIL),THEMAIL                                          
         BASR  RE,RF                                                            
         MVC   P(30),=CL30'++DDS      SUB BDE THF TEST'                         
         BASR  RE,RF                                                            
         MVC   P(30),=CL30'++DDS      FIL HOLDFEE'                              
         BASR  RE,RF                                                            
         MVC   P(30),=CL30'++DDS      EXT HTM'                                  
         BASR  RE,RF                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVC   SPECS,=A(MYSPECS)      SET A(SPECS) FOR HEADINGS                 
         MVC   HEADHOOK,=A(HDHOOK)                                              
         B     YES                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
BDEFOOT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   THEMAIL,SPACES         NO EMAIL, SKIP BDE FOOTER                 
         BNH   YES                                                              
         TM    THSTAT2,THBDEHD        BDE HEADER USED?                          
         BZ    YES                                                              
         TM    THSTAT,THPRNTED        SOME DETAIL PRINTED?                      
         BZ    YES                                                              
*                                                                               
BFOOT10  XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
*                                                                               
         MVI   FORCEHED,C'N'                                                    
         MVC   P(30),=CL30'*** END OF DDS MESSAGE ***'                          
         BRAS  RE,MYSPOOL          PRINT IT                                     
         NI    THSTAT2,X'FF'-THBDEHD                                            
         B     YES                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
*        PROCESS EMAIL AGY/CLI/PRD                                              
*======================================================================         
PRCEMAIL NTR1  BASE=*,LABEL=*                                                   
         TM    THOPTS,THBDE        BDE OPTION ON?                               
         BZ    PRCEZ                                                            
         MVC   THOFFNM(L'KEY),KEY  SAVE KEY TEMPORARILY                         
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A0',0)                                    
         BRAS  RE,GETEMAIL                                                      
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'A0',0)                                    
         BRAS  RE,GETEMAIL                                                      
         CLC   TGPRD,SPACES                                                     
         BNH   PRCEX                                                            
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'A0',0)                                    
         BRAS  RE,GETEMAIL                                                      
*                                                                               
PRCEX    MVC   KEY,THOFFNM                                                      
         GOTO1 HIGH                                                             
PRCEZ    B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*        GET EMAIL ADDRESS FROM RECORD                                          
*======================================================================         
GETEMAIL NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TACMELQ      VARIABLE COMMENT ELEMENT                     
         MVI   BYTE,TACMTYPI       EMAIL TYPE                                   
         GOTO1 GETL,DMCB,(1,BYTE)                                               
         BNE   NEWAX                                                            
         MVC   THEMAIL,SPACES                                                   
         L     R4,TGELEM                                                        
         USING TACMD,R4                                                         
         SR    R1,R1                                                            
         IC    R1,TACMLEN                                                       
         SH    R1,=Y(TACMLNQ)                                                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   THEMAIL(0),TACMCOMM                                              
NEWAX    B     YES                                                              
*======================================================================         
*        SAVE SIGNATORY FEE AND CAP INFORMATION                                 
*======================================================================         
         SPACE 2                                                                
         USING TANUD,R4                                                         
GETSIGN  NTR1  BASE=*,LABEL=*                                                   
         XC    THSGNINF,THSGNINF                                                
         SPACE                                                                  
         MVI   ELCODE,TANUELQ      GET SIGNATORY RATE                           
         GOTO1 GETL,DMCB,(1,=AL1(TANUTSGN))                                     
         BNE   GSIGNX                                                           
         L     R4,TGELEM                                                        
         ZIC   RF,TANULEN                                                       
         AHI   RF,-3                                                            
         GOTO1 CASHVAL,DMCB,TANUMBER,(RF)                                       
         CLI   0(R1),0                                                          
         BNE   GSIGNX                                                           
         MVC   THSGNRTE,6(R1)                                                   
         SPACE                                                                  
         GOTO1 GETL,DMCB,(1,=AL1(TANUTSCP))                                     
         BNE   GSIGNX             GET SIGNATORY FEE CAP                         
         L     R4,TGELEM                                                        
         ZIC   RF,TANULEN                                                       
         AHI   RF,-3                                                            
         GOTO1 CASHVAL,DMCB,TANUMBER,(RF)                                       
         CLI   0(R1),0                                                          
         BNE   GSIGNX                                                           
         MVC   THSGNCAP,4(R1)                                                   
GSIGNX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE CALCULATES TAX AND HANDLING                              
*---------------------------------------------------------------------          
         USING CASTD,R2            R2=A(CAST TABLE ENTRY)                       
TNHCALC  NTR1  BASE=*,LABEL=*                                                   
         XCEF  TMD,'TMLNQ'         CLEAR INTERFACE TO TALIM                     
         XC    XTRADSCT,XTRADSCT                                                
         LA    RF,XTRADSCT                                                      
         ST    RF,TMXTDSCT                                                      
*                                                                               
         TM    THOPTS,THTRACE      IF LOCAL TRACE SET                           
         BZ    *+8                                                              
         MVI   TMTRACE,C'Y'        SET TALIM TRACE                              
*                                                                               
         ST    RC,TMRC             A(GEND)                                      
         MVC   TMEFDTE0,TGTODAY0   EFFECTIVE DATE                               
         MVI   TMCURR,C'U'         CURRENCY                                     
         MVC   TMEMP,TGEMP         EMPLOYER                                     
         MVC   TMOFCDE,TGOFF       OFFICE                                       
         MVI   TMSTAT,TMSBIL       CALC. BASED ON BILLING RATES                 
         MVC   TMBILTYP,TGBTYPE    SET BILLING TYPE                             
         MVC   TMHNDRLS,TGHNDRLS   SET GLOBAL HANDLING RULES INFO               
         MVC   TMAPPLG,CASTAPPL                                                 
*                                                                               
         LA    RE,SYSCOMM                                                       
         ST    RE,TMASYCOM         A(SYSTEM ROUTINES)                           
*                                                                               
         CLI   TMHNDRLS,3          IF NOT GRT HANDLING TYPE 3                   
         BE    TNHCLC03                                                         
         CLI   TMHNDRLS,4          OR GRT HANDLING TYPE 4                       
         BE    TNHCLC03                                                         
         TM    THCASTAT,THCASCRP   HAS TO BE A CORPORATION                      
         BZ    TNHCLC05                                                         
TNHCLC03 OC    THGRTCD,SPACES                                                   
         CLC   THGRTCD,SPACES                                                   
         BNH   TNHCLC10                                                         
         GOTO1 =A(ISTYPE6)         SEE IF THIS IS A TYPE 6 GRT                  
         BNE   *+8                                                              
TNHCLC05 MVI   TMHNDRLS,X'FF'      DISABLE ADDITIONAL GRT HANDLING              
*                                                                               
TNHCLC10 TM    THCASTAT,THCASGST   IF GST # OF W4                               
         BZ    TNHCLC30                                                         
         CLC   =C'ACT',CASTUNI     AND UNION IS ACTRA                           
         BE    TNHCLC20                                                         
         CLC   =C'UDA',CASTUNI     OR UDA                                       
         BE    TNHCLC20                                                         
         CLC   =C'NON',CASTUNI     OR NON UNION AND CANADA LOCAL                
         BNE   TNHCLC30                                                         
         CLC   =C'CAN',CASTLCL                                                  
         BNE   TNHCLC30                                                         
TNHCLC20 OI    TMSTAT2,TMSGSTU     CALCULATE US DOLLAR GST                      
*                                                                               
TNHCLC30 LA    R1,TGBSRC           MOVE HALFWORD BILLING RATES TO               
         LA    RF,TMBRATES         FULLWORD BILLING RATES IN TALIMD             
         LA    R0,TMRATEN          R0=N'BILLING RATES                           
         MVC   2(2,RF),0(R1)                                                    
         LA    R1,2(R1)            BUMP TO NEXTS                                
         LA    RF,4(RF)                                                         
         BCT   R0,*-14             AND LOOP                                     
*                                                                               
         OC    ADRFUTA(L'TABRRATE),ADRFUTA  ANY RATES TO ACCUMULATE?            
         BZ    TNHCLC37                     NO, LEAVE                           
         MVC   TMBILTYP,ADRBTYPE                                                
         LA    RE,TMRATEN          N'BILLING RATES                              
         LA    R3,TMBRATES                                                      
         LA    R4,ADRFUTA                                                       
         XR    R1,R1                                                            
TNHCLC35 L     RF,0(R3)            ACCUMULATE THE RATES                         
         ICM   R1,3,0(R4)                                                       
         AR    RF,R1                                                            
         ST    RF,0(R3)                                                         
*                                                                               
         AHI   R3,4                BUMP TO NEXT RATES                           
         AHI   R4,2                                                             
         BCT   RE,TNHCLC35                                                      
*                                                                               
TNHCLC37 CLI   TCW4TYPE,TAW4TYCA   IF CANADIAN                                  
         BE    *+12                                                             
         CLI   TCW4TYPE,TAW4TYCO   OR CORPORATION                               
         BNE   TNHCLC40                                                         
         CLC   CASTSOW,=C'CN '     AND CANADIAN WORK                            
         BE    TNHCLC39                                                         
         MVC   TGCTRY,=C'CA'                                                    
         GOTO1 TAXVAL,DMCB,(X'FF',CASTSOW)                                      
         BE    TNHCLC39                                                         
         CLC   CASTCAT,=C'ZZZ'     OR CATEGORY IS ZZZ                           
         BNE   TNHCLC40                                                         
TNHCLC39 XC    TMBRWCRP,TMBRWCRP   DON'T CALCULATE WORKMEN'S COMP               
                                                                                
*                                  INITIALIZE CAST-SPECIFIC FIELDS              
TNHCLC40 MVC   TMSSN,TGSSN         S/S NUMBER                                   
         MVC   TMW4TYPE,TCW4TYPE   W4 TYPE                                      
         BAS   RE,SETSOW                                                        
         MVC   TMUNIT,CASTSOW      STATE OF WORK                                
         MVC   TMWUNIT,CASTSOW     STATE OF WORK                                
         BRAS  RE,SETTAXBL         SET TAXABLE STATE                            
         MVC   TMTXEARN,CASTPAYI   TAXABLE EARNINGS                             
         L     R1,CASTPAYC         NON-TAXABLE EARNINGS                         
**NO-OP**A     R1,CASTINR          6/22/99 AS PER IRV                           
         CLI   TGBTYPE,TABRTY20                                                 
         BNE   *+8                                                              
         A     R1,CASTINR                                                       
         ST    R1,TMNTEARN                                                      
         MVC   TMPNH,CASTPNH       P&H (NEEDED FOR SOME BILL TYPES)             
*                                                                               
         BRAS  RE,GETYTD           GET YTD FOR THIS S/S NUMBER                  
* 2010 jun30 - commented out cuz GST is supposed to include I&R                 
*        CLI   TMCURR,C'C'         IF THIS IS NOT CANADIAN $ INVOICE            
*        BNE   *+10                                                             
         OC    CASTINR,CASTINR                                                  
         BZ    *+10                                                             
         MVC   TMINR,CASTINR       SET INR FOR TALIM CALCULATIONS               
*                                                                               
         XC    TMREXP,TMREXP                                                    
*                                                                               
         GOTO1 =A(GETPSTR),DMCB,CASTSOW    GET PST RATE                         
         GOTO1 =V(TALIM),DMCB,TMD  CALC. TAX AND HANDLING                       
*                                                                               
         GOTOR MYTRACE2,DMCB,=C'FROM TALIM BLOCK',TMLNQ,TMD                     
*                                                                               
         L     R1,THTAXES          ACCUMULATE PAYROLL TAXES                     
         A     R1,TMXTOTAL                                                      
         TM    TGSYSTA2,TASYSNCS   ALLOW TO SUI SURCHARGE IT?                   
         BZ    TNHCLC45                                                         
         A     R1,TMSUISUR         ADD IN SUI SURCHARGE                         
TNHCLC45 CLI   TMBILTYP,TABRTY1    IF BILLING TYPE 1,6,7,8 OR 20                
         BE    TNHCLC50                                                         
         CLI   TMBILTYP,TABRTY6                                                 
         BE    TNHCLC50                                                         
         CLI   TMBILTYP,TABRTY8                                                 
         BE    TNHCLC50                                                         
         CLI   TMBILTYP,TABRTY16                                                
         BE    TNHCLC50                                                         
         CLI   TMBILTYP,TABRTY18                                                
         BE    TNHCLC50                                                         
         CLI   TMBILTYP,TABRTY23                                                
         BE    TNHCLC50                                                         
         CLI   TMBILTYP,TABRTY20                                                
         BE    TNHCLC50                                                         
         CLI   TMBILTYP,TABRTY7                                                 
         BE    TNHCLC50                                                         
         CLI   TMBILTYP,TABRTY21                                                
         BE    TNHCLC50                                                         
         CLI   TMBILTYP,TABRTY11                                                
         BE    TNHCLC50                                                         
         CLI   TMBILTYP,TABRTY25                                                
         BE    TNHCLC50                                                         
         CLI   TMBILTYP,TABRTY26                                                
         BE    TNHCLC50                                                         
         BNE   *+14                                                             
TNHCLC50 S     R1,TMXFICR          SUBTRACT FICA CREDITS FROM TAXES             
TNHCLC60 XC    TMXFICR,TMXFICR     AND DON'T ACCUM THEM SEPARATELY              
         ST    R1,THTAXES                                                       
*                                                                               
         L     R1,THHNDI           ACCUMULATE INDIVIDUAL HANDLING               
         A     R1,TMOHAND                                                       
         ST    R1,THHNDI                                                        
*                                                                               
         L     R1,THFICR           ACCUMULATE FICA CREDITS                      
         S     R1,TMXFICR                                                       
         ST    R1,THFICR                                                        
*                                                                               
         L     R1,THHNDC           ACCUMULATE CORP HANDLING                     
         A     R1,TMOCORP                                                       
         A     R1,TMOCAN                                                        
         ST    R1,THHNDC                                                        
*                                                                               
         L     R1,THGSTU           ACCUMULATE US DOLLAR GST FOR ACTRA           
         A     R1,TMOGSTU                                                       
         A     R1,TMOPST           AND PST                                      
         ST    R1,THGSTU                                                        
*                                                                               
         TM    TGSYSTA2,TASYSNCS   ALLOW TO SUI SURCHARGE IT?                   
         BZ    TNHCLCX                                                          
         L     RE,THSURTOT                                                      
         A     RE,TMSUISUR         ADD SUI SURCHARGE TO SURCHARGE TOTAL         
         ST    RE,THSURTOT                                                      
TNHCLCX  XIT1                                                                   
                                                                                
XTRADSCT DS    CL256                                                            
         EJECT                                                                  
*====================================================================           
*              SPECIAL HANDLING FOR GUARANTEE APPLIED                           
*====================================================================           
*&&DO                                                                           
SPCLHAND NTR1                                                                   
         CLI   TGOFF,C'7'          OFFICE 7 / G ONLY                            
         BE    *+8                                                              
         CLI   TGOFF,C'G'                                                       
         BNE   XIT                                                              
*                                                                               
         CLI   TMBILTYP,TABRTY7    AND FOR TYPES 7 AND 18                       
         BE    *+8                                                              
         CLI   TMBILTYP,TABRTY18                                                
         BNE   XIT                                                              
*                                                                               
         OC    CASTAPPL,CASTAPPL   MUST NOT BE ZERO                             
         BZ    XIT                                                              
         SR    R0,R0                                                            
         L     R1,CASTAPPL                                                      
         LCR   R1,R1               REVERSE THE SIGN                             
         CLI   TMBILTYP,TABRTY18                                                
         BNE   *+8                                                              
         A     R1,TCPNH            ADD IN P&H ALSO FOR TYPE 18                  
*                                                                               
         M     R0,TMBRHAND         HANDLING RATE                                
         CVD   R1,DUB              PACKED NUMBER                                
         SRP   DUB,60,5            DIVIDE BY 1000 AND ROUND                     
         CVB   R1,DUB                                                           
*                                                                               
         L     RF,TMOHAND                                                       
         AR    RF,R1                                                            
         ST    RF,TMOHAND                                                       
*                                                                               
         B     XIT                                                              
*&&                                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
*              ROUTINE SETS THE CORRECT SOW IN CASE IT'S A CITY                 
*              (SEE BILLING)                                                    
*----------------------------------------------------------------------         
         USING CASTD,R2            R2=A(CAST TABLE ENTRY)                       
SETSOW   NTR1                                                                   
         CLI   TCW4TYPE,TAW4TYCO   DON'T DO IF CORP                             
         BE    TNHCLCX                                                          
         CLI   TCW4TYPE,TAW4TYCA   OR CANADIAN                                  
         BE    TNHCLCX                                                          
         CLI   TCW4TYPE,TAW4TYFO   OR FOREIGNER                                 
         BE    TNHCLCX                                                          
         CLI   CASTSOW+2,C' '      IF THIS IS A CITY                            
         BNH   TNHCLCX                                                          
         GOTO1 TAXVAL,DMCB,(3,CASTSOW)                                          
         MVC   CASTSOW,TGTASTCY     FIND THE STATE IT BELONGS TO                
         B     TNHCLCX                                                          
         EJECT                                                                  
*======================================================================         
*        SEE IF GUARANTEE IS A TYPE 6 GUARANTEE                                 
*======================================================================         
ISTYPE6  NTR1                                                                   
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLGUD,R4                                                         
         MVI   TLGUCD,TLGUCDQ            GUARANTEE RECORD                       
         MVC   TLGUSSN,TGSSN             SOCIAL SECURITY NUMBER                 
         MVC   TLGUGUA,THGRTCD           GRT CODE                               
         XC    TLGUGUA,=X'FFFFFFFF'      COMPLEMENT IT                          
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING TAGUD,R4                                                         
         MVI   ELCODE,TAGUELQ      GRT ELEMENT                                  
         BRAS  RE,GETEL                                                         
         BNE   ISTYPE6N                                                         
         OC    TAGUCOM,TAGUCOM     COMMERCIAL SET, TYPE 6 GRT                   
         BZ    ISTYPE6N                                                         
*                                                                               
         CR    RB,RB                                                            
         B     *+6                                                              
ISTYPE6N LTR   RB,RB                                                            
ISTYPE6X MVC   AIO,AIO1                                                         
         B     TNHCLCX                                                          
*                                                                               
         LTORG                                                                  
         DROP   R3,R4                                                           
         EJECT                                                                  
*              ROUTINE TO OPEN MQ                                               
         SPACE 1                                                                
OPENMQ   NTR1  BASE=*,LABEL=*                                                   
         CLC   AGYALPHA,=C'D3'     ALWAYS OPEN IF ON FQA                        
         BE    OPENMQ10                                                         
         SPACE 1                                                                
         CLI   THRERUN,C'Y'        ELSE EXIT IF RERUNNING                       
         BE    OPENMQX                                                          
         CLI   THWRITE,C'N'        OR IF WRITE=N                                
         BE    OPENMQX                                                          
OPENMQ10 TM    THSTAT2,THOPENMQ    OR IF MQ ALREADY OPENED                      
         BO    OPENMQX                                                          
         SPACE 1                                                                
         CLI   RECNUM,SN           EXIT IF NOT SECOND NOTICES                   
         BE    *+12                                                             
         BRAS  RE,HFORUCHK         OR HOLDING FEES OR UPDATE                    
         BNE   OPENMQX                                                          
         SPACE 1                                                                
         BRAS  RE,WEBCHK           EXIT IF NOT SENDING TO MQ                    
         BNE   OPENMQX                                                          
         TM    THSTAT2,THMQEROR    OR IF MQ RETURNED ERROR PREVIOUSLY           
         BO    OPENMQX                                                          
         SPACE 1                                                                
         BRAS  RE,OPENWF           MAY NEED TO OPEN BACKUP FILE                 
         SPACE 1                                                                
         MVC   WORK(16),=C'TALHOLD*********'                                    
         CLC   TGUSER,=H'2276'                                                  
         BNE   *+10                                                             
         MVC   WORK(3),=C'TST'                                                  
         CLC   TGUSER,=H'7698'                                                  
         BNE   *+10                                                             
         MVC   WORK(3),=C'TST'                                                  
         CLC   TGUSER,=H'7538'                                                  
         BNE   *+10                                                             
         MVC   WORK(3),=C'FQA'                                                  
         CLC   TGUSER,=H'7697'                                                  
         BNE   *+10                                                             
         MVC   WORK(3),=C'FQA'                                                  
         SPACE 1                                                                
         GOTO1 VMQRPT,DMCB,(0,=C'OPEN'),(0,WORK),(X'C0',0),0                    
         MVC   WORK(L'HFOPEN),HFOPEN                                            
         BRAS  RE,MQEMSG                                                        
         SPACE 1                                                                
         TM    THSTAT2,THMQEROR    IF OPEN WAS SUCCESSFUL                       
         BO    OPENMQX                                                          
         OI    THSTAT2,THOPENMQ    TURN ON OPENED MQ STATUS                     
         SPACE 1                                                                
         LA    R3,TAPEREC          PUT OPENING RECORDS TO MQ                    
         BRAS  RE,CLRREC                                                        
         MVC   0(L'DMESSAGE,R3),DMESSAGE                                        
         LHI   RE,L'DMESSAGE                                                    
         STH   RE,PFILELEN                                                      
         BRAS  RE,PUTMQ                                                         
         SPACE 1                                                                
         BRAS  RE,CLRREC                                                        
         MVC   0(L'DHEAD1,R3),DHEAD1                                            
         MVC   L'DHEAD1(L'DHEAD2,R3),DHEAD2                                     
         MVC   L'DHEAD1+L'DHEAD2(L'DHEAD3,R3),DHEAD3                            
         LHI   RE,L'DHEAD1+L'DHEAD2+L'DHEAD3                                    
         STH   RE,PFILELEN                                                      
         BRAS  RE,PUTMQ                                                         
OPENMQX  XIT1                                                                   
         SPACE 2                                                                
HFOPEN   DC    CL15'MQ OPEN FAILED'                                             
         SPACE 2                                                                
DMESSAGE DC    C'<dmessage>'                                                    
DHEAD1   DC    C'<dhead><doctypeId>holding fee</doctypeId><sourceId>'           
DHEAD2   DC    C'mainframe</sourceId><formatId>fixed-length text'               
DHEAD3   DC    C'</formatId></dhead>'                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PUT RECORD TO BACKUP WEB FILE AND MQ                  
         SPACE 1                                                                
         USING RECD,R3                                                          
PUTMQ    NTR1  BASE=*,LABEL=*                                                   
         LA    R3,TAPEREC        IF CLOSING MQ, ALWAYS PUT MESSAGE              
         CLC   =C'</dmessage>',RECTYP    TO MQ (AND NEVER TO FILE)              
         BE    PUTMQ10                                                          
         SPACE 1                                                                
         BRAS  RE,WEBCHK         EXIT IF THIS HOLDING FEE SHOULD                
         BNE   PUTMQX            NOT GO TO WEB                                  
         SPACE 1                                                                
         BRAS  RE,BLDMHD2        ADD MORE INFO TO MORE HEADING REC              
         SPACE 1                                                                
         CLC   =C'<dmessage>',RECTYP    NEVER SEND OPENING MQ RECORDS           
         BE    PUTMQ10                  TO WEB FILE                             
         CLC   =C'<dhead><doctypeId>',RECTYP                                    
         BE    PUTMQ10                                                          
         SPACE 1                                                                
         MVI   TRSTAT,0          ATTACH STATUS                                  
         EDIT  PFILELEN,TRLENGTH AND LENGTH TO WEB FILE ENTRY                   
         BRAS  RE,PUTWEBF        PUT TO BACKUP WEB FILE                         
         SPACE 1                                                                
PUTMQ10  TM    THSTAT2,THOPENMQ  EXIT IF MQ HAS NOT BEEN OPENED                 
         BNO   PUTMQX                                                           
         SPACE 1                                                                
         TM    THSTAT2,THMQEROR  IF MQ RETURNED ERROR PREVIOUSLY                
         BO    PUTMQX            DO NOT TRY TO PUT TO MQ AGAIN                  
         SPACE 1                                                                
         TM    THSTAT2,THMQEROR                                                 
         BO    PUTMQX                                                           
         LH    RF,PFILELEN                                                      
         GOTO1 VMQRPT,DMCB,(0,=C'PUT'),TAPEREC,(RF),0                           
         MVC   WORK(L'HFPUT),HFPUT                                              
         BRAS  RE,MQEMSG             AND IF RUNNING SOON, DUMP                  
PUTMQX   XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
HFPUT    DC    CL15'MQ PUT FAILED'                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CLOSE MQ                                              
         SPACE 1                                                                
CLOSEMQ  NTR1  BASE=*,LABEL=*                                                   
         TM    THSTAT2,THOPENMQ  IF MQ IS OPEN                                  
         BNO   CMQ10                                                            
         LA    R3,TAPEREC        PUT END OF HOLDING FEE RECORD                  
         BRAS  RE,CLRREC                                                        
         MVC   0(11,R3),=C'</dmessage>'                                         
         LHI   RE,11                                                            
         STH   RE,PFILELEN                                                      
         BRAS  RE,PUTMQ                                                         
         SPACE 1                                                                
CMQ10    GOTO1 DATAMGR,DMCB,=C'COMMIT'  COMMIT                                  
         SPACE 1                                                                
         TM    THSTAT2,THOPENWF    IF WEB FILE IS OPEN                          
         BZ    CMQ20                                                            
         LA    R3,TAPEREC          PUT COMMIT MESSAGE TO BACKUP                 
         BRAS  RE,CLRREC           WEB FILE                                     
         MVI   TRSTAT,C'X'                                                      
         XC    TRLENGTH,TRLENGTH                                                
         MVC   TAPEREC(L'WFCOMMIT),WFCOMMIT                                     
         BRAS  RE,PUTWEBF                                                       
         SPACE 1                                                                
CMQ20    MVC   CMCOUNT,THCOMMIT  RESET NUMBER OF HFEES PER COMMIT               
         SPACE 1                                                                
         TM    THSTAT2,THOPENMQ  EXIT IF MQ IS NOT OPEN                         
         BNO   CMQX                                                             
         TM    THSTAT2,THMQEROR  IF MQ RETURNED ERROR PREVIOUSLY                
         BO    CMQX              DO NOT TRY TO CLOSE IT AGAIN                   
         SPACE 1                                                                
         GOTO1 VMQRPT,DMCB,(0,=C'CLOSE'),0,0,0 CLOSE THE MQ                     
         MVC   WORK(L'HFCLOSE),HFCLOSE                                          
         SPACE 1                                                                
         TM    THSTAT2,THMQEROR  IF MQ CLOSE RETURNED ERROR                     
         BO    CMQX              DO NOT TRY TO CLOSE IT AGAIN                   
         SPACE 1                                                                
         LA    R3,TAPEREC        PUT MQ CLOSED MESSAGE TO BACKUP                
         BRAS  RE,CLRREC         WEB FILE                                       
         MVI   TRSTAT,C'X'                                                      
         XC    TRLENGTH,TRLENGTH                                                
         MVC   TAPEREC(L'WFMQCLOS),WFMQCLOS                                     
         BRAS  RE,PUTWEBF                                                       
         SPACE 1                                                                
         NI    THSTAT2,X'FF'-THOPENMQ                                           
CMQX     XIT1                                                                   
         SPACE 2                                                                
WFMQCLOS DC    C'MQ SUCCESSFULLY CLOSED'                                        
HFCLOSE  DC    CL15'MQ CLOSE FAILED'                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CHECK IF INFORMATION SHOULD GO TO                     
*              BACKUP WEB FILE AND MQ                                           
                                                                                
WEBCHK   NTR1  BASE=*,LABEL=*                                                   
         CLC   TGUSER,=H'15778'    IF USER-ID IS TPTPC                          
         BE    WCHKNO                                                           
         CLC   TGUSER,=H'15777'    OR USER-ID IS TPCLI                          
         BE    WCHKNO              NEVER GO TO WEB                              
                                                                                
         CLI   RECNUM,SN           ONLY SECOND NOTICES,                         
         BE    WCHK10                                                           
         BRAS  RE,HFORUCHK         HOLDING FEES AND HOLDING FEE UPDATE          
         BNE   WCHKNO              SHOULD EVER WRITE TO WEB FILE                
                                                                                
WCHK10   TM    THHLDPEB,TAAYHSPO   PAPER ONLY AGENCIES AND CLIENTS              
         BO    WCHKNO              NEVER GO TO THE WEB                          
                                                                                
         TM    WHEN,X'20'          IF NOT RUNNING SOON                          
         BZ    WCHKYES             ALWAYS PUT INFORMATION TO WEB                
                                                                                
         TM    THOPTS,THWEBIT      IF RUNNING SOON WITHOUT WEBIT OPTION         
         BZ    WCHKNO              DO NOT SEND TO WEB                           
                                                                                
WCHKYES  XR    RC,RC                                                            
WCHKNO   LTR   RC,RC                                                            
         XIT1                                                                   
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CHECK IF TALENT FILE SHOULD BE UPDATED                
*              BY THIS ACTION                                                   
         SPACE 1                                                                
UPDCHK   NTR1  BASE=*,LABEL=*                                                   
         CLI   RECNUM,SN           ONLY SECOND NOTICES,                         
         BE    UCHK10                                                           
         BRAS  RE,HFORUCHK         HOLDING FEES AND HOLDING FEE UPDATE          
         BNE   UCHKNO              SHOULD EVER WRITE TO FILE                    
         SPACE 1                                                                
UCHK10   TM    WHEN,X'20'          IF NOT RUNNING SOON                          
         BZ    UCHKYES             ALWAYS UPDATE FILE                           
         TM    THHLDPEB,TAAYHSPO   SOONS FOR PAPER ONLY AGENCIES                
         BO    UCHKNO              NEVER UPDATE FILE                            
         TM    THOPTS,THWEBIT      ELSE IF RUNNING SOON WITHOUT WEBIT           
         BZ    UCHKNO              OPTION, DO NOT UPDATE FILE                   
         SPACE 1                                                                
UCHKYES  XR    RC,RC                                                            
UCHKNO   LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CHECKS TO SEE IF RUN IS NON-SOON                         
*              HOLDING FEES OR HOLDING FEE UPDATE                               
         SPACE 1                                                                
HFORUSCK NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,HFORUCHK       HOLDING FEES AND HOLDING FEES UPDATE           
         BNE   HUSCKNO                                                          
         TM    WHEN,X'20'        RUN OVERNIGHT                                  
         BO    HUSCKNO                                                          
HUSCKYES XR    RC,RC             RETURN POSITIVE CONDITION CODE                 
HUSCKNO  LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
*              ROUTINE CHECKS TO SEE IF RUN IS HOLDING FEES                     
*              OR HOLDING FEE UPDATE                                            
         SPACE 1                                                                
HFORUCHK NTR1  BASE=*,LABEL=*                                                   
         CLI   RECNUM,HF         HOLDING FEES                                   
         BE    HUCHKYES                                                         
         CLI   RECNUM,HC         AND HOLDING FEES UPDATE                        
         BNE   HUCHKNO                                                          
HUCHKYES XR    RC,RC             RETURN POSITIVE CONDITION CODE                 
HUCHKNO  LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PUT DOCUMENT HEADERS TO WEB FILE                      
*              (BEGINNING OF WRAP FOR EACH HOLDING FEE)                         
*                                                                               
*              (FIRST RECORD FOR EACH HOLDING FEE CONTAINS UNIQUE               
*               HOLDING FEE ID AND WHETHER OR NOT IT WAS PRINTED)               
         SPACE 1                                                                
PUTDOCHD NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,WEBCHK         EXIT IF NOT GENERATING WEB FILE                
         BNE   PDHX                                                             
         SPACE 1                                                                
         USING RECD,R3                                                          
         LA    R3,TAPEREC                                                       
         BRAS  RE,CLRREC                                                        
         SPACE 1                                                                
         MVC   0(L'DOCID1,R3),DOCID1                                            
         GOTO1 HEXOUT,DMCB,TGCOM,L'DOCID1(R3),8,0                               
         GOTO1 DATCON,DMCB,(1,TCPCYCS),(20,L'DOCID1+8(R3))                      
         MVC   L'DOCID1+16(L'DOCID2,R3),DOCID2                                  
         MVC   L'DOCID1+16+L'DOCID2(L'DOCID3F,R3),DOCID3F                       
         SPACE 1                                                                
         LR    RE,R3                                                            
         AHI   RE,L'DOCID1-2     INDICATE IF THIS IS FIRST NOTICE               
         MVC   THDOCID,0(RE)                                                    
         TM    THSTAT,THSECOND   OR SECOND NOTICE                               
         BZ    *+10                                                             
         MVC   L'DOCID1+16+L'DOCID2(L'DOCID3S,R3),DOCID3S                       
         SPACE 1                                                                
         LHI   RE,90                                                            
         STH   RE,PFILELEN                                                      
         BRAS  RE,PUTMQ          PUT TO WEB FILE                                
         SPACE 1                                                                
         LA    R3,TAPEREC                                                       
         BRAS  RE,CLRREC                                                        
         SPACE 1                                                                
         MVI   RECTYP,RECTYPD    INDICATE DOC ID RECORD                         
         LHI   RE,RECDLNQ                                                       
         STH   RE,PFILELEN       AND LENGTH                                     
         SPACE 1                                                                
         MVC   RECDOCID,THDOCID  PUT UNIQUE HOLDING FEE ID                      
         SPACE 1                                                                
         MVI   RECPRNT,C'Y'      PUT WHETHER PRINTED OR NOT                     
         TM    THHLDPEB,TAAYHSEL                                                
         BZ    *+8                                                              
         MVI   RECPRNT,C'N'                                                     
         BRAS  RE,PUTMQ          PUT TO WEB FILE                                
PDHX     XIT1                                                                   
         SPACE 2                                                                
DOCID1   DC    C'<ddoc externalDocId="HF'                                       
DOCID2   DC    C'" externalVersionId="'                                         
DOCID3F  DC    C'first notice"><!CDATA'                                       
DOCID3S  DC    C'second notice"><!CDATA'                                      
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PUT HOLDING FEE HEADING RECORDS TO                    
*              BACKUP WEB FILE AND MQ                                           
         SPACE 1                                                                
PUTHEADS NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,PUTDOCHD       PUT DOC ID REC TO BACKUP FILE AND MQ           
         SPACE 1                                                                
         BRAS  RE,BLDHEAD        BUILD HEADING RECORD                           
         BRAS  RE,PUTMQ          PUT RECORD TO BACKUP WEB FILE AND MQ           
         SPACE 1                                                                
         BRAS  RE,BLDMHEAD       BUILD MORE HEADINGS RECORD                     
         BRAS  RE,PUTMQ          PUT RECORD TO BACKUP WEB FILE AND MQ           
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              BUILD HEADINGS (TYPE H) RECORD                                   
         SPACE 1                                                                
         USING RECD,R3                                                          
BLDHEAD  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,TAPEREC                                                       
         BRAS  RE,CLRREC                                                        
         SPACE 1                                                                
         MVI   RECTYP,RECTYPH      INDICATE HEADING RECORD                      
         LHI   RE,RECHLNQ                                                       
         STH   RE,PFILELEN         AND LENGTH                                   
         SPACE 1                                                                
         LA    R1,THAGYNM          PRINT AGENCY NAME                            
         CLI   THATTNM,C' '                                                     
         BNH   *+8                                                              
         LA    R1,THATTNM          OR ATTENTION NAME OVERRIDE                   
         MVC   RECAGYN,0(R1)                                                    
         SPACE 1                                                                
         LA    RF,TH1STAD          PRINT ADDRESS                                
         LA    R0,THNADDS                                                       
P1H10    CLI   0(RF),C' '                                                       
         BNH   *+6                                                              
         LR    R2,RF                                                            
         LA    RF,L'THADDRS(RF)                                                 
         BCT   R0,P1H10                                                         
         GOTO1 CHOPPER,DMCB,(120,(R2)),(120,RECADDR),(C'P',1)                   
         SPACE 1                                                                
         LA    RF,TH1STATT         PRINT ATTENTION NAME                         
         LA    R0,THNATTS                                                       
         B     *+12                                                             
P1H20    CLI   0(RF),C' '                                                       
         BNH   *+6                                                              
         LR    R2,RF                                                            
         LA    RF,L'THATTNMS(RF)                                                
         BCT   R0,P1H20                                                         
         CLC   0(L'THATTNMS,R2),SPACES                                          
         BNH   *+10                                                             
         MVC   RECATTN,0(R2)                                                    
         SPACE 1                                                                
         MVC   RECCLIN(L'RECCLIN+L'RECPRDN+L'RECCOMN),THCLINM                   
         OC    RECCLIN(L'RECCLIN+L'RECPRDN+L'RECCOMN),SPACES                    
         XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              BUILD MORE HEADINGS (TYPE M) RECORD                              
         SPACE 1                                                                
         USING RECD,R3                                                          
BLDMHEAD NTR1  BASE=*,LABEL=*                                                   
         LA    R3,TAPEREC                                                       
         BRAS  RE,CLRREC                                                        
         SPACE 1                                                                
         MVI   RECTYP,RECTYPM    INDICATE MORE HEADINGS RECORD                  
         LHI   RE,RECMLNQ        AND LENGTH                                     
         STH   RE,PFILELEN                                                      
         SPACE 1                                                                
         MVC   RECCID,TGCID      PRINT COMMERCIAL ID                            
         SPACE 1                                                                
         MVC   RECFDAT(THFILNQ+THRELNQ),THFIDATE                                
         SPACE 1                                                                
         MVC   RECEMP,THEMPNM    PRINT EMPLOYER NAME                            
         MVC   RECAGY,TGAGY      AGENCY                                         
         MVC   RECCLI,TGCLI      CLIENT                                         
         MVC   RECPRD,TGPRD      PRODUCT CODE                                   
         MVC   RECTPC,THTPC      TPC STAFF                                      
         OC    RECPRD(L'RECPRD+L'RECTPC),SPACES                                 
         MVC   RECFAIR,THCOAIR   FIRST AIR DATE                                 
         MVC   RECFCYC,THCOFCYC  FIRST FIXED CYCLE DATE                         
         MVC   RECEXP,THCOEXP    EXPIRATION DATE                                
         MVC   RECCLEN,THCOLEN   LENGTH                                         
         MVC   RECMED,TGMENAME   MEDIA NAME                                     
         SPACE 1                                                                
         CLC   THLID,SPACES      IF LIFT EXISTS                                 
         BE    *+10                                                             
         MVC   RECLIFT(L'RECLIFT+L'RECLLEN),THLID                               
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(X'11',TCPCYC),(8,RECCYC)                            
         XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD ONTO MORE HEADINGS (TYPE M) RECORD                
         SPACE 1                                                                
         USING RECD,R3                                                          
BLDMHD2  NTR1  BASE=*,LABEL=*                                                   
         CLI   RECTYP,RECTYPM    IF RECORD TYPE IS "MORE HEADINGS"              
         BNE   BMHX              NEED TO ADD MORE INFORMATION NOW               
         SPACE 1                                                                
         MVC   TGDUB,THFIDATE                                                   
         BRAS  RE,CVTDTE20                                                      
         MVC   RECFDAT,TGDUB                                                    
         SPACE 1                                                                
         MVC   TGDUB,THREDATE                                                   
         BRAS  RE,CVTDTE20                                                      
         MVC   RECRDAT,TGDUB                                                    
         SPACE 1                                                                
         MVC   TGDUB,THCOAIR                                                    
         BRAS  RE,CVTDTE20                                                      
         MVC   RECFAIR,TGDUB                                                    
         SPACE 1                                                                
         MVC   TGDUB,THCOFCYC                                                   
         BRAS  RE,CVTDTE20                                                      
         MVC   RECFCYC,TGDUB                                                    
         SPACE 1                                                                
         MVC   TGDUB,THCOEXP                                                    
         BRAS  RE,CVTDTE20                                                      
         MVC   RECEXP,TGDUB                                                     
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(1,TCPCYCS),(20,RECCYCS)                             
         GOTO1 DATCON,DMCB,(1,TCPCYCE),(20,RECCYCE)                             
         SPACE 1                                                                
         MVC   RECCTYPE,THCOTYPE                                                
         MVC   RECSPCY,THCOSPCY                                                 
         MVC   RECPTYPE,=C'SHL'                                                 
         CLI   RECCTYPE,CTYSPAN                                                 
         BE    BMHX                                                             
         MVC   RECPTYPE,=C'ADH'                                                 
         CLI   RECCTYPE,CTYADD                                                  
         BE    BMHX                                                             
         MVC   RECPTYPE,=C'HLD'                                                 
         OC    RECCTYPE(L'RECCTYPE+L'RECPTYPE+L'RECSPCY),SPACES                 
         MVC   RECOFFNM,THOFFNM  OFFICE NAME                                    
         MVC   RECOFFAD,THOFFAD  OFFICE ADDRESS                                 
         GOTO1 DATCON,DMCB,(1,TGTODAY1),(20,RECRUNDT)                           
         TIME  DEC                                                              
         STCM  R0,12,HALF                                                       
         GOTO1 HEXOUT,DMCB,HALF,RECRUNTM,L'HALF                                 
BMHX     XIT1                                                                   
         DROP R3                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO UPDATE COMMERCIAL RECORD                              
         SPACE 1                                                                
         USING SORTD,R3                                                         
UPDCOMM  NTR1  BASE=*,LABEL=*                                                   
         USING TLCOPD,R4                                                        
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,TGCOM                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(TLCOCCOM+L'TLCOCCOM-TLCOPKEY),KEYSAVE                        
         BNE   UCOMX                                                            
         MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'Y'     NOT READY FOR WEB YET                          
         GOTO1 GETREC                                                           
         DROP  R4                                                               
         SPACE 1                                                                
         BRAS  RE,PUTVER         PUT COMMERCIAL VERSIONS TO MQ                  
         SPACE 1                                                                
         CLC   LSTUPDCM,TGCOM    IF COMMERCIAL HAS ALREADY BEEN                 
         BE    UCOM60            UPDATED, DON'T UPDATE AGAIN                    
         GOTOR MYTRACE,DMCB,=C'GET COM'                                         
         SPACE 1                                                                
         USING TACOD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ    GET COMMERCIAL DETAILS ELEMENT                 
         BRAS  RE,GETEL                                                         
         BNE   UCOMX                                                            
         MVI   TGBYTE,C'N'       ASSUME IT DOESN'T HAVE TO BE PUTREC'D          
         SPACE 1                                                                
         TM    TACOSTA2,TACOCHHF IF NEED TO TURN OFF CHANGED SINCE              
         BZ    UCOM10            LAST HF STATUS ...                             
         NI    TACOSTA2,X'FF'-TACOCHHF              DO SO                       
         MVI   TGBYTE,C'Y'       AND MARK THAT COMM'L MUST BE PUTREC'D          
         SPACE 1                                                                
UCOM10   TM    TACOSTA2,TACOHFUP IF LAST RUN WAS HFUPDATE                       
         BZ    UCOM20                                                           
         CLI   RECNUM,HC         AND THIS RUN IS NOT HFUPDATE ...               
         BE    UCOM30                                                           
         NI    TACOSTA2,X'FF'-TACOHFUP ... TURN OFF HFUPDATE STATUS             
         MVI   TGBYTE,C'Y'       AND MARK THAT COMM'L MUST BE PUTREC'D          
         B     UCOM30                                                           
         SPACE 1                                                                
UCOM20   CLI   RECNUM,HC         IF LAST RUN WAS NOT HFUPDATE                   
         BNE   UCOM30            AND THIS RUN IS HFUPDATE                       
         OI    TACOSTA2,TACOHFUP TURN ON HFUPDATE STATUS                        
         MVI   TGBYTE,C'Y'       AND MARK THAT COMM'L MUST BE PUTREC'D          
         SPACE 1                                                                
UCOM30   TM    TACOSTA2,TACOLHFS IF LAST RUN WAS SOON                           
         BZ    UCOM40                                                           
         TM    WHEN,X'20'        AND THIS RUN IS NOT SOON ...                   
         BO    UCOM50                                                           
         NI    TACOSTA2,X'FF'-TACOLHFS ... TURN OFF SOON STATUS                 
         MVI   TGBYTE,C'Y'       AND MARK THAT COMM'L MUST BE PUTREC'D          
         B     UCOM50                                                           
         SPACE 1                                                                
UCOM40   TM    WHEN,X'20'        IF LAST RUN WAS NOT SOON                       
         BZ    UCOM50            AND THIS RUN IS SOON                           
         OI    TACOSTA2,TACOLHFS TURN ON SOON STATUS                            
         MVI   TGBYTE,C'Y'       AND MARK THAT COMM'L MUST BE PUTREC'D          
         SPACE 1                                                                
UCOM50   CLI   TGBYTE,C'Y'       IF COMM'L NEED TO BE PUTREC'D                  
         BNE   UCOM60                                                           
         BRAS  RE,HFPUTREC       WRITE BACK THE RECORD                          
         MVC   LSTUPDCM,TGCOM                                                   
         GOTOR MYTRACE,DMCB,=C'PUT COM'                                         
         SPACE 1                                                                
UCOM60   BRAS  RE,PUTVER         PUT COMMERCIAL VERSIONS TO MQ                  
         SPACE 1                                                                
         USING TLVRD,R4                                                         
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLVRCD,TLVRCDQ                                                   
         MVC   TLVRCOM,TGCOM                                                    
         GOTO1 HIGH                                                             
         B     UCOM80                                                           
UCOM70   GOTO1 SEQ                                                              
UCOM80   CLC   KEY(TLVRVER-TLVRD),KEYSAVE                                       
         BNE   UCOMX                                                            
         DROP  R4                                                               
         SPACE 1                                                                
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         BRAS  RE,PUTVER         PUT COMMERCIAL VERSIONS TO MQ                  
         B     UCOM70                                                           
UCOMX    XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              BUILD COMMERCIAL VERSIONS (TYPE V) RECORDS AND                   
*              PUT THEM TO BACKUP WEB FILE AND MQ                               
*              ON ENTRY ... AIO=A(COMMERCIAL RECORD)                            
         SPACE 1                                                                
         USING RECD,R3                                                          
PUTVER   NTR1  BASE=*,LABEL=*                                                   
         LA    R3,TAPEREC        CLEAR RECORD SPACE                             
         BRAS  RE,CLRREC                                                        
         SPACE 1                                                                
         MVI   RECTYP,RECTYPV    INDICATE COMM'L VERSIONS RECORD                
         LHI   RE,RECVLNQ        AND LENGTH                                     
         STH   RE,PFILELEN                                                      
                                                                                
         USING TLVRD,R4                                                         
         L     R4,AIO                                                           
         MVI   RECCVCD,C'1'                                                     
         CLI   0(R4),TLCOCDQ                                                    
         JE    PV10                                                             
         EDIT  TLVRVER,RECCVCD,ALIGN=LEFT                                       
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
PV10     MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   RECCVID,TACOCID                                                  
         EDIT  (1,TACOSEC),(3,RECCVLN),ALIGN=LEFT                               
         MVI   RECCVRL,C'Y'                                                     
         TM    TACOSTAT,TACOSTRL                                                
         JO    *+8                                                              
         MVI   RECCVRL,C'N'                                                     
         DROP  R4                                                               
                                                                                
         USING TANAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TANAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         ZIC   RE,TANALEN                                                       
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   RECCVTI(0),TANANAME                                              
         DROP  R4                                                               
                                                                                
         OC    RECCVCD,SPACES                                                   
         BRAS  RE,PUTMQ          PUT RECORD TO WEB FILE                         
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO BUILD PERFORMER DETAILS (TYPE P) RECORD               
         SPACE 1                                                                
         USING RECD,R3                                                          
BLDPERF  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,TAPEREC                                                       
         BRAS  RE,CLRREC                                                        
         SPACE 1                                                                
         MVI   RECTYP,RECTYPP     INDIDATE PERFORMER RECORD                     
         LHI   RE,RECPLNQ                                                       
         STH   RE,PFILELEN                                                      
         SPACE 1                                                                
         MVC   RECPER1,P+1        STORE PRINT LINE IN TAPEREC                   
         MVC   RECPER2,P2         STORE SECOND PRINT LINE                       
         SPACE 1                                                                
BPERF10  LA    RE,4               PAD WITH SPACES                               
         LA    R3,TAPEREC                                                       
BPERF20  OC    0(100,R3),SPACES                                                 
         LA    R3,100(R3)                                                       
         BCT   RE,BPERF20                                                       
         SPACE 1                                                                
         L     R3,ABTAPREC                                                      
         LA    R4,TAPEREC                                                       
         BRAS  RE,COPYREC                                                       
         SPACE 1                                                                
         USING LINED,R3                                                         
         CLC   LINEXP,SPACES                                                    
         BE    BPERFX                                                           
         GOTO1 DATVAL,DMCB,LINEXP,DUB                                           
         GOTO1 DATCON,DMCB,(0,DUB),(20,LINEXP)                                  
BPERFX   XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PUT PERFOMER DETAILS (TYPE P) RECORD                  
*              TO BACKUP WEB FILE AND MQ                                        
         SPACE 1                                                                
         USING CASTD,R2                                                         
PUTMQP   NTR1  BASE=*,LABEL=*                                                   
         LA    R3,TAPEREC        MOVE INFORMATION FROM BACKUP                   
         L     R4,ABTAPREC       SPACE TO PRIMARY SPACE                         
         BRAS  RE,COPYREC                                                       
         SPACE 1                                                                
         USING RECD,R3                                                          
         LHI   RE,RECPLNQ2                                                      
         STH   RE,PFILELEN                                                      
         GOTO1 HEXOUT,DMCB,CASTSEQ,RECCSEQ,L'CASTSEQ                            
         DROP  R2,R3                                                            
         SPACE 1                                                                
         BRAS  RE,PUTMQ          AND SEND IT TO MQ                              
         LR    R3,R4                                                            
         BRAS  RE,CLRREC                                                        
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              BUILD PERFORMER'S VERSION (TYPE E) RECORDS AND                   
*              PUT THEM TO BACKUP WEB FILE AND MQ                               
         SPACE 1                                                                
         USING RECD,R3                                                          
PUTCVER  NTR1  BASE=*,LABEL=*                                                   
         USING TAFND,R4                                                         
         MVI   ELCODE,TAFNELQ    GET VERSIONS ELEMENT                           
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVER))                                     
         BNE   PCVX                                                             
         SPACE 1                                                                
         L     R4,TGELEM                                                        
         ZIC   R2,TAFNLEN                                                       
         SHI   R2,3              R3=# OF VERSIONS CAST IS ON                    
         SPACE 1                                                                
         LA    R4,TAFNNAME       R4=A(CAST'S VERSIONS)                          
         DROP  R4                                                               
         SPACE 1                                                                
PCV10    LA    R3,TAPEREC        CLEAR RECORD SPACE                             
         BRAS  RE,CLRREC                                                        
         SPACE 1                                                                
         MVI   RECTYP,RECTYPE    INDICATE PERFORMER'S VERSIONS RECORD           
         LHI   RE,RECELNQ        AND LENGTH                                     
         STH   RE,PFILELEN                                                      
         SPACE 1                                                                
         CLI   0(R4),251         IF CAST MEMBER ON ALL VERSIONS                 
         BNE   PCV20                                                            
         MVI   RECPVCD,C'*'      SEND DOWN *                                    
         B     PCV30                                                            
         SPACE 1                                                                
PCV20    EDIT  (1,0(R4)),RECPVCD,ALIGN=LEFT                                     
         SPACE 1                                                                
PCV30    OC    RECPVCD,SPACES                                                   
         BRAS  RE,PUTMQ          PUT RECORD TO WEB FILE                         
         SPACE 1                                                                
         LA    R4,1(R4)          BUMP TO NEXT VERSION                           
         BCT   R2,PCV10                                                         
PCVX     XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              BUILD MESSAGE (TYPE G) RECORD AND PUT IT TO                      
*              BACKUP WEB FILE AND MQ                                           
         SPACE 1                                                                
PUTMSG   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,WEBCHK                                                        
         BNE   PMSGX                                                            
         SPACE 1                                                                
         LR    R2,R3                                                            
         SPACE 1                                                                
         USING RECD,R3                                                          
         LA    R3,TAPEREC                                                       
         BRAS  RE,CLRREC                                                        
         SPACE 1                                                                
         MVI   RECTYP,RECTYPG                                                   
         LHI   RE,RECGLNQ                                                       
         STH   RE,PFILELEN                                                      
         MVC   RECMSG,1(R2)                                                     
         BRAS  RE,PUTMQ                                                         
PMSGX    XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              BUILD BREAKDOWN (TYPE B) RECORD AND PUT IT TO                    
*              BACKUP WEB FILE AND MQ                                           
         SPACE 1                                                                
PUTBRK   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,WEBCHK                                                        
         BNE   PBRKX                                                            
         SPACE 1                                                                
         USING LINED,R2                                                         
         LR    R2,R3                                                            
         SPACE 1                                                                
         USING RECD,R3                                                          
         LA    R3,TAPEREC                                                       
         BRAS  RE,CLRREC                                                        
         MVI   RECTYP,RECTYPB                                                   
         LHI   RE,RECBLNQ                                                       
         STH   RE,PFILELEN                                                      
         MVC   RECTLIT,LINLIT                                                   
         MVC   RECTAMT,LINPAYC                                                  
         BRAS  RE,PUTMQ                                                         
PBRKX    XIT1                                                                   
         DROP  R2                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              BUILD TOTALS (TYPE T) RECORD AND PUT IT TO                       
*              BACKUP WEB FILE AND MQ                                           
         SPACE 1                                                                
         USING RECD,R3                                                          
PUTTOTS  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,UPDCOMM          (POSSIBLY) UPDATE COMMERCIAL RECORD          
         SPACE 1                                                                
         BRAS  RE,WEBCHK                                                        
         BNE   PTOTS10                                                          
         SPACE 1                                                                
         LA    R3,TAPEREC                                                       
         BRAS  RE,CLRREC                                                        
         SPACE 1                                                                
         MVI   RECTYP,RECTYPT            TOTALS RECORD                          
         LHI   RE,RECTLNQ                                                       
         STH   RE,PFILELEN                                                      
         EDIT  (4,THPAYI),(12,RECGRS),2,MINUS=YES                               
         EDIT  (4,THPAYC),(12,RECMISC),2,MINUS=YES                              
         EDIT  (4,THPNH),(12,RECPHIR),2,MINUS=YES                               
         EDIT  (R2),(12,RECTOT),2,MINUS=YES,ZERO=NOBLANK                        
         MVC   RECDUEDT,SVDUEDT                                                 
         OC    RECDUEDT,SPACES                                                  
         SPACE 1                                                                
         CLC   AGYALPHA,=C'D3'      IF ON FQA                                   
         BNE   PTOTS05                                                          
         CLC   TGCOM,=X'00071EA2'   AND COMMERCIAL IS NOT RCOMML17              
         BE    PTOTS05                                                          
         CLC   TGCOM,=X'00071ED8'   H33.2.1.35                                  
         BE    PTOTS05                                                          
         CLC   TGCOM,=X'00071EED'   H33.2.1.56                                  
         BE    PTOTS05                                                          
         CLC   TGCOM,=X'00071F30'   H33.2.1.70                                  
         BE    PTOTS05                                                          
         CLC   TGCOM,=X'00071EB5'   H33.2.6.27                                  
         BE    PTOTS05                                                          
         CLC   TGCOM,=X'00071F89'   H33.2.1.79                                  
         BE    PTOTS05                                                          
         CLC   TGCOM,=X'00071F3D'   OR C33.6.1                                  
         BE    PTOTS05                                                          
         MVC   RECDUEDT,=C'20081230' SET DUE DATE AS 12/30/08                   
         SPACE 1                                                                
PTOTS05  BRAS  RE,PUTMQ                                                         
         SPACE 1                                                                
         LA    R3,TAPEREC                                                       
         BRAS  RE,CLRREC                                                        
         MVC   0(L'DDOC,R3),DDOC                                                
         LHI   RE,L'DDOC                                                        
         STH   RE,PFILELEN                                                      
         BRAS  RE,PUTMQ                                                         
         SPACE 1                                                                
         L     RE,HFCOUNT                                                       
         AHI   RE,1                                                             
         ST    RE,HFCOUNT                                                       
         SPACE 1                                                                
PTOTS10  ZIC   RE,CMCOUNT          DECREASE NUMBER OF HOLDING FEES              
         BCTR  RE,0                COMPLETED UNTIL NEXT COMMIT IS DUE           
         STC   RE,CMCOUNT          BY ONE                                       
         SPACE 1                                                                
         LTR   RE,RE               IF NEXT COMMIT IS DUE                        
         BNZ   PTOTSX                                                           
         BRAS  RE,CLOSEMQ          COMMIT AND (POSSIBLY) CLOSE MQ               
PTOTSX   XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
DDOC     DC    C'ÙÙ></ddoc>'                                                    
WFCOMMIT DC    C'ALL PREVIOUS PUTS COMMITTED'                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE BUILDS PERFORMER'S PER CYCLE GUARANTEE SUBSIDIARY    *         
*        COMMERCIAL RECORDS (TYPE S) AND PUT THEN TO BACKUP WEB FILE  *         
*        AND THE MQ                                                   *         
*        ON ENTRY ... R2=A(CAST TABLE ENTRY)                          *         
***********************************************************************         
                                                                                
         USING CASTD,R2                                                         
PUTCSUB  NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,PRICOM         IF NOTICE IS FOR GUARANTEE'S PRIMARY           
         JNE   XIT               COMMERCIAL ...                                 
                                                                                
         USING TLCAPD,R4                                                        
         LA    R4,KEY            READ ALL CAST RECORDS FOR THIS                 
         XC    KEY,KEY           PERFORMER ATTACHED TO THIS                     
         MVI   TLCAPD,TLCAGCDQ   PER CYCLE GUARANTEE                            
         MVC   TLCAGSSN,CASTSSN                                                 
         MVC   TLCAGGUA,CASTGUA                                                 
         GOTO1 HIGH                                                             
         J     PCS20                                                            
PCS10    GOTO1 SEQ                                                              
PCS20    CLC   KEY(TLCAGCOM-TLCAPD),KEYSAVE                                     
         JNE   XIT                                                              
         LA    R4,KEY                                                           
         CLC   TLCAGCOM,TGCOM    SKIP THE PER CYCLE COMMERCIAL                  
         JE    PCS10                                                            
         GOTO1 GETREC                                                           
         DROP  R2,R4                                                            
                                                                                
         XC    TGDATE,TGDATE                                                    
                                                                                
         USING TACRD,R4                                                         
         L     R4,AIO            R4=A(CAST RECORD)                              
         MVI   ELCODE,TACRELQ    READ APPLIED CREDIT HISTORY ELEMENTS           
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
PCS30    BRAS  RE,NEXTEL                                                        
         JNE   PCS40                                                            
         MVC   TGDATE,TACRSTRT   SAVE LATEST PAYMENT'S START DATE               
         J     PCS30                                                            
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
PCS40    L     R4,AIO            R4=A(CAST RECORD)                              
         MVI   ELCODE,TACAELQ    GET CAST DETAIL ELEMENT                        
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         OC    TACALAST,TACALAST SKIP COMMERCIAL IF LAST SERVICED               
         JNZ   PCS10                                                            
                                                                                
         OC    TGDATE,TGDATE     IF NO PAYMENTS FOUND ...                       
         JNZ   PCS50                                                            
                                                                                
         MVC   TGDATE,TACAFCYC   SAVE CAST'S FIRST FIXED CYCLE DATE             
         DROP  R4                                                               
                                                                                
PCS50    MVC   SVCAKEY,KEY       SAVE CAST KEY                                  
                                                                                
         USING TLCOPD,R4                                                        
         LA    R4,KEY                                                           
         XC    KEY,KEY           READ THE COMMERCIAL RECORD                     
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,SVCAKEY+TLCAGCOM-TLCAPD                                 
         GOTO1 HIGH                                                             
         CLC   TLCOPKEY,KEYSAVE                                                 
         JNE   PCS60                                                            
         GOTO1 GETREC                                                           
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO            R4=A(COMMERCIAL RECORD)                        
         MVI   ELCODE,TACOELQ    GET COMMERCIAL DETAILS ELEMENT                 
         BRAS  RE,GETEL                                                         
         JNE   PCS60                                                            
                                                                                
         TM    TACOSTAT,TACOSTRL SKIP COMMERCIAL IF RELEASED                    
         JO    PCS60                                                            
                                                                                
         USING RECD,R3                                                          
         LA    R3,TAPEREC        CLEAR RECORD SPACE                             
         BRAS  RE,CLRREC                                                        
         MVC   RECSCID,TACOCID   AND INSERT COMMERCIAL ID                       
         DROP  R4                                                               
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO            R4=A(COMMERCIAL RECORD)                        
         MVI   RECTYP,RECTYPS    INDICATE GUARANTEE ATTACHED COMM'LS            
         LHI   RE,RECSLNQ        RECORD AND LENGTH                              
         STH   RE,PFILELEN                                                      
         MVC   RECSAGY,TLCOAGY   INSERT AGENCY                                  
         MVC   RECSCLI,TLCOCLI   CLIENT                                         
         MVC   RECSPRD,TLCOPRD   AND PRODUCT                                    
         OC    RECSPRD,SPACES                                                   
         DROP  R4                                                               
                                                                                
         USING TANAD,R4                                                         
         MVI   ELCODE,TANAELQ    GET NAME ELEMENT                               
         BRAS  RE,GETEL                                                         
         JNE   PCS60                                                            
         MVC   RECSTIT,SPACES                                                   
         ZIC   RE,TANALEN        INSERT COMMERCIAL TITLE                        
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   RECSTIT(0),TANANAME                                              
         DROP  R4                                                               
                                                                                
         GOTO1 DATCON,DMCB,(1,TGDATE),(8,WORK)                                  
         MVI   WORK+8,C'-'                                                      
         MVC   WORK+9(5),THLCYC  BUILD INPUT TO PERVAL                          
                                                                                
         USING PERVALD,R5                                                       
         LA    R5,BLOCK          INSERT NEXT CYCLE START DATE                   
         GOTO1 PERVAL,DMCB,(14,WORK),(R5)                                       
         MVC   TGDATE,PVALPEND                                                  
         GOTO1 DATCON,DMCB,(1,TGDATE),(20,RECSNCS)                              
         DROP  R5                                                               
                                                                                
         GOTO1 DATCON,DMCB,(1,TGDATE),(8,WORK)                                  
         MVI   WORK+8,C'-'                                                      
         MVC   WORK+9(5),THLCYC  BUILD INPUT TO PERVAL                          
                                                                                
         USING PERVALD,R5                                                       
         LA    R5,BLOCK          INSERT NEXT CYCLE END DATE                     
         GOTO1 PERVAL,DMCB,(14,WORK),('PVIN1DYL',(R5))                          
         GOTO1 DATCON,DMCB,(1,PVALPEND),(20,RECSNCE)                            
         DROP  R5                                                               
                                                                                
         BRAS  RE,PUTMQ          AND PUT RECORD TO WEB FILE                     
         DROP  R3                                                               
                                                                                
PCS60    MVC   KEY,SVCAKEY       RESTORE CAST READ SEQUENCE                     
         GOTO1 HIGH              AND GO READ THE NEXT KEY                       
         J     PCS10                                                            
                                                                                
***********************************************************************         
*        ROUTINE DETERMINES IF THIS IS A NOTICE FOR THE PRIMARY       *         
*        COMMERCIAL OF PERFORMER'S PER CYCLE GUARANTEE                *         
*        ON ENTRY ... R2=A(CAST TABLE ENTRY)                          *         
***********************************************************************         
                                                                                
         USING CASTD,R2                                                         
PRICOM   NTR1                                                                   
         TM    THCOSTA2,TACOPCYC   IF NOTICE IS FOR PER CYCLE COMM'L            
         JO    YES                 MUST BE PRIMARY COMMERCIAL                   
                                                                                
         OC    CASTGUA,CASTGUA     IF PERFORMER IS ON GUARANTEE                 
         JZ    NO                                                               
                                                                                
         USING TLGUD,R4                                                         
         LA    R4,KEY                                                           
         XC    KEY,KEY             READ THE GUARANTEE RECORD                    
         MVI   TLGUCD,TLGUCDQ                                                   
         MVC   TLGUSSN,CASTSSN                                                  
         MVC   TLGUGUA,CASTGUA                                                  
         XC    TLGUGUA,=X'FFFFFFFF'                                             
         GOTO1 HIGH                                                             
         CLC   TLGUKEY,KEYSAVE                                                  
         JNE   NO                                                               
         GOTO1 GETREC                                                           
         DROP  R2,R4                                                            
                                                                                
         USING TAGUD,R4                                                         
         L     R4,AIO              GET GUARANTEE DETAILS ELEMENT                
         MVI   ELCODE,TAGUELQ                                                   
         BAS   RE,GETEL                                                         
         JNE   NO                                                               
         CLC   TAGUCOM,TGCOM                                                    
         JE    YES                 CHECK PRIMARY COMMERCIAL                     
         J     NO                                                               
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              PUT SPACES IN 400 BYTES OF TAPEREC                               
         SPACE 1                                                                
CLRREC   NTR1  BASE=*,LABEL=*                                                   
         LA    RE,4                PUT 400 SPACES INTO AREA                     
         MVC   0(100,R3),SPACES    POINTED AT BY R3                             
         LA    R3,100(R3)                                                       
         BCT   RE,*-10                                                          
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO COPY RECORD FROM 400 BYTE AREA POINTED                
*              AT BY R4 TO AREA POINTED AT BY R3                                
         SPACE 1                                                                
COPYREC  NTR1  BASE=*,LABEL=*                                                   
         LA    RE,4                                                             
         MVC   0(100,R3),0(R4)                                                  
         LA    R3,100(R3)                                                       
         LA    R4,100(R4)                                                       
         BCT   RE,*-14                                                          
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CONVERT TYPE 8 DATE FIELD TO TYPE 20                  
*              INPUT AND OUTPUT IN TGDUB                                        
         SPACE 1                                                                
CVTDTE20 NTR1  BASE=*,LABEL=*                                                   
         GOTO1 DATVAL,DMCB,TGDUB,DUB                                            
         GOTO1 DATCON,DMCB,(0,DUB),(20,TGDUB)                                   
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PERFORM PUTREC                                        
         SPACE 1                                                                
HFPUTREC NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,UPDCHK           NOT READY FOR WEB YET                        
         BNE   HPRX                                                             
         GOTO1 PUTREC                                                           
HPRX     XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CALCULATES EMS RATES FOR BILLING TYPE E                  
         SPACE 1                                                                
CALCEMS  NTR1  BASE=*,LABEL=*                                                   
         ZAP   TGDUB,=P'0'         CLEAR GRAND TOTAL                            
         MVI   MSGSTAT,0                                                        
         SPACE 1                                                                
         USING RECD,R5                                                          
         L     R3,ABTAPREC                                                      
         BRAS  RE,CLRREC                                                        
         LR    R5,R3                                                            
         SPACE 1                                                                
         USING LINED,R3                                                         
         LA    R3,P                                                             
         SPACE 1                                                                
         TM    WHEN,X'20'          IF NOT RUNNING SOON                          
         BO    CEMS10              SAVE PRINTABLE TOTALS                        
         EDIT  (4,THPAYI),(12,RECGRS),2,MINUS=YES                               
         EDIT  (4,THPAYC),(12,RECMISC),2,MINUS=YES                              
         EDIT  (4,THPNH),(12,RECPHIR),2,MINUS=YES                               
         SPACE 1                                                                
CEMS10   L     R2,THPAYI           ADD PAYMENT TOTALS TOGETHER                  
         A     R2,THPAYC           AND PUT AS WAGES/MISC                        
         MVC   LINLIT(L'LTEMSWG),LTEMSWG                                        
         BRAS  RE,EDIT2TOT                                                      
         MVC   RECWMI,LINPAYC                                                   
         SPACE 1                                                                
         MVC   HALF,TGBFUTA        PUT PAYROLL TAXES                            
         TM    TGUSSTAT,SESSION                                                 
         BO    *+10                                                             
         MVC   HALF,TGBSUTA                                                     
         BRAS  RE,MULTP                                                         
         L     R2,FULL                                                          
         A     R2,THSURTOT         ADD SUI SURCHARGE (NY/CA)                    
         ST    R2,FULL                                                          
         ZICM  R2,FULL,4                                                        
         MVC   LINLIT(L'LTTAX),LTTAX                                            
         BRAS  RE,EDIT2TOT                                                      
         MVC   RECPRX,LINPAYC                                                   
         SPACE 1                                                                
         L     R2,THPNH            P&H AMOUNT                                   
         S     R2,THINR            SUBTRACT I&R (ADDED TO WAGES)                
         MVC   LINLIT(L'LTPNH),LTPNH                                            
         BRAS  RE,EDIT2TOT         ADD P&H TO TOTAL                             
         MVC   RECPNH,LINPAYC                                                   
         SPACE 1                                                                
         L     R2,THGSTU           GST AMOUNT                                   
         MVC   LINLIT(L'LTGST),LTGST                                            
         BRAS  RE,EDIT2TOT         ADD GST TO TOTAL                             
         MVC   RECGST,LINPAYC                                                   
         SPACE 1                                                                
         CVB   R2,TGDUB            SUBTOTAL                                     
         A     R2,THINR            ADD I&R                                      
         CVD   R2,TGDUB            STORE BACK IN SUBTOTAL                       
         MVC   HALF,TGBFICA        HANDLING FEE RATE                            
         BRAS  RE,MULTP            MULT PERCENTAGE                              
         ZICM  R2,FULL,4           TOTAL HANDLING FEE                           
         MVC   LINLIT(L'LTHND),LTHND                                            
         BRAS  RE,EDIT2TOT                                                      
         MVC   RECHND,LINPAYC                                                   
         SPACE 1                                                                
         CVB   R2,TGDUB            SUBTOTAL                                     
         MVC   HALF,TGBOFIC        EMS FEE RATE                                 
         BRAS  RE,MULTP            MULT PERCENTAGE                              
         ZICM  R2,FULL,4           TOTAL EMS FEE                                
         MVC   LINLIT(L'LTEMSF),LTEMSF                                          
         BRAS  RE,EDIT2TOT                                                      
         MVC   RECEMSF,LINPAYC                                                  
         SPACE 1                                                                
         BRAS  RE,HFORUSCK         IF OVERNIGHT HOLDING FEES OR                 
         BNE   CEMSX               OVERNIGHT HOLDING FEE UPDATE                 
         MVC   LINLIT(L'LTGRAND),LTGRAND                                        
         CVB   R2,TGDUB            GRAND TOTAL                                  
         BRAS  RE,EDIT2FOR                                                      
         MVC   RECTOT,LINPAYC                                                   
         SPACE 1                                                                
         BRAS  RE,PUTTOTSE         PUT TOTALS TO EMS FILE                       
         SPACE 1                                                                
         NI    THSTAT3,X'FF'-THELHDRP                                           
CEMSX    XIT1                                                                   
         DROP  R3,R5                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              PUT EMS INVOICE TOTALS TO DATASET                                
         SPACE 1                                                                
         USING RECD,R3                                                          
PUTTOTSE NTR1  BASE=*,LABEL=*                                                   
         LA    R3,TAPEREC                                                       
         L     R4,ABTAPREC                                                      
         BRAS  RE,COPYREC                                                       
         SPACE 1                                                                
         MVI   RECTYP,RECTYPT    TOTALS RECORD                                  
         GOTOR PUTTAPE           PUT RECORD TO TAPE                             
         SPACE 1                                                                
         BRAS  RE,PUTTOTS                                                       
         XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         EJECT                                                                  
*                                                                               
*              TOTAL PRINTING ROUTINES FOR EMS                                  
         SPACE 2                                                                
         USING LINED,R3            R3 --> P                                     
EDIT2TOT NTR1  BASE=*,LABEL=*      EDIT/PRINT WITH SUPPRESSING ZERO             
         LTR   R2,R2               IF NOTHING TO PRINT                          
         BZ    EDIT2T3                                                          
         BRAS  RE,EDIT2FOR                                                      
         B     EDIT2X                                                           
EDIT2T3  MVC   LINLIT,SPACES       CLEAR LITERAL                                
         MVC   LINPAYC,SPACES      CLEAR OLD AMOUNT                             
EDIT2X   B     EDITX               AND RETURN                                   
         SPACE 3                                                                
EDIT2FOR NTR1  BASE=*,LABEL=*      EDIT/PRINT ALWAYS                            
         BRAS  RE,PRTMSG           PRINT OUT SEASONAL MESSAGES                  
         SPACE 1                                                                
         EDIT  (R2),(12,LINPAYC),2,MINUS=YES,ZERO=NOBLANK                       
         SPACE 1                                                                
         CLC   LINLIT(L'LTSTOT),LTSTOT    IF THIS ISN'T SUB-TOTAL               
         BE    EDIT2F5                                                          
         CLC   LINLIT(L'LTGRAND),LTGRAND  OR GRAND TOTAL                        
         BE    EDIT2F3                                                          
         AP    TGDUB,DUB                  ADD TO GRAND TOTAL                    
         B     EDIT2F5                                                          
         SPACE 1                                                                
EDIT2F3  AP    THAGYTOT,TGDUB      ADD GRAND TOTAL TO AGY/CLI TOTAL             
         SPACE 1                                                                
EDIT2F5  BRAS  RE,PUTBRK           PUT BREAKDOWN REC TO WEB FILE                
         SPACE 1                                                                
         TM    WHEN,X'20'          IF NOT SOON, DON'T PRINT                     
         BNO   EDITX                                                            
         BRAS  RE,MYSPOOL          PRINT THE LINE                               
EDITX    XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              PUT RECORD TO EMS TAPE                                           
         SPACE 1                                                                
         USING RECD,R3                                                          
PUTTAPE  NTR1  BASE=*,LABEL=*                                                   
         CLI   WHEN,X'20'        IF SOON, DON'T WRITE TO TAPE                   
         BE    PTAPEX                                                           
         LA    R4,HLDISK                                                        
         LA    R3,TAPEREC                                                       
         PUT   (R4),(R3)         PUT IT TO TAPE/DATASET                         
         SPACE 1                                                                
         BRAS  RE,WEBCHK         EXIT IF NOT GENERATING WEB FILE                
         BNE   PTAPEX                                                           
         SPACE 1                                                                
         CLI   RECTYP,RECTYPT                                                   
         BE    PTAPEX                                                           
         SPACE 1                                                                
         CLI   RECTYP,RECTYPP                                                   
         BNE   PTAPE10                                                          
         BRAS  RE,PUTMQP                                                        
         B     PTAPEX                                                           
         SPACE 1                                                                
PTAPE10  BRAS  RE,PUTMQ                                                         
PTAPEX   XIT                                                                    
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO SET NAME OF DYNAMICALLY GENERATED EMS FILE            
*              RETURNS NAME IN WORK                                             
         SPACE 1                                                                
STTAPNAM NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK,SPACES                                                      
         MVC   WORK(20),=CL20'TALDISK.TA0EMSHF'                                 
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SET NAME OF DYNAMICALLY GENERATED WEB FILE            
*              AND WRITE IT TO HOLDING FEE WEB FILENAMES RECORD                 
*              RETURNS FILENAME IN WORK                                         
         SPACE 1                                                                
STWEBNAM NTR1  BASE=*,LABEL=*                                                   
         USING SSOOFF,R3                                                        
         GOTO1 DATAMGR,DMCB,=C'SSBAD'                                           
         ICM   R3,15,4(R1)         =A(SSB)                                      
         CLC   0(2,R3),=H'00'                                                   
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   TGBYTE4,SSODSPAC-SSOOFF(R3)                                      
         DROP  R3                                                               
                                                                                
         USING WEBNAMD,R4                                                       
         LA    R4,WORK                                                          
         MVC   WORK,SPACES                                                      
                                                                                
         MVC   WNHEAD,WEBHEADT   SET FILE NAME HEADER                           
         MVI   WNRUNSYS,C'2'     AND SYSTEM                                     
         CLI   TGBYTE4,C'T'      FOR TST RUN                                    
         BE    SWN20                                                            
                                                                                
         MVC   WNHEAD,WEBHEADQ   SET FILE NAME HEADER                           
         MVI   WNRUNSYS,C'3'     AND SYSTEM                                     
         CLI   TGBYTE4,C'Q'      FOR FQA RUN                                    
         BE    SWN20                                                            
                                                                                
         MVC   WNHEAD,WEBHEADC   SET FILE NAME HEADER                           
         MVI   WNRUNSYS,C'1'     AND SYSTEM                                     
         CLI   TGBYTE4,C'C'      FOR CSC RUN                                    
         BE    SWN20                                                            
                                                                                
         MVC   WNHEAD,WEBHEAD    OTHERWISE SET FOR ADV                          
                                                                                
SWN20    MVC   WNRUNTYP,WEBHF    INDICATE WHAT KIND OF RUN                      
         CLI   RECNUM,HF         GENERATED THE FILE                             
         BE    SWN30                                                            
         MVC   WNRUNTYP,WEBSN                                                   
         CLI   RECNUM,SN                                                        
         BE    SWN30                                                            
         MVC   WNRUNTYP,WEBHU                                                   
         CLI   RECNUM,HC                                                        
         BE    SWN30                                                            
         DC    H'00'                                                            
                                                                                
SWN30    MVC   WNRUNWHN(L'WEBOV),WEBOV                                          
         TM    WHEN,X'20'        INDICATE IF REPORT IS                          
         BNO   SWN40             OVERNIGHT OR SOON                              
         MVC   WNRUNWHN(L'WEBSO),WEBSO                                          
                                                                                
SWN40    MVC   TGDUB,TGTODAY8    INDICATE RUN DATE                              
         BRAS  RE,CVTDTE20                                                      
         MVC   WNRUNDTE,TGDUB+2                                                 
                                                                                
         MVC   WNDOT3(L'WEBTIME),WEBTIME                                        
         TIME  DEC               INDICATE RUN TIME                              
         ST    R0,TGDUB                                                         
         GOTO1 HEXOUT,DMCB,TGDUB,WNRUNTIM,3                                     
         DROP  R4                                                               
                                                                                
         USING TLSYD,R4                                                         
         LA    R4,KEY            GET HOLDING FEE WEB FILENAMES RECORD           
         XC    KEY,KEY                                                          
         MVI   TLSYCD,TLSYCDQ                                                   
         MVI   TLSYTYPE,TLSYHFFN                                                
SWN50    GOTO1 HIGH                                                             
         CLC   TLSYKEY,KEYSAVE                                                  
         BNE   SWNX                                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
                                                                                
         USING TLRCD,RE                                                         
         L     RE,AIO            IS THERE ROOM TO ADD FILENAME ELEMENT?         
         CLC   TLRCLEN,=H'1900'                                                 
         BL    SWN60                                                            
         DROP  RE                                                               
                                                                                
         MVC   KEY,KEYSAVE       IF NOT, SET TO READ NEXT                       
         ZIC   RE,TLSYSEQ        HOLDING FEE WEB FILENAMES RECORD               
         AHI   RE,1                                                             
         STC   RE,TLSYSEQ                                                       
         B     SWN50                                                            
         DROP  R4                                                               
                                                                                
SWN60    GOTOR MYTRACE,DMCB,=C'GET SYS'                                         
                                                                                
         USING TACMD,R4                                                         
         LA    R4,ELEMENT        ADD THIS FILENAME TO RECORD                    
         XC    ELEMENT,ELEMENT                                                  
         MVI   TACMEL,TACMELQ                                                   
         MVI   TACMLEN,3+WNLNQ                                                  
         MVI   TACMTYPE,TACMTYPF                                                
         MVC   TACMCOMM(WNLNQ),WORK                                             
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         GOTO1 PUTREC            AND WRITE BACK RECORD                          
         GOTOR MYTRACE,DMCB,=C'PUT SYS'                                         
SWNX     XIT1                                                                   
                                                                                
WEBHEAD  DC    C'TALDISK.TA0W '                                                 
WEBHEADT DC    C'TST.TAL.TA0W '                                                 
WEBHEADQ DC    C'FQA.TAL.TA0W '                                                 
WEBHEADC DC    C'CSC.TAL.TA0W '                                                 
WEBHF    DC    C'HF'                                                            
WEBSN    DC    C'SN'                                                            
WEBHU    DC    C'HU'                                                            
WEBOV    DC    C'O.D'                                                           
WEBSO    DC    C'S.D'                                                           
WEBTIME  DC    C'.T'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SET ADDRESS OF DYNAMIC ALLOCATION ROUTINE             
         SPACE 1                                                                
SETDYNA  NTR1  BASE=*,LABEL=*                                                   
         L     RE,ATWA                                                          
         USING CONHEADH-64,RE                                                   
         L     R1,TWADCONS                                                      
         USING TWADCOND,R1                                                      
         MVC   DYNALLOC,TDYNALLO                                                
         DROP  R1,RE                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              OPEN EMS TAPE                                                    
         SPACE 1                                                                
OPENTAPE NTR1  BASE=*,LABEL=*                                                   
         TM    THSTAT2,THOPEN    EXIT IF EMS FILE ALREADY OPENED                
         BO    OTAPEX                                                           
         BRAS  RE,HFORUSCK       IF OVERNIGHT HOLDING FEES OR HOLDING           
         BNE   OTAPEX            FEE UPDATE, DON'T WRITE TO TAPE                
         SPACE 1                                                                
         BRAS  RE,SETDYNA        SET ADDRESS OF DYNALLOC ROUTINE                
         BRAS  RE,STTAPNAM                                                      
         GOTO1 DYNALLOC,DMCB,(0,=CL8'HLDISK'),(0,WORK)                          
         SPACE 1                                                                
         LA    R2,HLDISK         OPEN EMS FILE                                  
         OPEN  ((2),OUTPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'00'                                                            
         OI    THSTAT2,THOPEN   TURN ON OPENED FILE STATUS                      
         SPACE 1                                                                
*        GOTO1 DATCON,DMCB,(5,0),(11,EMSDATE)                                   
*        MVC   EMSNAME,WORK                                                     
*        GOTO1 DATAMGR,DMCB,=C'OPMSG',(=AL1(EMSFILLN),EMSFILE)                  
OTAPEX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO OPEN UP BACKUP FILE OF ALL MQ-BOUND                   
*              INFORMATION                                                      
         SPACE 1                                                                
OPENWF   NTR1  BASE=*,LABEL=*                                                   
         TM    THSTAT2,THOPENWF    IF BACKUP FILE NOT ALREADY OPENED            
         BNZ   OPENWFX                                                          
         BRAS  RE,SETDYNA          SET ADDRESS OF DYNALLOC ROUTINE              
         BRAS  RE,STWEBNAM         AND SET BACKUP FILE NAME                     
         SPACE 1                                                                
         LA    R2,TGDUB                                                         
         SPACE 1                                                                
         MVI   BYTE,X'41'              CYLINDER                                 
         MVC   TGDUB,=X'000005000010'  PRI=5,SEC=10                             
*        MVC   TGDUB,=X'000005000001'  PRI=5,SEC=1                              
*        MVC   TGDUB,=X'000012000001'                                           
         OC    TIFCOM,TIFCOM       DYNAMICALLY ALLOCATE FILE                    
         BZ    OPENWF10            (SIZE DETERMINED BY COMM'L FILTER)           
*        MVI   BYTE,X'81'                                                       
*        MVC   TGDUB,=X'000002000018'                                           
         MVC   TGDUB,=X'000001000001'  PRI=1,SEC=1                              
OPENWF10 GOTO1 DYNALLOC,DMCB,(X'80',=CL8'WEBFIL'),                     +        
               (BYTE,(R2)),(X'80',WORK)                                         
         SPACE 1                                                                
         LA    R2,WEBFILE          OPEN BACKUP FILE FOR INPUT                   
         OPEN  ((2),OUTPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'00'                                                            
         OI    THSTAT2,THOPENWF    TURN ON OPENED WEB FILE STATUS               
OPENWFX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PUT ENTRY TO BACKUP WEB FILE                          
         SPACE 1                                                                
PUTWEBF  NTR1  BASE=*,LABEL=*                                                   
         TM    THSTAT2,THOPENWF    IF WEB FILE IS OPEN                          
         BZ    PWFX                                                             
         LA    R2,WEBFILE                                                       
         LA    R3,FILEREC                                                       
         PUT   (R2),(R3)         PUT TO WEB FILE                                
PWFX     XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              CLOSE TAPE                                                       
         SPACE 1                                                                
CLOSTAPE NTR1  BASE=*,LABEL=*                                                   
         TM    THSTAT2,THOPEN    HAS THE DATASET BEEN OPENED?                   
         BNO   CTAPEX                                                           
         LA    R2,HLDISK         CLOSE TAPE                                     
         CLOSE ((2))                                                            
         LTR   RF,RF                                                            
         BZ    CTAPEX                                                           
         DC    H'00'                                                            
CTAPEX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CLOSE WEB FILE                                        
         SPACE 1                                                                
CLOSEWF  NTR1  BASE=*,LABEL=*                                                   
         TM    THSTAT2,THOPENWF  IF WEB FILE HAS BEEN OPENED                    
         BNO   CFILEX            CLOSE IT                                       
         SPACE 1                                                                
         LA    R3,TAPEREC        PUT COUNT OF HOLDING FEES TO MQ                
         BRAS  RE,CLRREC                                                        
         MVI   TRSTAT,C'X'                                                      
         XC    TRLENGTH,TRLENGTH                                                
         EDIT  HFCOUNT,(10,TAPEREC),ALIGN=LEFT                                  
         SPACE 1                                                                
         LA    R2,WEBFILE                                                       
         LA    R3,FILEREC                                                       
         PUT   (R2),(R3)         PUT TO WEB FILE                                
         CLOSE ((2))                                                            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'00'                                                            
         SPACE 1                                                                
         NI    THSTAT2,X'FF'-THOPENWF                                           
CFILEX   XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SEND ERROR EMAIL TO TALENT TEAM                       
         SPACE 1                                                                
MQEMSG   NTR1  BASE=*,LABEL=*                                                   
         CLI   DMCB+8,0                        IF MQ RETURNS AN                 
         BE    MQEMX                           ERROR CODE                       
         GOTO1 DATCON,DMCB,(5,0),(11,HFDATE)                                    
         MVC   HFMSG,WORK                      SEND EMAIL NOTIFICATION          
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(=AL1(HFNAMELN),HFFILE)                   
         OI    THSTAT2,THMQEROR                                                 
MQEMX    XIT1                                                                   
         SPACE 2                                                                
HFFILE   DC    C'AUTONOTE*US-TALENT_TEAM:'                                      
HFDATE   DC    C'JAN01/01 '                                                     
HFMSG    DS    CL15                                                             
HFNAMELN EQU   *-HFFILE                                                         
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
DYNALLOC DS    A                   DYNAMIC ALLOCATION OF DATASET                
*                                                                               
HLDISK   DCB   DDNAME=HLDISK,DSORG=PS,RECFM=FB,LRECL=400,BLKSIZE=0,    X        
               MACRF=PM                                                         
WEBFILE  DCB   DDNAME=WEBFIL,DSORG=PS,RECFM=FB,LRECL=500,BLKSIZE=0,    X        
               MACRF=PM                                                         
*                                                                               
EMSFILE  DC    C'AUTONOTE*GHOA,MZEI,JBAS,SCHT:'                                 
EMSDATE  DC    C'JAN01/01 EMS FILE CREATED '                                    
EMSNAME  DS    CL20                                                             
EMSFILLN EQU   *-EMSFILE                                                        
*                                                                               
         EJECT                                                                  
*              ROUTINE CHECKS IF OFFICE IS AN FLIST                             
*                                                                               
FILTOFF  NTR1  BASE=*,LABEL=*                                                   
         L     RE,ATWA                                                          
         USING T703FFD,RE                                                       
         LA    RF,SHFOFFH                                                       
         GOTOR POSFLIST            POSITIVE FLIST?                              
         JNE   FOFF10                                                           
         GOTOR GETOFF              IF OFFICE IS A POSITIVE FLIST                
         JNE   NO                  IT MUST BE IN THE TABLE                      
         J     YES                                                              
FOFF10   GOTOR NEGFLIST            NEGATIVE FLIST?                              
         JNE   YES                                                              
         GOTOR GETOFF              IF OFFICE IS A NEGATIVE FLIST                
         JE    NO                  IT MUST NOT BE IN THE TABLE                  
         J     YES                                                              
         DROP  RE                                                               
         EJECT                                                                  
*              ROUTINE TO SEND CERNO MESSAGE TO MQ                              
         SPACE 1                                                                
CRNTEST  NTR1  BASE=*,LABEL=*                                                   
         TM    THOPTS,THCNOTST     IF OPTION FIELD CONTAINS CERNOTEST           
         JZ    CTNO                                                             
         SPACE 2                                                                
         MVC   WORK(16),=C'TALHOLD*********'                                    
         CLC   TGUSER,=H'7538'                                                  
         BNE   *+10                                                             
         MVC   WORK(3),=C'FQA'                                                  
         SPACE 1                                                                
         GOTO1 VMQRPT,DMCB,(0,=C'OPEN'),(0,WORK),(X'C0',0),0                    
         CLI   DMCB+8,0            OPEN THE MQ                                  
         JE    *+6                                                              
         DC    H'00'                                                            
         SPACE 1                                                                
         GOTO1 VMQRPT,DMCB,(0,=C'PUT'),CTDM,L'CTDM,0                            
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'00'               AND SEND MESSAGE TO MQ                       
         SPACE 1                                                                
         GOTO1 VMQRPT,DMCB,(0,=C'PUT'),CTDH1,L'CTDH1+L'CTDH2+L'CTDH3,0          
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         SPACE 1                                                                
         GOTO1 VMQRPT,DMCB,(0,=C'PUT'),CTDD1,L'CTDD1+L'CTDD2,0                  
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         SPACE 1                                                                
         GOTO1 VMQRPT,DMCB,(0,=C'PUT'),CTDDX,L'CTDDX,0                          
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         SPACE 1                                                                
         GOTO1 VMQRPT,DMCB,(0,=C'PUT'),CTDMX,L'CTDMX,0                          
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         SPACE 1                                                                
         GOTO1 VMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'00'               CLOSE THE MQ                                 
         SPACE 2                                                                
CTYES    XR    RC,RC                                                            
CTNO     LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 2                                                                
CTDM     DC    C'<dmessage>'                                                    
CTDH1    DC    C'<dhead><doctypeId>holding fee</doctypeId><sourceId>'           
CTDH2    DC    C'mainframe</sourceId><formatId>fixed-length text'               
CTDH3    DC    C'</formatId></dhead>'                                           
CTDD1    DC    C'<ddoc externalDocId="IDONTEXIST" '                             
CTDD2    DC    C'externalVersionId="test"><!CDATA'                            
CTDDX    DC    C'ÙÙ></ddoc>'                                                    
CTDMX    DC    C'</dmessage>'                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CHECKS IF AGENCY IS AN FLIST                             
*                                                                               
AGYFLIST NTR1  BASE=*,LABEL=*                                                   
         L     R3,ATWA                                                          
         USING T703FFD,R3                                                       
         LA    RF,SHFAGYH                                                       
         GOTOR POSFLIST            IF AGENCY IS A POSITIVE FLIST                
         BE    AGFL10                                                           
         GOTOR NEGFLIST            OR IF AGENCY IS A NEGATIVE FLIST             
         BE    AGFL15                                                           
         B     XIT2                                                             
AGFL10   ZIC   R1,SHFAGYH+5        POSITIVE FLIST                               
         SHI   R1,2                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TGLST(0),SHFAGY+1                                                
         B     AGFL18                                                           
AGFL15   ZIC   R1,SHFAGYH+5        NEGATIVE FLIST                               
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TGLST(0),SHFAGY+2                                                
AGFL18   OC    TGLST,SPACES                                                     
*                                                                               
         GOTOR GETFFLST            GET FLIST                                    
         BE    AGFL30                                                           
AGFL20   GOTOR GETNFLST            GET NEXT CODE IN FLIST                       
         BNE   AGFLX                                                            
AGFL30   GOTOR POSFLIST                                                         
         BNE   AGFL40              IF POSITIVE FLIST,                           
         GOTOR AGFLTAB             STORE FLIST AGENCIES INTO AGYTAB             
         B     AGFL20                                                           
AGFL40   GOTOR NAGFLTAB            IF NEGATIVE FLIST,                           
         B     AGFL20              STORE NEG FLIST AGY'S INTO FAGYTAB           
AGFLX    XC    TIFAGY,TIFAGY                                                    
         B     XIT2                                                             
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE STORES AGENCIES FROM THE FLIST INTO AGYTAB               
*                                                                               
AGFLTAB  NTR1  BASE=*,LABEL=*                                                   
         L     R3,=A(AGYTAB)       R3=A(TABLE)                                  
         USING AGYD,R3                                                          
AFLT10   CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                (INCREASE NAGY)                              
         CLI   0(R3),0             NEXT SLOT                                    
         BE    AFLT20                                                           
         LA    R3,AGYNEXT                                                       
         B     AFLT10              GO TO NEXT OPEN SLOT                         
*                                                                               
AFLT20   MVC   AGYAGY,TIFAGY       BUILD NEW ENTRY                              
         MVC   TGAGY,TIFAGY        SET TO GET AGENCY RECORD                     
*                                                                               
         GOTO1 NEWALL,DMCB,('TLAYCDQ',0),0,0,=C'AGENCY RULES'                   
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ      GET AGENCY DETAILS ELEMENT                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R4                                                         
         MVC   AGYHLDS,TAAYHLDS    SAVE HOLDING FEE STATUS                      
         MVC   AGYOFF,TAAYTPOF     OFFICE                                       
         MVC   AGYTPC,TAAYTPC      TPC STAFF                                    
*                                                                               
         GOTOR SETPEND             SET PERIOD END DATE FOR THIS AGENCY          
         MVC   AGYLAST,THCYCE      SAVE IT                                      
         B     XIT2                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
*              ROUTINE STORES AGENCIES FROM THE NEGATIVE FLIST INTO             
*              FAGYTAB                                                          
*                                                                               
NAGFLTAB NTR1  BASE=*,LABEL=*                                                   
         L     R3,=A(FAGYTAB)      R3=A(TABLE)                                  
         USING FAGYD,R3                                                         
NAFLT10  CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                (INCREASE NFAGY)                             
         CLI   0(R3),0             NEXT SLOT                                    
         BE    NAFLT20                                                          
         LA    R3,FAGYNEXT                                                      
         B     NAFLT10             GO TO NEXT OPEN SLOT                         
*                                                                               
NAFLT20  MVC   FAGYAGY,TIFAGY      BUILD NEW ENTRY                              
         MVC   TGAGY,TIFAGY        SET TO GET AGENCY RECORD                     
         B     XIT2                                                             
         DROP  R3                                                               
         EJECT                                                                  
*              SET PERIOD END DATE FOR THIS AGENCY                              
*                                                                               
         USING TAAYD,R4            R4=A(AGENCY DETAILS EL.)                     
SETPEND  NTR1  BASE=*,LABEL=*                                                   
         MVC   THLCYC,=C'(3M) '    ASSUME 3 MONTH CYCLES                        
         TM    TAAYSTAT,TAAYS13W                                                
         BZ    *+10                                                             
         MVC   THLCYC,=C'(13W)'    AGENCY USES 13 WEEK CYCLES                   
*                                                                               
         MVC   THNWEEKS,TAAYHLD    SET N'WEEKS ADVANCE NOTICE                   
         NI    THNWEEKS,X'7F'      (CRUNCH OFF FREQUENCY BIT)                   
*                                                                               
         MVC   BLOCK(8),TGTODAY8   SET UP PERIOD FOR PERVAL                     
         MVC   BLOCK+8(2),=C'-('   TODAY                                        
*                                                                               
         ZIC   R2,THNWEEKS         + (N'WEEKS ADVANCE NOTICE                    
         AHI   R2,1                   + 1)                                      
*                                                                               
         TM    THSTAT,THSECOND     IF WE'RE RUNNING SECOND NOTICES              
         BZ    *+8                                                              
         LA    R2,2                ALWAYS USE THE SAME N'WEEKS NOTICE           
*                                                                               
         EDIT  (R2),(3,BLOCK+10),ALIGN=LEFT                                     
         LR    R1,R0                                                            
         LA    R1,BLOCK+10(R1)                                                  
         MVC   0(2,R1),=C'W)'                                                   
         AHI   R0,12               R0=L'INPUT                                   
*                                                                               
         LA    R3,BLOCK+20         R3=A(OUTPUT BLOCK FROM PERVAL)               
         USING PERVALD,R3                                                       
         XC    PVALOUTB,PVALOUTB   INITIALIZE BECAUSE SETTING TODAY             
         MVC   PVALCSTA,TGTODAY2   PASS TODAY'S DATE                            
*                                                                               
         GOTO1 PERVAL,DMCB,((R0),BLOCK),('PVINTOD+PVIN1DYL',(R3))               
*                                                                               
         TM    THSTAT,THSECOND     IF WE'RE NOT RUNNING SECOND NOTICES          
         BO    SETP10                                                           
         TM    TAAYHLD,X'80'       AND NOTICES ARE REQUIRED FOR MONTH           
         BZ    SETP10                                                           
         MVI   PVALPEND+2,X'31'    SET LAST DAY OF MONTH                        
         MVC   THCYCE,PVALPEND     AND SAVE IT                                  
         B     SETPX                                                            
*                                                                               
SETP10   GOTO1 GETDAY,DMCB,PVALEEND WEEKLY - GET DAY OF WEEK (SUNDAY=7)         
         ZIC   R2,0(R1)                                                         
         SH    R2,=H'7'                                                         
         LCR   R2,R2               R2=N'DAYS UNTIL SUNDAY                       
         BZ    SETP14                                                           
         GOTO1 ADDAY,DMCB,PVALEEND,PVALEEND,(R2)  GET SUNDAY OF THAT WK         
*                                                                               
SETP14   GOTO1 DATCON,DMCB,(0,PVALEEND),(1,THCYCE) SAVE IT                      
*                                                                               
SETPX    B     XIT2                                                             
         EJECT                                                                  
*              ROUTINE LOOKS TO SEE IF CLIENT IS IN THE FLIST TABLE             
*                                                                               
         USING TLCAPD,R2           R2=A(COMML HOLDING FEE POINTER)              
GETCLI   NTR1  BASE=*,LABEL=*                                                   
         L     R3,=A(CLITAB)       R3=A(TABLE)                                  
         USING FCLID,R3                                                         
GETC2    CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                (INCREASE TABLE SIZE)                        
         CLI   0(R3),0             NO MORE CLIENTS                              
         BE    NO2                                                              
         CLC   TLCAHCLI,FCLICLI    MATCH ON CLIENT                              
         BE    GETC4                                                            
         LA    R3,FCLINEXT                                                      
         B     GETC2               KEEP ON LOOKING                              
*                                                                               
GETC4    MVC   TGCLI,FCLICLI       SET CLIENT                                   
         B     YES2                RETURN CC EQ - CLIENT IN TABLE               
         DROP  R2,R3                                                            
         EJECT                                                                  
*              ROUTINE LOOKS TO SEE IF PRODUCT IS IN THE FLIST TABLE            
*                                                                               
         USING TLCAPD,R2           R2=A(COMML HOLDING FEE POINTER)              
GETPRD   NTR1  BASE=*,LABEL=*                                                   
         L     R3,=A(PRDTAB)       R3=A(TABLE)                                  
         USING FPRDD,R3                                                         
GETP2    CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                (INCREASE TABLE SIZE)                        
         CLI   0(R3),0             NO MORE PRODUCTS                             
         BE    NO2                                                              
         CLC   TLCAHPRD,FPRDPRD    MATCH ON PRODUCT                             
         BE    GETP4                                                            
         LA    R3,FPRDNEXT                                                      
         B     GETP2               KEEP ON LOOKING                              
*                                                                               
GETP4    MVC   TGPRD,FPRDPRD       SET PRODUCT                                  
         B     YES2                RETURN CC EQ - PRODUCT IN TABLE              
         DROP  R2,R3                                                            
         EJECT                                                                  
*              ROUTINE LOOKS TO SEE IF COMML IS IN NEG FLIST TABLE              
*              RETURNS CC EQ IF COMM IS IN THE TABLE                            
*                                                                               
         USING TLCAPD,R2           R2=A(COMML HOLDING FEE POINTER)              
GETNGCO  NTR1  BASE=*,LABEL=*                                                   
         L     R3,=A(COMTAB)       R3=A(TABLE)                                  
         USING FCOMD,R3                                                         
GNGCO10  CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                (INCREASE TABLE SIZE)                        
         CLC   0(4,R3),=F'00'      NO MORE COMMERCIALS                          
         BE    NO2                 RETURN CC NEQ - COMM NOT IN TABLE            
         CLC   TLCAHCOM,FCOMCOM    MATCH ON COMMERCIAL                          
         BE    YES2                RETURN CC EQ - COMM IN TABLE                 
         LA    R3,FCOMNEXT                                                      
         B     GNGCO10             KEEP ON LOOKING                              
*                                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
*              ROUTINE LOOKS TO SEE IF OFFICE IS IN THE FLIST TABLE             
*                                                                               
GETOFF   NTR1  BASE=*,LABEL=*                                                   
         USING AGYD,R3             R3=A(NEXT SLOT IN AGENCY TABLE)              
         L     R4,=A(OFFTAB)       R4=A(TABLE)                                  
         USING FOFFD,R4                                                         
GETOF2   CLI   0(R4),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                (INCREASE TABLE SIZE)                        
         CLI   0(R4),0             NO MORE OFFICES                              
         BE    NO2                                                              
         CLC   AGYOFF,FOFFOFF      MATCH ON OFFICE                              
         BE    GETOF4                                                           
         LA    R4,FOFFNEXT                                                      
         B     GETOF2              KEEP ON LOOKING                              
*                                                                               
GETOF4   MVC   TGOFF,FOFFOFF       SET OFFICE                                   
         B     YES2                RETURN CC EQ - OFFICE IN TABLE               
         DROP  R3,R4                                                            
         EJECT                                                                  
*              ROUTINE CHECKS TO SEE IF AGENCY IS NEGATIVE FLIST                
*              IF SO, IT MAKES SURE AGENCY IS NOT IN THE FLIST                  
*                                                                               
         USING TLCAPD,R2           R2=A(COMML HOLDING FEE POINTER)              
CHKNAGY  NTR1  BASE=*,LABEL=*                                                   
         L     RE,ATWA                                                          
         USING T703FFD,RE                                                       
         LA    RF,SHFAGYH                                                       
         GOTOR NEGFLIST            IF AGENCY IS A NEGATIVE FLIST                
         BNE   NO2                 AGENCY MUST NOT BE IN THE TABLE              
         DROP  RE                                                               
         L     R3,=A(FAGYTAB)      R3=A(TABLE)                                  
         USING FAGYD,R3                                                         
CNAGY10  CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                (INCREASE TABLE SIZE)                        
         CLI   0(R3),0             NO MORE AGENCIES                             
         BE    NO2                 RETURN CC NEQ - AGENCY NOT IN TABLE          
         CLC   TLCAHAGY,FAGYAGY    MATCH ON AGENCY                              
         BE    YES2                RETURN CC EQ - AGENCY IN TABLE               
         LA    R3,FAGYNEXT                                                      
         B     CNAGY10             KEEP ON LOOKING                              
*                                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
*              IF COMMERCIAL IS A POSITIVE FLIST, SET FIRST                     
*              COMMERCIAL OF FLIST INTO TIFCOM                                  
*                                                                               
GETCOM   NTR1  BASE=*,LABEL=*                                                   
         L     RE,ATWA                                                          
         USING T703FFD,RE                                                       
         LA    RF,SHFCOMH                                                       
         GOTOR POSFLIST            IF COMMERCIAL IS A POSITIVE FLIST            
         BNE   XIT2                SET FIRST COMMERCIAL IN TIFCOM               
         DROP  RE                                                               
*                                                                               
         MVC   THFLCNT,=H'1'                                                    
         L     R3,=A(COMTAB)       R3=A(TABLE)                                  
         USING FCOMD,R3                                                         
         MVC   TIFCOM,FCOMCOM      SAVE COMMERCIAL AS FILTER                    
         B     XIT2                                                             
         DROP  R3                                                               
         EJECT                                                                  
*              IF COMMERCIAL IS A POSITIVE FLIST, ROUTINE                       
*              SETS NEXT COMMERCIAL OF FLIST INTO TIFCOM                        
*                                                                               
GETNCOM  NTR1  BASE=*,LABEL=*                                                   
         L     RE,ATWA                                                          
         USING T703FFD,RE                                                       
         LA    RF,SHFCOMH                                                       
         GOTOR POSFLIST            IF COMMERCIAL IS A POSITIVE FLIST            
         BNE   NO2                 SET NEXT COMMERCIAL IN TIFCOM                
         DROP  RE                                                               
*                                                                               
         XC    TIFCOM,TIFCOM                                                    
         L     R3,=A(COMTAB)       R3=A(TABLE)                                  
         USING FCOMD,R3                                                         
         LH    R1,THFLCNT                                                       
GETNCM2  CLI   0(R3),X'FF'         END OF TABLE                                 
         BE    NO2                                                              
         CLC   0(4,R3),=F'00'      NO MORE COMMERCIALS                          
         BE    NO2                                                              
         LA    R3,FCOMNEXT                                                      
         BCT   R1,GETNCM2          KEEP ON GOING TILL WE GET A NEW ONE          
         LH    RE,THFLCNT          INCREMENT FLIST COUNTER                      
         AHI   RE,1                                                             
         STH   RE,THFLCNT                                                       
         MVC   TIFCOM,FCOMCOM      SET NEXT COMMERCIAL FILTER                   
         B     YES2                RETURN CC EQ - COMMERCIAL IN TABLE           
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE CHECKS FOR FLISTS IN CLIENT, PRODUCT OR COMM'L           
*              AND STORES THE ADDRESS OF THE CORRESPONDING FLIST                
*              TABLE INTO R4                                                    
*                                                                               
ANYFLIST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,ATWA                                                          
         USING T703FFD,R3                                                       
         LA    R2,SHFCLIH          CLIENT                                       
         L     R4,=A(CLITAB)       ADDRESS OF CLIENT FLIST TABLE                
         GOTOR VFLIST                                                           
*                                                                               
         LA    R2,SHFPRDH          PRODUCT                                      
         L     R4,=A(PRDTAB)       ADDRESS OF PRODUCT FLIST TABLE               
         GOTOR VFLIST                                                           
*                                                                               
         LA    R2,SHFCOMH          COMMERCIAL                                   
         L     R4,=A(COMTAB)       ADDRESS OF COMMERCIAL FLIST TABLE            
         GOTOR VFLIST                                                           
*                                                                               
         LA    R2,SHFOFFH          OFFICE                                       
         L     R4,=A(OFFTAB)       ADDRESS OF OFFICE FLIST TABLE                
         GOTOR VFLIST                                                           
*                                                                               
         B     XIT2                                                             
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE VALIDATES FLIST                                          
*                                                                               
VFLIST   NTR1  BASE=*,LABEL=*                                                   
         LR    RF,R2               IF FIELD IS A POSITIVE OR                    
         GOTOR POSFLIST            NEGATIVE FLIST                               
         BE    VFL5                                                             
         GOTOR NEGFLIST                                                         
         BE    VFL8                                                             
         B     XIT2                                                             
*                                                                               
VFL5     ZIC   R1,5(R2)                                                         
         SHI   R1,2                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TGLST(0),9(R2)      SKIP 1 FOR '@'                               
         OC    TGLST,SPACES                                                     
         B     VFL9                                                             
*                                                                               
VFL8     ZIC   R1,5(R2)                                                         
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TGLST(0),10(R2)      SKIP 2 FOR '@-' OR '-@'                     
         OC    TGLST,SPACES                                                     
*                                                                               
VFL9     C     R4,=A(CLITAB)                                                    
         BE    VFL10                                                            
         C     R4,=A(PRDTAB)                                                    
         BE    VFL35                                                            
         C     R4,=A(COMTAB)                                                    
         BE    VFL55                                                            
         C     R4,=A(OFFTAB)                                                    
         BE    VFL75                                                            
         DC    H'00'               R4 IS NOT POINTING AT A TABLE                
*                                                                               
VFL10    GOTOR GETFCLI             GET CLIENT FLIST                             
         BE    VFL30                                                            
VFL20    GOTOR GETNFCLI            GET NEXT CODE IN FLIST                       
         BNE   VFLCLX                                                           
VFL30    GOTOR FLTAB               STORE FLIST INTO CORRECT TABLE               
         B     VFL20                                                            
VFLCLX   XC    TIFCLI,TIFCLI                                                    
         B     XIT2                                                             
*                                                                               
VFL35    GOTOR GETFPRD             GET PRODUCT FLIST                            
         BE    VFL50                                                            
VFL40    GOTOR GETNFPRD            GET NEXT CODE IN FLIST                       
         BNE   VFLPRX                                                           
VFL50    GOTOR FLTAB               STORE FLIST INTO CORRECT TABLE               
         B     VFL40                                                            
VFLPRX   XC    TIFPRD,TIFPRD                                                    
         B     XIT2                                                             
*                                                                               
VFL55    GOTOR MULTAGY             SEE IF THERE ARE MULTIPLE AGENCIES           
         BE    VFLCOX                                                           
         GOTOR GETFCOM             GET COMMERCIAL FLIST                         
         BE    VFL70                                                            
VFL60    GOTOR GETNFCOM            GET NEXT CODE IN FLIST                       
         BNE   VFLCOX                                                           
VFL70    GOTOR FLTAB               STORE FLIST INTO CORRECT TABLE               
         B     VFL60                                                            
VFLCOX   XC    TIFCOM,TIFCOM                                                    
         B     XIT2                                                             
*                                                                               
VFL75    GOTOR GETFOFF             GET OFFICE FLIST                             
         BE    VFL90                                                            
VFL80    GOTOR GETNFOFF            GET NEXT CODE IN FLIST                       
         BNE   VFLOFX                                                           
VFL90    GOTOR FLTAB               STORE FLIST INTO CORRECT TABLE               
         B     VFL80                                                            
VFLOFX   XC    TIFOFF,TIFOFF                                                    
         B     XIT2                                                             
         EJECT                                                                  
*              ROUTINE STORES ENTRIES FROM THE FLIST INTO A TABLE               
*              R4 CONTAINS THE ADDRESS OF THE CORRESPONDING TABLE               
*                                                                               
FLTAB    NTR1  BASE=*,LABEL=*                                                   
         C     R4,=A(CLITAB)                                                    
         BE    FLT10                                                            
         C     R4,=A(PRDTAB)                                                    
         BE    FLT30                                                            
         C     R4,=A(COMTAB)                                                    
         BE    FLT50                                                            
         C     R4,=A(OFFTAB)                                                    
         BE    FLT70                                                            
         DC    H'00'               R4 IS NOT POINTING AT A TABLE                
*                                                                               
         USING FCLID,R4                                                         
FLT10    CLI   0(R4),X'FF'         R4 --> CLITAB                                
         BNE   *+6                                                              
         DC    H'0'                TABLE IS NOT BIG ENOUGH                      
         CLI   0(R4),0             NEXT SLOT                                    
         BE    FLT20                                                            
         LA    R4,FCLINEXT                                                      
         B     FLT10               GO TO NEXT OPEN SLOT                         
*                                                                               
FLT20    MVC   FCLICLI,TIFCLI      BUILD NEW ENTRY                              
         MVC   TGCLI,TIFCLI        SET TO GET CLIENT RECORD                     
         B     XIT2                                                             
         DROP  R4                                                               
*                                                                               
         USING FPRDD,R4                                                         
FLT30    CLI   0(R4),X'FF'         R4 --> PRDTAB                                
         BNE   *+6                                                              
         DC    H'0'                TABLE IS NOT BIG ENOUGH                      
         CLI   0(R4),0             NEXT SLOT                                    
         BE    FLT40                                                            
         LA    R4,FPRDNEXT                                                      
         B     FLT30               GO TO NEXT OPEN SLOT                         
*                                                                               
FLT40    MVC   FPRDPRD,TIFPRD      BUILD NEW ENTRY                              
         MVC   TGPRD,TIFPRD        SET TO GET PRODUCT RECORD                    
         B     XIT2                                                             
         DROP  R4                                                               
*                                                                               
         USING FCOMD,R4                                                         
FLT50    CLI   0(R4),X'FF'         R4 --> COMTAB                                
         BNE   *+6                                                              
         DC    H'0'                TABLE IS NOT BIG ENOUGH                      
         CLC   0(4,R4),=F'00'      NEXT SLOT                                    
         BE    FLT60                                                            
         CLC   0(4,R4),TIFCOM      IF COMM IS ALREADY IN TABLE                  
         BE    XIT2                (SAME COMM, DIFF VERSION) SKIP IT            
         LA    R4,FCOMNEXT                                                      
         B     FLT50               GO TO NEXT OPEN SLOT                         
*                                                                               
FLT60    MVC   FCOMCOM,TIFCOM      BUILD NEW ENTRY                              
         MVC   TGCOM,TIFCOM        SET TO GET COMM RECORD                       
         B     XIT2                                                             
         DROP  R4                                                               
*                                                                               
         USING FOFFD,R4                                                         
FLT70    CLI   0(R4),X'FF'         R4 --> OFFTAB                                
         BNE   *+6                                                              
         DC    H'0'                TABLE IS NOT BIG ENOUGH                      
         CLC   0(4,R4),=F'00'      NEXT SLOT                                    
         BE    FLT80                                                            
         LA    R4,FOFFNEXT                                                      
         B     FLT70               GO TO NEXT OPEN SLOT                         
*                                                                               
FLT80    MVC   FOFFOFF,TIFOFF      BUILD NEW ENTRY                              
         MVC   TGOFF,TIFOFF        SET TO GET OFFICE RECORD                     
         B     XIT2                                                             
         DROP  R4                                                               
*              ROUTINE CHECKS IF THERE IS AN AGENCY FLIST. IF SO IT             
*              WILL SET ONE AGENCY AT A TIME AS TGAGY                           
*                                                                               
MULTAGY  NTR1  BASE=*,LABEL=*                                                   
         L     R2,ATWA                                                          
         USING T703FFD,R2                                                       
         LA    RF,SHFAGYH                                                       
         DROP  R2                                                               
         GOTOR POSFLIST                                                         
         BE    MLTAGY10                                                         
         GOTOR NEGFLIST                                                         
         BNE   NO2                 EXIT IF AGENCY IS NOT AN FLIST               
         GOTOR NEGMAGY             NEGATIVE AGENCY FLIST                        
         B     MLTAGYX                                                          
*                                                                               
MLTAGY10 L     R3,=A(AGYTAB)       POSITIVE AGENCY FLIST                        
         USING AGYD,R3             READ COMM RECORD FOR EVERY AGENCY            
         B     *+8                 IN FLIST                                     
MLTAGY15 LA    R3,AGYNEXT                                                       
         CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'00'               (INCREASE NAGY)                              
         CLI   0(R3),0             NEXT EMPTY SLOT                              
         BE    MLTAGYX             NO MORE AGENCIES                             
         MVC   TGAGY,AGYAGY        SET AGENCY FOR COMM RECORD                   
*                                                                               
         GOTOR GETFCOM                                                          
         BE    MLTAGY20                                                         
MLTAGY18 GOTOR GETNFCOM                                                         
         BNE   MLTAGY15            NO MORE COMM, TRY NEXT AGENCY                
MLTAGY20 GOTOR FLTAB               STORE FLIST IN CORRECT TABLE                 
         B     MLTAGY18                                                         
*                                                                               
MLTAGYX  B     YES2                                                             
         EJECT                                                                  
*              ROUTINE SETS EVERY AGENCY RECORD TO TGAGY EXCEPT FOR             
*              AGENCIES IN THE NEGATIVE AGENCY FLIST                            
*                                                                               
NEGMAGY  NTR1  BASE=*,LABEL=*                                                   
         L     R2,ATWA                                                          
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING TLAYD,R1                                                         
         MVI   TLAYCD,TLAYCDQ                                                   
         GOTO1 HIGH                                                             
NGMAGY10 CLC   KEY(1),KEYSAVE      IF NO MORE AGENCY RECORDS, EXIT              
         BNE   XIT2                                                             
*                                                                               
         USING FAGYD,R3                                                         
         L     R3,=A(FAGYTAB)      NEGATIVE AGENCY FLIST                        
         B     *+8                                                              
NGMAGY20 LA    R3,FAGYNEXT                                                      
         CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'00'               (INCREASE NAGY)                              
         CLI   0(R3),0             IF NO MORE, AGENCY NOT IN TABLE              
         BE    NGMAGY40                                                         
         CLC   FAGYAGY,TLAYAGY     IS THIS AGENCY IN NEG AGY FLIST?             
         BNE   NGMAGY20            IF IT IS, SKIP IT                            
NGMAGY30 GOTO1 SEQ                                                              
         B     NGMAGY10                                                         
*                                                                               
NGMAGY40 MVC   TGAGY,TLAYAGY       SET AGENCY FOR COMM RECORD                   
         GOTOR GETFCOM                                                          
         BE    NGMAGY60                                                         
NGMAGY50 GOTOR GETNFCOM                                                         
         BNE   NGMAGY30            NO MORE COMM, TRY NEXT AGENCY                
NGMAGY60 GOTOR FLTAB               STORE FLIST IN CORRECT TABLE                 
         B     NGMAGY50                                                         
*                                                                               
         DROP  R1                                                               
         EJECT                                                                  
*                                                                               
YES2     SR    RC,RC               SET CONDITION CODE                           
NO2      LTR   RC,RC                                                            
XIT2     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CHECKS FOR FLIST.  SETS CC NEQ IF 1ST CODE IN            
*              FLIST IS INVALID, ELSE SETS CC EQ                                
*                                                                               
GETFFLST NTR1  BASE=*,LABEL=*                                                   
         XC    THSVFILT,THSVFILT                                                
         MVC   THFLCNT,=H'1'       FLIST COUNTER                                
         XC    TIFAGY,TIFAGY                                                    
         MVC   AIO,AIO2            SET AIO AND READ FLIST                       
         MVI   TGLTYP,TLGLTYPF                                                  
         GOTO1 RECVAL,DMCB,TLGLCDQ,(X'A0',0)                                    
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R4,AIO2                                                          
         MVI   ELCODE,TAGLELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAGLD,R4                                                         
         ZIC   R1,TAGLLEN                                                       
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TIFAGY(0),TAGLDATA  CHECK THAT AGENCY EXISTS                     
         OC    TIFAGY,SPACES                                                    
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'80',TIFAGY)                               
         BE    YES3                IF VALID RETURN CC EQ                        
*                                                                               
         GOTOR AGYWILD             CHECK FOR WILDCARD AGENCY                    
         B     NO3                 ALWAYS RETURN CC NEQ HERE                    
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CHECK IF ANOTHER REQUEST IS NECESSARY                 
*                                                                               
         USING TAGLD,R4            R4=A(CURRENT TAGLD ELEMENT)                  
GETNFLST NTR1  BASE=*,LABEL=*                                                   
GETNF5   XC    THSVFILT,THSVFILT                                                
         MVC   AIO,AIO2            SET AIO AND READ FLIST                       
         MVI   TGLTYP,TLGLTYPF                                                  
         GOTO1 RECVAL,DMCB,TLGLCDQ,(X'A0',0)                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   ELCODE,TAGLELQ      RESET ELEMENT CODE                           
         LH    R3,THFLCNT                                                       
         L     R4,AIO2                                                          
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
GETNF10  BRAS  RE,NEXTEL           GET NEXT ONE                                 
         BNE   NO3                                                              
         BCT   R3,GETNF10          KEEP ADVANCING TILL WE GET A NEW ONE         
*                                                                               
         LH    RE,THFLCNT          INCREMENT FLIST COUNTER                      
         AHI   RE,1                                                             
         STH   RE,THFLCNT                                                       
*                                                                               
         LHI   R3,1                IN CASE WE GO BACK TO THE BCT LOOP           
*                                                                               
         ZIC   R1,TAGLLEN          IF CODE IS EQUAL TO PREVIOUS                 
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TIFAGY(0),TAGLDATA                                               
         BE    GETNF10             SKIP IT                                      
*                                                                               
         XC    TIFAGY,TIFAGY       SAVE NEXT AGENCY FROM FLIST                  
         ZIC   R1,TAGLLEN                                                       
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TIFAGY(0),TAGLDATA  CHECK THAT AGENCY EXISTS                     
         OC    TIFAGY,SPACES                                                    
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'80',TIFAGY)                               
         BE    YES3                SET CC EQ                                    
         GOTOR AGYWILD             IF INVALID, CHECK FOR WILDCARD AGY           
         B     GETNF5              GET NEXT ELEMENT (MUST RESTORE AIO)          
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CHECKS FOR CLIENT FLIST. SETS CC NEQ IF 1ST              
*              CODE IN FLIST IS INVALID, ELSE SETS CC EQ                        
*                                                                               
GETFCLI  NTR1  BASE=*,LABEL=*                                                   
         XC    THSVFILT,THSVFILT                                                
         MVC   THFLCNT,=H'1'       FLIST COUNTER                                
         XC    TIFCLI,TIFCLI                                                    
         MVC   AIO,AIO2            SET AIO AND READ FLIST                       
         MVI   TGLTYP,TLGLTYPF                                                  
         GOTO1 RECVAL,DMCB,TLGLCDQ,(X'A0',0)                                    
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R4,AIO2                                                          
         MVI   ELCODE,TAGLELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAGLD,R4                                                         
         ZIC   R1,TAGLLEN                                                       
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TIFCLI(0),TAGLDATA  CHECK THAT CLIENT EXISTS                     
         OC    TIFCLI,SPACES                                                    
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'80',TIFCLI)                               
         BE    YES3                IF VALID RETURN CC EQ                        
*                                                                               
         GOTOR CLIWILD             CHECK FOR WILDCARD CLIENT                    
         B     NO3                 ALWAYS RETURN CC NOT EQ HERE                 
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CHECK IF ANOTHER REQUEST IS NECESSARY                 
*                                                                               
         USING TAGLD,R4            R4=A(CURRENT TAGLD ELEMENT)                  
GETNFCLI NTR1  BASE=*,LABEL=*                                                   
GETNC5   XC    THSVFILT,THSVFILT                                                
         MVC   AIO,AIO2            SET AIO AND READ FLIST                       
         MVI   TGLTYP,TLGLTYPF                                                  
         GOTO1 RECVAL,DMCB,TLGLCDQ,(X'A0',0)                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   ELCODE,TAGLELQ      RESET ELEMENT CODE                           
         LH    R3,THFLCNT                                                       
         L     R4,AIO2                                                          
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
GETNC10  BRAS  RE,NEXTEL           GET NEXT ONE                                 
         BNE   NO3                                                              
         BCT   R3,GETNC10          KEEP ADVANCING TILL WE GET A NEW ONE         
*                                                                               
         LH    RE,THFLCNT          INCREMENT FLIST COUNTER                      
         AHI   RE,1                                                             
         STH   RE,THFLCNT                                                       
*                                                                               
         LHI   R3,1                IN CASE WE GO BACK TO THE BCT LOOP           
*                                                                               
         ZIC   R1,TAGLLEN          IF CODE IS EQUAL TO PREVIOUS                 
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TIFCLI(0),TAGLDATA                                               
         BE    GETNC10             SKIP IT                                      
*                                                                               
         XC    TIFCLI,TIFCLI       SAVE NEXT CLIENT FROM FLIST                  
         ZIC   R1,TAGLLEN                                                       
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TIFCLI(0),TAGLDATA  CHECK THAT CLIENT EXISTS                     
         OC    TIFCLI,SPACES                                                    
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'80',TIFCLI)                               
         BE    YES3                SET CC EQ                                    
         GOTOR CLIWILD             IF INVALID CHECK FOR WILDCARD CLI            
         B     GETNC5              GET NEXT ELEMENT (RESTORE AIO)               
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CHECKS FOR PRODUCT FLIST. SETS CC NEQ IF 1ST             
*              CODE IN FLIST IS INVALID, ELSE SETS CC EQ                        
*                                                                               
GETFPRD  NTR1  BASE=*,LABEL=*                                                   
         XC    THSVFILT,THSVFILT                                                
         MVC   THFLCNT,=H'1'       FLIST COUNTER                                
         XC    TIFPRD,TIFPRD                                                    
         MVC   AIO,AIO2            SET AIO AND READ FLIST                       
         MVI   TGLTYP,TLGLTYPF                                                  
         GOTO1 RECVAL,DMCB,TLGLCDQ,(X'A0',0)                                    
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R4,AIO2                                                          
         MVI   ELCODE,TAGLELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAGLD,R4                                                         
         ZIC   R1,TAGLLEN                                                       
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TIFPRD(0),TAGLDATA  CHECK THAT PRODUCT EXISTS                    
         OC    TIFPRD,SPACES                                                    
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'80',TIFPRD)                               
         BE    YES3                IF VALID RETURN CC EQ                        
*                                                                               
         GOTOR PRDWILD             CHECK FOR WILDCARD PRODUCT                   
         B     NO3                 ALWAYS RETURN CC NEQ HERE                    
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CHECK IF ANOTHER REQUEST IS NECESSARY                 
*                                                                               
         USING TAGLD,R4            R4=A(CURRENT TAGLD ELEMENT)                  
GETNFPRD NTR1  BASE=*,LABEL=*                                                   
GETNP5   XC    THSVFILT,THSVFILT                                                
         MVC   AIO,AIO2            SET AIO AND READ FLIST                       
         MVI   TGLTYP,TLGLTYPF                                                  
         GOTO1 RECVAL,DMCB,TLGLCDQ,(X'A0',0)                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   ELCODE,TAGLELQ      RESET ELEMENT CODE                           
         LH    R3,THFLCNT                                                       
         L     R4,AIO2                                                          
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
GETNP10  BRAS  RE,NEXTEL           GET NEXT ONE                                 
         BNE   NO3                                                              
         BCT   R3,GETNP10          KEEP ADVANCING TILL WE GET A NEW ONE         
*                                                                               
         LH    RE,THFLCNT          INCREMENT FLIST COUNTER                      
         AHI   RE,1                                                             
         STH   RE,THFLCNT                                                       
*                                                                               
         LHI   R3,1                IN CASE WE GO BACK TO THE BCT LOOP           
*                                                                               
         ZIC   R1,TAGLLEN          IF CODE IS EQUAL TO PREVIOUS                 
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TIFPRD(0),TAGLDATA                                               
         BE    GETNP10             SKIP IT                                      
*                                                                               
         XC    TIFPRD,TIFPRD       SAVE NEXT PRODUCT FROM FLIST                 
         ZIC   R1,TAGLLEN                                                       
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TIFPRD(0),TAGLDATA  CHECK THAT PRODUCT EXISTS                    
         OC    TIFPRD,SPACES                                                    
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'80',TIFPRD)                               
         BE    YES3                SET CC EQ                                    
         GOTOR PRDWILD             IF INVALID CHECK FOR WILDCARD PRD            
         B     GETNP5              GET NEXT ELEMENT (RESTORE AIO)               
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CHECKS FOR COMMERCIAL FLIST. SETS CC NEQ IF 1ST          
*              CODE IN FLIST IS INVALID, ELSE SETS CC EQ                        
*                                                                               
GETFCOM  NTR1  BASE=*,LABEL=*                                                   
         XC    THSVFILT,THSVFILT                                                
         MVC   THFLCNT,=H'1'       FLIST COUNTER                                
         XC    TIFCOM,TIFCOM                                                    
         XC    BLOCK(12),BLOCK                                                  
         MVC   AIO,AIO2            SET AIO AND READ FLIST                       
         MVI   TGLTYP,TLGLTYPF                                                  
         GOTO1 RECVAL,DMCB,TLGLCDQ,(X'A0',0)                                    
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R4,AIO2                                                          
         MVI   ELCODE,TAGLELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAGLD,R4                                                         
         ZIC   R1,TAGLLEN                                                       
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK(0),TAGLDATA  CHECK THAT COMMERCIAL EXISTS                  
         OC    BLOCK(12),SPACES                                                 
*                                                                               
         OC    TGCLI,TGCLI         IF WE DON'T HAVE THE CLIENT OR               
         BZ    GETFC10             PRODUCT, READ PASSIVE KEY                    
         OC    TGPRD,TGPRD                                                      
         BZ    GETFC10                                                          
         GOTO1 RECVAL,DMCB,TLCOCDQ,(X'80',BLOCK)                                
         BNE   GETFC20                                                          
         USING TLCOD,R5                                                         
         LA    R5,KEY                                                           
         MVC   TIFCOM,TLCOCOM      INTERNAL COMMERCIAL NUMBER                   
         B     YES3                SET CC EQ                                    
         DROP  R5                                                               
*                                                                               
GETFC10  GOTO1 RECVAL,DMCB,TLCOICDQ,(X'80',BLOCK)                               
         BNE   GETFC20                                                          
         USING TLCOPD,R5                                                        
         LA    R5,KEY                                                           
         MVC   TIFCOM,TLCOICOM     INTERNAL COMMERCIAL NUMBER                   
         B     YES3                SET CC EQ                                    
*                                                                               
GETFC20  GOTOR COMWILD             CHECK FOR WILDCARD COMMERCIAL                
         B     NO3                 ALWAYS RETURN CC NEQ HERE                    
         DROP  R5                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CHECK IF ANOTHER REQUEST IS NECESSARY                 
*                                                                               
         USING TAGLD,R4            R4=A(CURRENT TAGLD ELEMENT)                  
GETNFCOM NTR1  BASE=*,LABEL=*                                                   
GETCO5   XC    THSVFILT,THSVFILT                                                
         MVC   AIO,AIO2            SET AIO AND READ FLIST                       
         MVI   TGLTYP,TLGLTYPF                                                  
         GOTO1 RECVAL,DMCB,TLGLCDQ,(X'A0',0)                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   ELCODE,TAGLELQ      RESET ELEMENT CODE                           
         LH    R3,THFLCNT                                                       
         L     R4,AIO2                                                          
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
GETCO10  BRAS  RE,NEXTEL           GET NEXT ONE                                 
         BNE   NO3                                                              
         BCT   R3,GETCO10          KEEP ADVANCING TILL WE GET A NEW ONE         
*                                                                               
         LH    RE,THFLCNT          INCREMENT FLIST COUNTER                      
         AHI   RE,1                                                             
         STH   RE,THFLCNT                                                       
*                                                                               
         LHI   R3,1                IN CASE WE GO BACK TO THE BCT LOOP           
*                                                                               
         ZIC   R1,TAGLLEN          IF CODE IS EQUAL TO PREVIOUS                 
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   BLOCK(0),TAGLDATA                                                
         BE    GETCO10             SKIP IT                                      
*                                                                               
         XC    TIFCOM,TIFCOM       SAVE NEXT COMMERCIAL FROM FLIST              
         XC    BLOCK(12),BLOCK                                                  
         ZIC   R1,TAGLLEN                                                       
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK(0),TAGLDATA    CHECK THAT COMMERCIAL EXISTS                
         OC    BLOCK(12),SPACES                                                 
*                                                                               
         OC    TGCLI,TGCLI         IF WE DON'T HAVE THE CLIENT OR               
         BZ    GETCO20             PRODUCT, READ PASSIVE KEY                    
         OC    TGPRD,TGPRD                                                      
         BZ    GETCO20                                                          
         GOTO1 RECVAL,DMCB,TLCOCDQ,(X'80',BLOCK)                                
         BNE   GETCO30             INVALID?                                     
         LA    R5,KEY                                                           
         USING TLCOD,R5                                                         
         MVC   TIFCOM,TLCOCOM      INTERNAL COMMERCIAL NUMBER                   
         B     YES3                SET CC EQ                                    
         DROP  R5                                                               
*                                                                               
         USING TLCOPD,R5                                                        
GETCO20  GOTO1 RECVAL,DMCB,TLCOICDQ,(X'80',BLOCK)                               
         BNE   GETCO30             INVALID?                                     
         LA    R5,KEY                                                           
         MVC   TIFCOM,TLCOICOM     INTERNAL COMMERCIAL NUMBER                   
         B     YES3                SET CC EQ                                    
*                                                                               
GETCO30  GOTOR COMWILD             IF INVALID, CHECK FOR WILDCARD COMM          
         B     GETCO5              GET NEXT ELEMENT (RESTORE AIO)               
         DROP  R5                                                               
         EJECT                                                                  
*              ROUTINE CHECKS FOR OFFICE FLIST. SETS CC NEQ IF 1ST              
*              CODE IN FLIST IS INVALID, ELSE SETS CC EQ                        
*                                                                               
GETFOFF  NTR1  BASE=*,LABEL=*                                                   
         XC    THSVFILT,THSVFILT                                                
         MVC   THFLCNT,=H'1'       FLIST COUNTER                                
         XC    TIFOFF,TIFOFF                                                    
         MVC   AIO,AIO2            SET AIO AND READ FLIST                       
         MVI   TGLTYP,TLGLTYPF                                                  
         GOTO1 RECVAL,DMCB,TLGLCDQ,(X'A0',0)                                    
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R4,AIO2                                                          
         MVI   ELCODE,TAGLELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAGLD,R4                                                         
         ZIC   R1,TAGLLEN                                                       
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TIFOFF(0),TAGLDATA  CHECK THAT OFFICE EXISTS                     
         OC    TIFOFF,SPACES                                                    
         GOTO1 RECVAL,DMCB,TLOFCDQ,(X'80',TIFOFF)                               
         BE    YES3                IF VALID RETURN CC EQ                        
         B     NO3                                                              
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CHECK IF ANOTHER REQUEST IS NECESSARY                 
*                                                                               
         USING TAGLD,R4            R4=A(CURRENT TAGLD ELEMENT)                  
GETNFOFF NTR1  BASE=*,LABEL=*                                                   
GETNO5   XC    THSVFILT,THSVFILT                                                
         MVC   AIO,AIO2            SET AIO AND READ FLIST                       
         MVI   TGLTYP,TLGLTYPF                                                  
         GOTO1 RECVAL,DMCB,TLGLCDQ,(X'A0',0)                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   ELCODE,TAGLELQ      RESET ELEMENT CODE                           
         LH    R3,THFLCNT                                                       
         L     R4,AIO2                                                          
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
GETNO10  BRAS  RE,NEXTEL           GET NEXT ONE                                 
         BNE   NO3                                                              
         BCT   R3,GETNO10          KEEP ADVANCING TILL WE GET A NEW ONE         
*                                                                               
         LH    RE,THFLCNT          INCREMENT FLIST COUNTER                      
         AHI   RE,1                                                             
         STH   RE,THFLCNT                                                       
*                                                                               
         LHI   R3,1                IN CASE WE GO BACK TO THE BCT LOOP           
*                                                                               
         ZIC   R1,TAGLLEN          IF CODE IS EQUAL TO PREVIOUS                 
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TIFOFF(0),TAGLDATA                                               
         BE    GETNO10             SKIP IT                                      
*                                                                               
         XC    TIFOFF,TIFOFF       SAVE NEXT OFFICE FROM FLIST                  
         ZIC   R1,TAGLLEN                                                       
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TIFOFF(0),TAGLDATA  CHECK THAT OFFICE EXISTS                     
         OC    TIFOFF,SPACES                                                    
         GOTO1 RECVAL,DMCB,TLOFCDQ,(X'80',TIFOFF)                               
         BE    YES3                SET CC EQ                                    
         B     GETNO5              GET NEXT ELEMENT (RESTORE AIO)               
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CHECK IF AGENCY IS A WILDCARD AND                     
*              STORE WILDCARD AGENCIES IN AGYTAB OF FAGYTAB                     
AGYWILD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TIFAGY                                                        
         LA    R3,6                CHECK 6 CHARACTERS                           
         BAS   RE,CKWILD           IS THIS A WILDCARD?                          
         BNE   NO3                 IF INVALID, RETURN CC NOT EQ                 
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLAYD,R4                                                         
         MVI   TLAYCD,TLAYCDQ                                                   
         GOTO1 HIGH                READ FIRST AGENCY                            
AGYWLD10 CLC   KEY(1),KEYSAVE                                                   
         BNE   XIT3                                                             
         LA    R3,6                                                             
         LA    R2,TLAYAGY                                                       
         BAS   RE,COMPWILD                                                      
         BNE   AGYWLD20                                                         
         MVC   THSVFILT(6),TIFAGY  SAVE AGENCY FILTER                           
         MVC   TIFAGY,TLAYAGY                                                   
         DROP  R4                                                               
         L     RE,ATWA                                                          
         USING T703FFD,RE                                                       
         LA    RF,SHFAGYH                                                       
         DROP  RE                                                               
         GOTOR POSFLIST                                                         
         BNE   AGYWLD15            IF POSITIVE FLIST,                           
         GOTOR AGFLTAB             STORE FLIST AGENCIES INTO AGYTAB             
         B     AGYWLD18                                                         
AGYWLD15 GOTOR NAGFLTAB            IF NEG FLIST,STORE AGY'S IN FAGYTAB          
*                                                                               
AGYWLD18 MVC   TIFAGY,THSVFILT     RESTORE AGENCY FILTER                        
AGYWLD20 GOTO1 SEQ                                                              
         B     AGYWLD10                                                         
         EJECT                                                                  
*              ROUTINE TO CHECK IF CLIENT IS A WILDCARD AND                     
*              STORE WILDCARD CLIENTS IN CLITAB                                 
CLIWILD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TIFCLI                                                        
         LA    R3,6                CHECK 6 CHARACTERS                           
         BAS   RE,CKWILD           IS THIS A WILDCARD?                          
         BNE   NO3                 IF INVALID, RETURN CC NOT EQ                 
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLCLD,R4                                                         
         MVI   TLCLCD,TLCLCDQ                                                   
         MVC   TLCLAGY,TGAGY                                                    
         GOTO1 HIGH                READ FIRST CLIENT                            
CLIWLD10 CLC   KEY(TLCLCLI-TLCLD),KEYSAVE                                       
         BNE   XIT3                                                             
         LA    R3,6                                                             
         LA    R2,TLCLCLI                                                       
         BAS   RE,COMPWILD                                                      
         BNE   CLIWLD20                                                         
         MVC   THSVFILT(6),TIFCLI  SAVE CLIENT FILTER                           
         MVC   TIFCLI,TLCLCLI                                                   
         DROP  R4                                                               
         L     R4,=A(CLITAB)                                                    
         GOTOR FLTAB               STORE FLIST CLIENTS INTO CLITAB              
         MVC   TIFCLI,THSVFILT     RESTORE CLIENT FILTER                        
         LA    R4,KEY                                                           
CLIWLD20 GOTO1 SEQ                                                              
         B     CLIWLD10                                                         
         EJECT                                                                  
*              ROUTINE TO CHECK IF PRODUCT IS A WILDCARD AND                    
*              STORE WILDCARD PRODUCTS IN PRDTAB                                
PRDWILD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TIFPRD                                                        
         LA    R3,6                CHECK 6 CHARACTERS                           
         BAS   RE,CKWILD           IS THIS A WILDCARD?                          
         BNE   NO3                 IF INVALID, RETURN CC NOT EQ                 
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLPRD,R4                                                         
         MVI   TLPRCD,TLPRCDQ                                                   
         MVC   TLPRAGY,TGAGY                                                    
         MVC   TLPRCLI,TGCLI                                                    
         GOTO1 HIGH                READ FIRST PRODUCT                           
PRDWLD10 CLC   KEY(TLPRPRD-TLPRD),KEYSAVE                                       
         BNE   XIT3                                                             
         LA    R3,6                                                             
         LA    R2,TLPRPRD                                                       
         BAS   RE,COMPWILD                                                      
         BNE   PRDWLD20                                                         
         MVC   THSVFILT(6),TIFPRD  SAVE PRODUCT FILTER                          
         MVC   TIFPRD,TLPRPRD                                                   
         DROP  R4                                                               
         L     R4,=A(PRDTAB)                                                    
         GOTOR FLTAB               STORE FLIST PRODUCTS INTO PRDTAB             
         MVC   TIFPRD,THSVFILT     RESTORE PRODUCT FILTER                       
         LA    R4,KEY                                                           
PRDWLD20 GOTO1 SEQ                                                              
         B     PRDWLD10                                                         
         EJECT                                                                  
*              ROUTINE TO CHECK IF COMMERCIAL IS A WILDCARD AND                 
*              STORE WILDCARD COMMERCIALS IN COMTAB                             
COMWILD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,BLOCK                                                         
         LA    R3,12               CHECK 12 CHARACTERS                          
         BAS   RE,CKWILD           IS THIS A WILDCARD?                          
         BNE   NO3                 IF INVALID, RETURN CC NOT EQ                 
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         OC    TGCLI,TGCLI         IF WE DON'T HAVE CLIENT OR PRODUCT,          
         BZ    COMWLD30            WE MUST READ THE PASSIVE KEY                 
         OC    TGPRD,TGPRD                                                      
         BZ    COMWLD30                                                         
*                                                                               
         USING TLCOD,R4                                                         
         MVI   TLCOCD,TLCOCDQ                                                   
         MVC   TLCOAGY,TGAGY                                                    
         MVC   TLCOCLI,TGCLI                                                    
         MVC   TLCOPRD,TGPRD                                                    
         GOTO1 HIGH                READ FIRST COMMERCIAL                        
COMWLD10 CLC   KEY(TLCOCID-TLCOD),KEYSAVE                                       
         BNE   XIT3                                                             
         LA    R2,TLCOCID                                                       
         LA    R3,8                8 CHARARCTERS FOR COMMERCIAL ID              
         BAS   RE,COMPWILD                                                      
         BNE   COMWLD20                                                         
         MVC   THSVFILT(12),BLOCK   SAVE COMMERCIAL FILTER                      
         MVC   TIFCOM,TLCOCOM                                                   
         DROP  R4                                                               
         L     R4,=A(COMTAB)                                                    
         GOTOR FLTAB               STORE FLIST COMMERCIALS INTO COMTAB          
         MVC   BLOCK,THSVFILT      RESTORE COMMERCIAL FILTER                    
         LA    R4,KEY                                                           
COMWLD20 GOTO1 SEQ                                                              
         B     COMWLD10                                                         
*                                                                               
         USING TLCOPD,R4                                                        
COMWLD30 MVI   TLCOPCD,TLCOICDQ    PASSIVE KEY                                  
         MVC   TLCOIAGY,TGAGY                                                   
         GOTO1 HIGH                READ FIRST COMMERCIAL                        
COMWLD40 CLC   KEY(TLCOICID-TLCOPD),KEYSAVE                                     
         BNE   XIT3                                                             
         LA    R2,TLCOICID                                                      
         LA    R3,12               12 CHARARCTERS FOR COMMERCIAL ID             
         BAS   RE,COMPWILD                                                      
         BNE   COMWLD50                                                         
         MVC   THSVFILT(12),BLOCK  SAVE COMMERCIAL FILTER                       
         MVC   TIFCOM,TLCOICOM                                                  
         DROP  R4                                                               
         L     R4,=A(COMTAB)                                                    
         GOTOR FLTAB               STORE FLIST COMMERCIALS INTO COMTAB          
         MVC   BLOCK,THSVFILT      RESTORE COMMERCIAL FILTER                    
         LA    R4,KEY                                                           
COMWLD50 GOTO1 SEQ                                                              
         B     COMWLD40                                                         
         EJECT                                                                  
*              ROUTINE TO CHECK FOR WILD CARD                                   
*              R3=NUMBER OF CHARS, R1-->FILTER                                  
CKWILD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
CKW10    CLI   0(R1),C'*'          IF THERE IS A '*' THEN THIS                  
         BE    YES3                IS A WILDCARD                                
         LA    R1,1(R1)            BUMP TO NEXT LETTER                          
         BCT   R3,CKW10                                                         
         B     NO3                                                              
         EJECT                                                                  
*              ROUTINE TO COMPARE WITH WILDCARD                                 
*              R3=NUMBER OF CHARS, R1-->FILTER, R2-->FIELD IN RECORD            
COMPWILD NTR1  BASE=*,LABEL=*                                                   
*                                                                               
CMPW10   CLI   0(R1),C'*'          IF THIS IS A WILD CARD                       
         BE    CMPW20              THEN DON'T COMPARE LETTERS                   
         CLC   0(1,R1),0(R2)       IF THE LETTER IS DIFFERENT                   
         BNE   NO3                 RETURN CC NEQ                                
*                                                                               
CMPW20   LA    R1,1(R1)            BUMP TO NEXT LETTER                          
         LA    R2,1(R2)                                                         
         BCT   R3,CMPW10                                                        
         B     YES3                                                             
         EJECT                                                                  
*              ROUTINE CHECKS IF FIELD IS A POSITIVE FLIST                      
*              RF =A(FIELD HEADER)                                              
*                                                                               
POSFLIST NTR1  BASE=*,LABEL=*                                                   
         CLI   8(RF),C'@'          POSITIVE FLIST?                              
         BNE   NO3                                                              
         CLI   9(RF),C'-'          MAKE SURE ITS NOT NEGATIVE                   
         BE    NO3                                                              
         B     YES3                                                             
         EJECT                                                                  
*              ROUTINE CHECKS IF FIELD IS A NEGATIVE FLIST                      
*              RF =A(FIELD HEADER)                                              
*                                                                               
NEGFLIST NTR1  BASE=*,LABEL=*                                                   
         CLC   8(2,RF),=C'-@'        NEGATIVE FLIST?                            
         BE    YES3                                                             
         CLC   8(2,RF),=C'@-'        NEGATIVE FLIST?                            
         BE    YES3                                                             
         B     NO3                                                              
         EJECT                                                                  
YES3     SR    RC,RC               SET CONDITION CODE                           
NO3      LTR   RC,RC                                                            
XIT3     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        ROUTINE MULTIPLIES WHAT IS CONTAINED IN R2 BY % IN HALF                
*        RETURNS AMOUNT IN FULL                                                 
*                                                                               
         SPACE 1                                                                
MULTP    NTR1  BASE=*,LABEL=*                                                   
         LH    RE,HALF             RATE                                         
*                                                                               
         LR    R3,R2                                                            
         XR    R2,R2                                                            
*                                                                               
         MR    R2,RE               R2,R3 = RE * R3                              
         LHI   RF,5000                                                          
         DR    R2,RF                                                            
         LTR   R3,R3               R3 = QUOTIENT                                
         BM    *+8                                                              
         AHI   R3,1                ROUND                                        
         SRA   R3,1                                                             
         ST    R3,FULL             RETURN CALC FOR 1                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO GET FROM DATASET AND PRINT SECOND REPORT              
*                                                                               
PREPD    NTR1  BASE=*,LABEL=*                                                   
         L     R2,ATWA                                                          
         USING T703FFD,R2                                                       
         MVC   AMASTD,TWAMASTC     SAVE ADDRESS OF MASTER                       
         L     R2,TWADCONS                                                      
         DROP  R2                                                               
*                                                                               
         USING TWADCOND,R2                                                      
         MVC   ALOGOC,TLOGOC       SAVE A(LOGOC)                                
         L     R2,AMASTD                                                        
         USING MASTD,R2                                                         
         MVC   ALOGO,MCVLOGO       SAVE A(LOGO)                                 
         MVC   AREMOT,MCVREMOT     SAVE A(REMOTE)                               
*                                                                               
         BAS   RE,NEWPRTQ           SET NEW PRINT QUEUE REPORT                  
         GOTO1 REQTWA,DMCB,(3,ATWA),,VPRINT,(C'B',ABOX)                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   FORCEHED,C'Y'                                                    
         BRAS  RE,MYSPOOL                                                       
*                                                                               
         L     R2,AMASTD            DO NOT PRINT LOGOS                          
         USING MASTD,R2                                                         
         NI    MCPRTIND,X'FF'-MCPRTINL                                          
         DROP  R2                                                               
*                                                                               
         L     R2,=A(TADOWN)                                                    
         OPEN  ((2),INPUT)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,DLBLOCK                                                       
         USING DLCBD,R3                                                         
         BAS   RE,INITDWN                                                       
*                                                                               
PREPD2   GET   (R2),DISKREC        GET REC FROM TEMP DATASET                    
         LA    R5,DISKREC                                                       
         LA    R0,2                2*40+18=98                                   
*                                                                               
PREPD3   MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
         MVI   DLCBLEN,40                                                       
         MVC   DLCBFLD(40),0(R5)    PASS 40 BYTES                               
         GOTO1 =V(DLFLD),DLCBD                                                  
*                                                                               
         LA    R5,40(R5)           BUMP TO NEXT TAPE FIELD                      
         BCT   R0,PREPD3                                                        
*                                                                               
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
         MVI   DLCBLEN,18                                                       
         MVC   DLCBFLD(18),0(R5)   PASS LAST 18 BYTES                           
         GOTO1 =V(DLFLD),DLCBD                                                  
*                                                                               
         MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 =V(DLFLD),DLCBD                                                  
         B     PREPD2                                                           
*                                                                               
NOMORE   CLOSE ((2))               CLOSE THE DATASET                            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   DLCBACT,DLCBEOR                                                  
         GOTO1 =V(DLFLD),DLCBD                                                  
PREPDX   XIT1                                                                   
         SPACE                                                                  
AMASTD   DS    A                   A(MASTER)                                    
ALOGOC   DS    A                   A(LOGOC)                                     
ALOGO    DS    A                   A(LOGO)                                      
AREMOT   DS    A                   A(REMOTE)                                    
         EJECT                                                                  
*              ROUTINE TO SET UP SECOND REPORT ON QUEUE                         
*                                                                               
NEWPRTQ  NTR1                                                                   
         XC    SPECS,SPECS         CLEAR SPECS                                  
         XC    HEADHOOK,HEADHOOK   AND HEADLINE HOOK                            
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         BRAS  RE,MYSPOOL                                                       
*                                                                               
         L     R2,ALOGOC                                                        
         USING LOGOD,R2                                                         
         MVI   LOGOTYPE,C'E'                                                    
         GOTO1 ALOGO,DMCB,(R2)     END REPORT LOGOS                             
*                                                                               
         TM    WHEN,X'20'          SOON?                                        
         BO    NPQ010                                                           
         GOTO1 VPRINT,DMCB,=C'CLOSE'                                            
*                                                                               
NPQ010   L     R2,ABOX                                                          
         USING BOXD,R2                                                          
         LA    RE,132                                                           
         ST    RE,BOXWIDTH         SET LENGTH OF PRINT LINE                     
*                                                                               
         L     RF,AMASTD                                                        
         USING MASTD,RF                                                         
         L     R2,AREMOT                                                        
         USING REMOTED,R2                                                       
*                                                                               
         TM    WHEN,X'20'          SOON                                         
         BZ    NPQ050                                                           
         XC    MCREMPQK,MCREMPQK                                                
         B     NPQ060                                                           
*                                                                               
NPQ050   MVC   REMOTABF,MCVPQBUF                                                
         MVC   REMOTADM,MCVDMGR                                                 
         MVC   REMOTAOP,MCVPQOPN                                                
         MVC   REMOTDST,MCDESTID                                                
         XC    MCALTREF,MCALTREF                                                
         MVI   REMOTCPY,C'1'                                                    
         MVI   REMOTCLS,C'Q'                                                    
         MVC   REMOTJID,=C'THD'                                                 
NPQ060   MVC   REMOTKEY(11),SPACES                                              
         MVC   REMOTSYS(6),=C'HFDATA'                                           
         MVC   REMOTFRM(4),=C'DATA'                                             
         J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
INITDWN  NTR1                                                                   
         XC    DLCBD(DLCBXLX),DLCBD                                             
*                                                                               
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,SPLATDWN         A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         B     PREPDX                                                           
         EJECT                                                                  
*              USER SUPPLIED PRINT ROUTINE - DOWNLOAD                           
*                                                                               
SPLATDWN NTR1                                                                   
         BRAS  RE,MYSPOOL                                                       
         MVI   LINE,1              PREVENT PAGE BREAK                           
         B     PREPDX                                                           
         SPACE 2                                                                
RECDLEN  DS    CL1                                                              
DLBLOCK  DS    CL(DLCBXLX)                                                      
DISKREC  DS    CL98                                                             
TADOWN   DCB   DDNAME=TADOWN,DSORG=PS,RECFM=FB,LRECL=98,               X        
               BLKSIZE=980,MACRF=(GM,PM),EODAD=NOMORE                           
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE                                          
         SPACE 2                                                                
MYSPOOL  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,WEBCHK           IF NOT PUTTING TO WEB, ALWAYS PRINT          
         BNE   MSPOOL10                                                         
         TM    THHLDPEB,TAAYHSEL   ELSE, IF AGY/CLI NOT ELECTRIC ONLY           
         BO    MSPOOL20                                                         
MSPOOL10 GOTO1 SPOOL,DMCB,(R8)     PRINT IT                                     
         B     MSPOOLX                                                          
         SPACE 1                                                                
MSPOOL20 MVC   P1,SPACES           IF WANT IN JUST ELECTRONIC FORM              
         MVC   P2,SPACES           CLEAR THE PRINT LINE                         
         MVC   P3,SPACES                                                        
         MVC   P4,SPACES                                                        
MSPOOLX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              BOX HOOK ROUTINE                                                 
*                                  P1, BYTE 0 = ROW CHARACTER                   
*                                  P1, BYTES 1-3 = A(PRINT LINE)                
*                                  P2, BYTE 0 = LINE NUMBER                     
*                                  P2, BYTES 1-3 = A(BOXD)                      
         DS    0D                                                               
BXHOOK   NMOD1 0,BOXHOOK                                                        
         L     R4,4(R1)                                                         
         USING BOXD,R4             R4=A(BOX AREA)                               
         LA    R3,BOXCOLS                                                       
*                                                                               
         USING LINED,R3            R3=A(PRINT LINE DSECT)                       
BX10     CLI   4(R1),LQDTLS+1      AT TOP OF DETAIL HEADS BOX                   
         BNE   *+12                                                             
         BAS   RE,DTLCOLS          NEED TO SET DETAIL COLUMNS                   
         B     BXX                                                              
*                                                                               
         CLI   4(R1),LQDTLTOT-1    AT TOP OF DETAIL TOTALS                      
         BNE   BX20                                                             
         MVC   BOXCOLS(LINAMTS-LINED),SPACES  CLEAR ALL NON-ACCUM COLS          
         MVI   LINAMTS-1,C'L'      LHS IS NOW START OF AMOUNTS FIELDS           
         B     BXX                                                              
*                                                                               
BX20     CLI   4(R1),LQDTLTOT+1    AT BOTTOM OF DETAIL TOTALS                   
         BNE   BXIT                                                             
         MVI   BCACC1,C' '         CLEAR ACCUM COLS                             
         MVI   BCACC2,C' '                                                      
         MVI   BCACC3,C' '                                                      
*                                                                               
BXX      MVI   BOXREQ,C'N'         SET NEW CONFIGURATION FLAG                   
*                                                                               
BXIT     XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO SET UP CAST DETAIL BOXES                              
*                                                                               
         USING LINED,R3            R3=A(PRINT LINE DSECT)                       
         USING BOXD,R4             R4=A(BOX AREA)                               
DTLCOLS  NTR1                                                                   
         MVC   BOXCOLS,SPACES      CLEAR AND RESET ALL COLUMNS                  
         MVI   BXL,C'L'                                                         
         MVI   BC1,C'C'                                                         
         MVI   BC2,C'C'                                                         
         MVI   BC3,C'C'                                                         
         MVI   BC4,C'C'                                                         
         MVI   BC5,C'C'                                                         
         MVI   BC6,C'C'                                                         
         MVI   BC7,C'C'                                                         
         MVI   BC8,C'C'                                                         
         MVI   BC9,C'C'                                                         
         MVI   BC10,C'C'                                                        
         MVI   BC11,C'C'                                                        
         MVI   BC12,C'C'                                                        
         MVI   BC13,C'C'                                                        
         MVI   BC14,C'C'                                                        
         MVI   BCACC1,C'C'                                                      
         MVI   BCACC2,C'C'                                                      
         MVI   BCACC3,C'C'                                                      
         MVI   BXR,C'R'            WILL BECOME RHS ON NEXT LINE                 
         B     BXIT                                                             
         EJECT                                                                  
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              CAST TABLE FOR NOTICE PRINTING                                   
*                                                                               
         DS    0D                                                               
         DC    CL8'**CAST**'                                                    
CASTTAB  DC    (NCAST*CASTLNQ)X'00'                                             
         DC    X'FF'                                                            
*                                                                               
NCAST    EQU   400                 MAXIMUM CAST SIZE                            
         SPACE 3                                                                
*              AGENCY NEGATIVE FLIST TABLE                                      
*                                                                               
         DS    0D                                                               
         DC    CL8'*FAGYTB*'                                                    
FAGYTAB  DC    (NFAGY*FAGYLNQ)X'00'                                             
         DC    X'FF'                                                            
*                                                                               
NFAGY    EQU   333                 MAXIMUM N'AGENCIES                           
*              CLIENT TABLE                                                     
*                                                                               
         DS    0D                                                               
         DC    CL8'*CLITAB*'                                                    
CLITAB   DC    (NCLI*FCLLNQ)X'00'                                               
         DC    X'FF'                                                            
*                                                                               
NCLI     EQU   333                 MAXIMUM N'CLIENTS                            
*              PRODUCT TABLE                                                    
*                                                                               
         DS    0D                                                               
         DC    CL8'*PRDTAB*'                                                    
PRDTAB   DC    (NPRD*FPRLNQ)X'00'                                               
         DC    X'FF'                                                            
*                                                                               
NPRD     EQU   333                 MAXIMUM N'PRODUCTS                           
*              COMMERCIAL TABLE                                                 
*                                                                               
         DS    0D                                                               
         DC    CL8'*COMTAB*'                                                    
COMTAB   DC    (NCOM*FCOLNQ)X'00'                                               
         DC    X'FF'                                                            
*                                                                               
NCOM     EQU   5000                MAXIMUM N'COMMERCIALS                        
*              AGENCY/OFFICE TABLE                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'*AGYTAB*'                                                    
AGYTAB   DC    (NAGY*AGYLNQ)X'00'                                               
         DC    X'FF'                                                            
*                                                                               
NAGY     EQU   5000                MAXIMUM N'AGENCIES                           
*              OFFICE TABLE                                                     
*                                                                               
         DS    0D                                                               
         DC    CL8'*OFFTAB*'                                                    
OFFTAB   DC    (NCLI*FOFLNQ)X'00'                                               
         DC    X'FF'                                                            
*                                                                               
NOFF     EQU   50                  MAXIMUM N'OFFICES                            
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
       ++INCLUDE TAHFND                                                         
         EJECT                                                                  
       ++INCLUDE TAYTDD                                                         
         EJECT                                                                  
*              CAST TABLE DSECT                                                 
*                                                                               
CASTD    DSECT                                                                  
CASTSSN  DS    CL9                 SOCIAL SECURITY NUMBER                       
CASTSEQ  DS    XL2                 SEQUENCE NUMBER                              
CASTDA   DS    XL4                 DISK ADDRESS                                 
CASTCORP DS    CL1                 CORP CODE                                    
CASTOV1  DS    XL4                 HLD OVERSCALE RATE                           
CASTOV2  DS    XL4                 HLD OVERSCALE RATE                           
CASTCAT  DS    CL3                 CATEGORY                                     
CASTCAM  DS    CL3                 ON/OFF CAMERA                                
CASTAGT  DS    XL2                 AGENT                                        
CASTSOW  DS    CL3                 WORK STATE                                   
CASTSOR  DS    CL3                 RESIDENT STATE                               
         DS    CL1                 SPARE                                        
CASTLIFT DS    CL1                 LIFT                                         
CASTDBL  DS    CL1                 N'DOUBLES                                    
CASTFRST DS    PL3                 FIRST SERVICES DATE                          
         ORG   CASTFRST                                                         
CASTEXP  DS    PL3                 EXPIRATION DATE                              
CASTUNI  DS    CL3                 UNION                                        
CASTLCL  DS    CL3                 LOCAL                                        
CASTSTAT DS    XL1                 STATUS                                       
CASTNAGU EQU   X'80'               USE DOES NOT APPLY TO GUARANTEE              
         DS    CL2                 SPARE                                        
CASTYR   DS    CL3                 CONTRACT YEAR                                
CASTGUA  DS    CL4                 GUARANTEE CODE                               
CASTSPNH DS    XL4                 SUBJ. TO PENSION & HEALTH                    
CASTSPAY DS    XL4                 SAVED CASTPAYI FOR DISK OPTION               
CASTSVPH DS    XL4                 SAVED CASTPNH FOR DISK OPTION                
CASTINR  DS    XL4                 I&R                                          
CASTGSTU DS    XL4                 US DOLLAR GST ON ACTRA PAYMENTS              
CASTAMTS DS    0XL4                                                             
CASTAPPL DS    XL4                 APPLIED AMOUNT                               
CASTPAYI DS    XL4                 INDIVIDUAL PAYMENTS                          
CASTPAYC DS    XL4                 CORP. PAYMENTS                               
CASTPNH  DS    XL4                 PENSION & HEALTH                             
NCASTAMT EQU   (*-CASTAMTS)/L'CASTAMTS                                          
CASTLNQ  EQU   *-CASTD                                                          
CASTNEXT EQU   *                                                                
         SPACE 3                                                                
*              AGENCY TABLE DSECT                                               
*                                                                               
AGYD     DSECT                                                                  
AGYOFF   DS    CL1                 OFFICE                                       
AGYTPC   DS    CL8                 TPC STAFF                                    
AGYAGY   DS    CL6                 AGENCY                                       
AGYHLDS  DS    XL1                 HOLDING FEE STATUS                           
AGYLAST  DS    PL3                 PERIOD END DATE                              
AGYLNQ   EQU   *-AGYD                                                           
AGYNEXT  EQU   *                                                                
         EJECT                                                                  
*              AGENCY NEGATIVE FLIST TABLE DSECT                                
*                                                                               
FAGYD    DSECT                                                                  
FAGYAGY  DS    CL6                 AGENCY                                       
FAGYLNQ  EQU   *-FAGYD                                                          
FAGYNEXT EQU   *                                                                
*                                                                               
*              CLIENT FLIST TABLE DSECT                                         
*                                                                               
FCLID    DSECT                                                                  
FCLICLI  DS    CL6                 CLIENT                                       
FCLLNQ   EQU   *-FCLID                                                          
FCLINEXT EQU   *                                                                
*                                                                               
*              PRODUCT FLIST TABLE DSECT                                        
*                                                                               
FPRDD    DSECT                                                                  
FPRDPRD  DS    CL6                 PRODUCT                                      
FPRLNQ   EQU   *-FPRDD                                                          
FPRDNEXT EQU   *                                                                
*                                                                               
*              COMMERCIAL FLIST TABLE DSECT                                     
*                                                                               
FCOMD    DSECT                                                                  
FCOMCOM  DS    CL4                 INTERNAL COMMERCIAL NUMBER                   
FCOLNQ   EQU   *-FCOMD                                                          
FCOMNEXT EQU   *                                                                
         EJECT                                                                  
*              OFFICE FLIST TABLE DSECT                                         
*                                                                               
FOFFD    DSECT                                                                  
FOFFOFF  DS    CL1                 OFFICE                                       
FOFLNQ   EQU   *-FOFFD                                                          
FOFFNEXT EQU   *                                                                
*                                                                               
*              DSECT TO COVER PRINT LINE                                        
*                                                                               
LINED    DSECT                                                                  
BXL      DS    CL1                                                              
LINSSN   DS    CL9                 SOCIAL SECURITY NUMBER                       
BC1      DS    CL1                                                              
LINNAME  DS    CL17                PERFORMER NAME                               
BC2      DS    CL1                                                              
LINOV1   DS    CL3                 HLD OVERSCALE RATE                           
BC3      DS    CL1                                                              
LINOV2   DS    CL3                 2ND OVERSCALE RATE                           
BC4      DS    CL1                                                              
LINCAT   DS    CL3                 CATEGORY                                     
BC5      DS    CL1                                                              
LINCAM   DS    CL3                 ON/OFF CAMERA                                
BC6      DS    CL1                                                              
LINAGT   DS    CL4                 AGENT                                        
BC7      DS    CL1                                                              
LINSOW   DS    CL3                 WORK STATE                                   
BC8      DS    CL1                                                              
LINLIFT  DS    CL1                 LIFT STATUS                                  
BC9      DS    CL1                                                              
LINDBL   DS    CL1                 N'DOUBLES                                    
BC10     DS    CL1                                                              
*INFRST  DS    CL8                 FIRST SERVICES DATE                          
LINEXP   DS    CL8                 EXPIRATION DATE                              
BC11     DS    CL1                                                              
LINUNI   DS    CL3                 UNION                                        
BC12     DS    CL1                                                              
LINYR    DS    CL3                 CONTRACT YEAR                                
BC13     DS    CL1                                                              
LINGUA   DS    CL4                 GUARANTEE CODE                               
BC14     DS    CL1                                                              
LINAMTS  EQU   *                                                                
LINLIT   DS    0CL25                                                            
LINAPPL  DS    CL12                APPLIED AMOUNT                               
BCACC1   DS    CL1                                                              
LINPAYI  DS    CL12                INDIVIDUAL WAGES                             
BCACC2   DS    CL1                                                              
LINPAYC  DS    CL12                CORP. PAYMENTS                               
BCACC3   DS    CL1                                                              
LINPNH   DS    CL12                PENSION & HEALTH                             
BXR      DS    CL1                                                              
*                                                                               
         ORG   LINSSN+132                                                       
LINCRPID DS    CL(L'LINSSN)        CORPORATION ID                               
         ORG   LINNAME+132                                                      
LINCRPNM DS    CL(L'LINNAME)       CORPORATION NAME                             
         ORG   LINOV1+132                                                       
LINCRPLT DS    CL3                 CRP LITERAL                                  
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE FOR SUMMARY                            
*                                                                               
HSLINED  DSECT                                                                  
         DS    CL25                                                             
HSBXL    DS    CL1                                                              
HSCID    DS    CL12                COMMERCIAL ID                                
HSBC1    DS    CL1                                                              
HSCOTITL DS    CL36                COMMERCIAL TITLE                             
HSBC2    DS    CL1                                                              
HSFFC    DS    CL8                 FIRST FIXED CYCLE                            
HSBC3    DS    CL1                                                              
HSDDTE   DS    CL8                 DUE DATE                                     
HSBC4    DS    CL1                                                              
HSEXP    DS    CL8                 EXPIRATION DATE                              
HSBXR    DS    CL1                                                              
HSTOT    DS    CL12                TOTAL                                        
HSBXR3   DS    CL1                                                              
HSAPPR   DS    CL7                 APPROVE                                      
HSBC5    DS    CL1                                                              
HSREL    DS    CL7                 RELEASE                                      
HSBXR2   DS    CL1                                                              
         EJECT                                                                  
*        DSECT TO COVER EMS FILE RECORD                                         
RECD     DSECT                                                                  
REC      DS    0CL400                                                           
RECTYP   DS    CL1               RECORD TYPE                                    
RECTYPB  EQU   C'B'              BREAKDOWN RECORD                               
RECTYPD  EQU   C'D'              DOC ID RECORD                                  
RECTYPE  EQU   C'E'              PERFORMER VERSIONS RECORD                      
RECTYPG  EQU   C'G'              MESSAGES RECORD                                
RECTYPH  EQU   C'H'              HEADING RECORD                                 
RECTYPM  EQU   C'M'              MORE HEADINGS RECORD                           
RECTYPP  EQU   C'P'              PERFORMER DETAILS RECORD                       
RECTYPS  EQU   C'S'              GUARANTEE ATTACHED COMM'LS RECORD              
RECTYPT  EQU   C'T'              TOTALS RECORD                                  
RECTYPV  EQU   C'V'              COMMERCIAL VERSIONS RECORD                     
*                                                                               
*                                DOC ID RECORD                                  
RECDOCID DS    CL18              DOC ID                                         
RECPRNT  DS    CL1               PRINTED?                                       
RECDLNQ  EQU   *-RECD                                                           
*                                                                               
*                                HEADING RECORD                                 
         ORG   RECDOCID                                                         
RECAGYN  DS    CL36              AGENCY NAME / ATTN NAME OVERRIDE               
RECADDR  DS    CL120             ADDRESS                                        
RECATTN  DS    CL36              ATTN NAME                                      
RECCLIN  DS    CL36              CLIENT NAME                                    
RECPRDN  DS    CL36              PRODUCT NAME                                   
RECCOMN  DS    CL36              COMMERCIAL NAME                                
RECHLNQ  EQU   *-RECD            SPARE                                          
*                                                                               
*                                MORE HEADINGS RECORD                           
         ORG   RECDOCID                                                         
RECCID   DS    CL12              COMMERCIAL ID                                  
RECFDAT  DS    CL8               FILM DATE                                      
RECFSTU  DS    CL12              FILM STUDIO                                    
RECFCTY  DS    CL12              FILM CITY                                      
RECRDAT  DS    CL8               RECORD DATE                                    
RECRSTU  DS    CL12              RECORD STUDIO                                  
RECRCTY  DS    CL12              RECORD CITY                                    
RECEMP   DS    CL36              EMPLOYER NAME                                  
RECAGY   DS    CL6               AGENCY CODE                                    
RECCLI   DS    CL6               CLIENT CODE                                    
RECPRD   DS    CL6               PRODUCT CODE                                   
RECTPC   DS    CL8               TPC STAFF                                      
RECFAIR  DS    CL8               FIRST AIR DATE                                 
RECFCYC  DS    CL8               FIRST FIXED CYCLE                              
RECEXP   DS    CL8               EXPIRATION DATE                                
RECCLEN  DS    CL3               COMMERCIAL LENGTH                              
RECMED   DS    CL5               MEDIA NAME                                     
RECLIFT  DS    CL12              LIFT ID                                        
RECLLEN  DS    CL3               LIFT LENGTH                                    
RECCYC   DS    CL17              CYCLE START AND END DATES                      
         ORG   RECCYC                                                           
RECCYCS  DS    CL8               CYCLE START                                    
         DS    CL1                                                              
RECCYCE  DS    CL8               CYCLE END                                      
RECCTYPE DS    CL1               COMMERCIAL TYPE                                
RECPTYPE DS    CL3               PAYMENT TYPE                                   
RECSPCY  DS    CL1               SPLIT CYCLES?                                  
RECOFFNM DS    CL36              OFFICE NAME                                    
RECOFFAD DS    CL120             OFFICE ADDRESS                                 
RECRUNDT DS    CL8               RUN DATE                                       
RECRUNTM DS    CL4               RUN TIME                                       
RECMLNQ  EQU   *-RECD                                                           
*                                                                               
*                                PERFOMER DETAILS RECORD                        
         ORG   RECDOCID                                                         
RECPER1  DS    CL131             PRINT LINE 1                                   
RECPER2  DS    CL32              PRINT LINE 2                                   
RECPLNQ  EQU   *-RECD                                                           
RECCSEQ  DS    CL4               CAST SEQUENCE NUMBER                           
RECPLNQ2 EQU   *-RECD                                                           
*                                                                               
*                                TOTALS RECORD FOR EMS                          
*                                REMITTANCE RECORD FOR FILE                     
         ORG   RECDOCID                                                         
RECGRS   DS    CL12              GROSS WAGES                                    
RECMISC  DS    CL12              MISC PAYMENT                                   
RECPHIR  DS    CL12              P&H/I&R                                        
RECWMI   DS    CL12              WAGES/MISC/I&R                                 
RECPRX   DS    CL12              PAYROLL TAXES                                  
RECPNH   DS    CL12              P&H CONTRIBUTION                               
RECGST   DS    CL12              GST                                            
RECHND   DS    CL12              HANDLING                                       
RECEMSF  DS    CL12              EMS FEE                                        
RECTOT   DS    CL12              HOLDING FEE TOTAL                              
RECDUEDT DS    CL8               HOLDING FEE DUE DATE                           
RECTLNQ  EQU   *-RECD                                                           
*                                                                               
*                                MESSAGE RECORD FOR FILE                        
         ORG   RECDOCID                                                         
RECMSG   DS    CL60              MESSAGE                                        
RECGLNQ  EQU   *-RECD                                                           
*                                                                               
*                                BREAKDOWN RECORD FOR FILE                      
         ORG   RECDOCID                                                         
RECTLIT  DS    CL25              TOTAL LITERAL                                  
RECTAMT  DS    CL12              MISC PAYMENT                                   
RECBLNQ  EQU   *-RECD                                                           
*                                                                               
*                                COMMERCIAL VERSIONS RECORD FOR FILE            
         ORG   RECDOCID                                                         
RECCVCD  DS    CL3               VERSION CODE                                   
RECCVID  DS    CL12              VERSION ID                                     
RECCVLN  DS    CL3               VERSION LENGTH                                 
RECCVRL  DS    CL1               RELEASED?                                      
RECCVTI  DS    CL36              VERSION TITLE                                  
RECVLNQ  EQU   *-RECD                                                           
*                                                                               
*                                GRT ATTACHED COMML RECORD FOR FILE             
         ORG   RECDOCID                                                         
RECSAGY  DS    CL6               AGENCY CODE                                    
RECSCLI  DS    CL6               CLIENT CODE                                    
RECSPRD  DS    CL6               PRODUCT CODE                                   
RECSCID  DS    CL12              COMMERCIAL ID                                  
RECSTIT  DS    CL36              COMMERCIAL TITLE                               
RECSNCS  DS    CL8               NEXT HOLDING FEE CYCLE START DATE              
RECSNCE  DS    CL8               NEXT HOLDING FEE CYCLE END DATE                
RECSLNQ  EQU   *-RECD                                                           
*                                                                               
*                                PERFORMER VERSIONS RECORD FOR FILE             
         ORG   RECDOCID                                                         
RECPVCD  DS    CL3               VERSION CODE                                   
RECELNQ  EQU   *-RECD                                                           
*                                                                               
         DS    CL396             SPARE                                          
RECLNQ   EQU   *-RECD                                                           
         EJECT                                                                  
*              DSECT TO COVER WEB FILE NAME                                     
         SPACE 1                                                                
WEBNAMD  DSECT                                                                  
*NHEAD   DS    CL12               TALDISK.TA0W                                  
WNHEAD   DS    CL12               TAL.TA0W                                      
WNRUNSYS DS    CL1                RUN ON SYSTEM                                 
WNRUNTYP DS    CL2                RUN TYPE                                      
WNRUNWHN DS    CL1                RUN WHEN                                      
WNDOT2   DS    CL1                DOT 2                                         
WNDATEHD DS    CL1                DATE HEADER                                   
WNRUNDTE DS    CL6                RUN DATE                                      
WNDOT3   DS    CL1                DOT 3                                         
WNRUNTHD DS    CL1                DATE HEADER                                   
WNRUNTIM DS    CL6                RUN TIME                                      
WNLNQ    EQU   *-WEBNAMD                                                        
         EJECT                                                                  
**********************************************************************          
*        DSECT FOR BACKUP TAPE                                       *          
**********************************************************************          
                                                                                
BTD      DSECT                                                                  
BTAPEREC DS    CL400                                                            
BTLNQ    EQU   *-BTD                                                            
         EJECT                                                                  
* TAREPFFD                                                                      
* TAREPFAD                                                                      
* DDGENTWA                                                                      
* DDTWADCOND                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* DDBIGBOX                                                                      
* DDMASTD                                                                       
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAREPWORKD                                                                    
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE TAREPFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPFAD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGETRETD                                                      
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'219TAREP1A   04/22/15'                                      
         END                                                                    
