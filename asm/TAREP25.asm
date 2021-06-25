*          DATA SET TAREP25    AT LEVEL 001 AS OF 11/10/06                      
*PHASE T70325A,*                                                                
         TITLE 'T70302 - INTERNET/NEW MEDIA REPORT'                             
T70325A  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70302                                                         
         L     RC,0(R1)            RC=A(CONTROLLER W/S)                         
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(SCREEN)                                 
         USING T703FFD,RA                                                       
         L     R9,ASUBSYSD         R9=A(SYSTEM W/S)                             
         USING SUBSYSD,R9                                                       
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8                                                        
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING MYD,R7              R7=A(LOCAL W/S)                              
         EJECT                                                                  
***********************************************************************         
*        MODE CONTROLLED ROUTINES                                     *         
***********************************************************************         
                                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   *+8                                                              
         BRAS  RE,VK               VALIDATE KEY                                 
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   *+8                                                              
         BRAS  RE,PREP             PRINT REPORT                                 
*                                                                               
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE KEY                                                 *         
***********************************************************************         
                                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
         XC    MYD(MYDLNQ),MYD     CLEAR WORKING STORAGE                        
         XC    TGAGY,TGAGY                                                      
         XC    TGCLI,TGCLI                                                      
*                                                                               
         USING PERVALD,R3                                                       
         LA    R2,SINPDH           PERIOD FILTER                                
         LA    R3,BLOCK                                                         
         GOTO1 PDVAL,DMCB,(X'80',(R3))                                          
         MVC   FLTPST,PVALPSTA                                                  
         MVC   FLTPEN,PVALPEND                                                  
         DROP  R3                                                               
*                                                                               
         CLI   SINAGYH+5,0         AGENCY FILTER                                
         JE    VK10                                                             
         GOTO1 RECVAL,DMCB,TLAYCDQ,(8,SINAGYH),SINAGYNH                         
         MVC   FLTAGY,TGAGY                                                     
*                                                                               
VK10     CLI   SINCLIH+5,0         CLIENT FILTER                                
         JE    VK20                                                             
         GOTO1 RECVAL,DMCB,TLCLCDQ,(8,SINCLIH),SINCLINH                         
         MVC   FLTCLI,TGCLI                                                     
*                                                                               
VK20     CLI   SINPRDH+5,0         PRODUCT FILTER                               
         JE    VK30                                                             
         GOTO1 RECVAL,DMCB,TLPRCDQ,(8,SINPRDH),SINPRDNH                         
         MVC   FLTPRD,TGPRD                                                     
*                                                                               
         USING TLCOD,R4                                                         
VK30     CLI   SINCIDH+5,0         COMMERCIAL ID FILTER                         
         JE    VK40                                                             
         LA    R2,SINCIDH                                                       
         GOTO1 RECVAL,DMCB,TLCOICDQ,(8,(R2)),SINCIDNH                           
         L     R4,AIO                                                           
         MVC   FLTCOM,TLCOCOM                                                   
*                                                                               
         OC    FLTCLI,FLTCLI                                                    
         JZ    *+14                                                             
         CLC   FLTCLI,TLCOCLI                                                   
         JNE   FLDINV                                                           
*                                                                               
         OC    FLTPRD,FLTPRD                                                    
         JZ    *+14                                                             
         CLC   FLTPRD,TLCOPRD                                                   
         JNE   FLDINV                                                           
         DROP  R4                                                               
*                                                                               
         USING TACOD,R4                                                         
         CLI   SINMEDH+5,0                                                      
         JE    VK40                                                             
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         LA    R2,SINMEDH                                                       
         CLC   TACOMED,SINMED                                                   
         JNE   FLDINV                                                           
         DROP  R4                                                               
*                                                                               
VK40     CLI   SINMEDH+5,0         COMMERCIAL MEDIA FILTER                      
         JE    VK50                                                             
         LA    R2,SINMEDH                                                       
         CLI   5(R2),1                                                          
         JNE   FLDINV                                                           
         GOTO1 MEDVAL,DMCB,8(R2)                                                
         JNE   FLDINV                                                           
         MVC   FLTMED,SINMED                                                    
*                                                                               
VK50     CLI   SINUSEH+5,0         USE FILTER                                   
         JE    VK60                                                             
         LA    R2,SINUSEH                                                       
         CLI   5(R2),3                                                          
         JNE   FLDINV                                                           
         GOTO1 USEVAL,DMCB,(X'40',SINUSE)                                       
         JNE   FLDINV                                                           
         MVC   FLTUSE,SINUSE                                                    
*                                                                               
VK60     CLI   SINCODH+5,0         INTERNET/NEWMEDIA CODE FILTER                
         JE    VK70                                                             
         MVI   TGMEEQU,INTERNET                                                 
         CLI   RECNUM,111                                                       
         JE    *+8                                                              
         MVI   TGMEEQU,NEWMEDIA                                                 
         GOTO1 RECVAL,DMCB,TLMDCDQ,(8,SINCODH),SINCODNH                         
         MVC   FLTCOD,TGMDCODE                                                  
*                                                                               
VK70     CLI   SINOPTH+5,0         OPTIONS                                      
         JE    XIT                                                              
*                                                                               
         USING SCAND,R3                                                         
         LA    R2,SINOPTH                                                       
         LA    R3,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3),0                                         
         CLI   4(R1),0                                                          
         JE    FLDINV                                                           
         ZIC   R0,4(R1)                                                         
*                                                                               
VK80     CLC   =C'CHECKS',SCDATA1  CHECKS OPTION                                
         JNE   FLDINV                                                           
         CLI   SCLEN2,0                                                         
         JNE   FLDINV                                                           
         OI    OPTIONS,CHECKS                                                   
*                                                                               
         LA    R3,SCANNEXT                                                      
         BCT   R0,VK80                                                          
         DROP  R3                                                               
*                                                                               
         J     XIT                                                              
*                                                                               
FLDINV   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PRINT REPORT                                                 *         
***********************************************************************         
                                                                                
PREP     NTR1  BASE=*,LABEL=*                                                   
         USING PRINTD,R2           R2=A(PRINT LINE)                             
         LA    R2,P                                                             
*                                                                               
         XR    R6,R6               R6=GROSS DOLLARS FOR REPORT                  
         ZAP   ICOUNT,=P'0'        CLEAR INVOICE AND                            
         ZAP   MCOUNT,=P'0'        INTERNET/NEW MEDIA COUNTER                   
*                                                                               
         LA    R1,IRNSPECS         SET FOR INTERNET PROCESSING                  
         MVI   TGMEEQU,INTERNET                                                 
         CLI   RECNUM,111                                                       
         JE    PREP10                                                           
         LA    R1,NMRSPECS         OR NEWMEDIA PROCESSING                       
         MVI   TGMEEQU,NEWMEDIA                                                 
PREP10   ST    R1,SPECS                                                         
*                                                                               
         USING TLINPD,R3                                                        
         LA    R3,KEY              SET TO READ INVOICE'S PAY DATE               
         XC    KEY,KEY             POINTERS FROM 11/10/06 ON                    
         MVI   TLINPCD,TLINPCDQ                                                 
         MVC   TLINPDTE,=X'A61110'                                              
         MVC   TLINPAGY,FLTAGY                                                  
         GOTO1 HIGH                                                             
         J     PREP30                                                           
*                                                                               
PREP20   GOTO1 SEQ                 READ NEXT INVOICE                            
*                                                                               
PREP30   CLI   KEY,TLINPCDQ                                                     
         JNE   PREP300                                                          
*                                                                               
         OC    FPK(FPKLNQ),FPK     FILTERING ON PASSIVE KEY?                    
         BZ    PREP50                                                           
*                                                                               
         OC    FLTAGY,FLTAGY       IF FILTERING ON AGENCY                       
         JZ    PREP40              FILTER NOW                                   
         CLC   TLINPAGY,FLTAGY                                                  
         JNE   PREP20                                                           
*                                                                               
PREP40   OC    FLTCLI,FLTCLI       IF FILTERING ON CLIENT                       
         JZ    PREP50              FILTER NOW                                   
         CLC   TLINPCLI,FLTCLI                                                  
         JNE   PREP20                                                           
*                                                                               
PREP50   GOTO1 GETREC              GET INVOICE RECORD                           
*                                                                               
         USING TAMDD,R4                                                         
         L     R4,AIO              REJECT IF NO INTERNET/                       
         MVI   ELCODE,TAMDELQ      NEW MEDIA ELEMENTS                           
         BRAS  RE,GETEL                                                         
         JNE   PREP20              REJECT IF NEWMEDIA/INTERNET                  
         CLC   TAMDTYPE,TGMEEQU    DOES NOT MATCH REPORT                        
         JNE   PREP20                                                           
         ST    R4,ATAMDEL          SAVE A(INTERNET/NEW MEDIA ELEMENT)           
         DROP  R4                                                               
*                                                                               
         USING TAPDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
*                                                                               
         OC    FPD(FPDLNQ),FPD     FILTERING ON PAYMENT DETAILS?                
         BZ    PREP90                                                           
*                                                                               
         OC    FLTPST,FLTPST       IF FILTERING ON PERIOD                       
         JZ    PREP60              FILTER NOW                                   
         CLC   FLTPST,TAPDCYCE                                                  
         JH    PREP20                                                           
         CLC   FLTPEN,TAPDCYCS                                                  
         JL    PREP20                                                           
*                                                                               
PREP60   OC    FLTPRD,FLTPRD       IF FILTERING ON PRODUCT                      
         JZ    PREP70              FILTER NOW                                   
         CLC   FLTPRD,TAPDPRD                                                   
         JNE   PREP20                                                           
*                                                                               
PREP70   OC    FLTCOM,FLTCOM       IF FILTERING ON COMMERCIAL                   
         JZ    PREP80              FILTER NOW                                   
         CLC   FLTCOM,TAPDCOM                                                   
         JNE   PREP20                                                           
*                                                                               
PREP80   OC    FLTUSE,FLTUSE       IF FILTERING ON USE                          
         JZ    PREP90              FILTER NOW                                   
         CLC   FLTUSE,TAPDUSE                                                   
         JNE   PREP20                                                           
*                                                                               
PREP90   ST    R4,ATAPDEL          SAVE A(PAYMENT DETAILS ELEMENT)              
         DROP  R4                                                               
*                                                                               
         USING TACOD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BRAS  RE,GETEL                                                         
*                                                                               
         OC    FLTMED,FLTMED       IF FILTERING ON COMMERCIAL MEDIA             
         JZ    PREP100             FILTER NOW                                   
         CLC   FLTMED,TACOMED                                                   
         JNE   PREP20                                                           
*                                                                               
PREP100  ST    R4,ATACOEL          SAVE A(COMMERCIAL DETAILS ELEMENT)           
         DROP  R4                                                               
*                                                                               
         MVI   INVSTAT,0           CLEAR INVOICE STATUS                         
*                                                                               
         USING TAMDD,R4                                                         
         LA    R5,MEDTAB           PREPARE TO BUILD INTERNET/                   
         MVI   ELCODE,TAMDELQ      NEW MEDIA CODE TABLE                         
         L     R4,ATAMDEL                                                       
*                                                                               
PREP110  CLC   FLTCOD,TAMDCODE     IF FILTERING ON CODE                         
         JNE   PREP120                                                          
         OI    INVSTAT,CODFOUND    AND CODE IS FOUND, SET STATUS                
*                                                                               
PREP120  MVC   0(L'TAMDCODE,R5),TAMDCODE  ADD CODE TO TABLE                     
         LA    R5,L'TAMDCODE(R5)                                                
*                                                                               
         BRAS  RE,NEXTEL           READ NEXT INTERNET/NEW MEDIA ELEMENT         
         JE    PREP110                                                          
         DROP  R4                                                               
*                                                                               
         OC    FLTCOD,FLTCOD       IF FILTERING ON CODE                         
         JZ    PREP130             FILTER NOW                                   
         TM    INVSTAT,CODFOUND                                                 
         JZ    PREP20                                                           
*                                                                               
PREP130  MVI   0(R5),X'FF'         MARK END OF TABLE                            
*                                                                               
         AP    ICOUNT,=P'1'        ADD 1 TO INVOICE COUNTER                     
*                                                                               
         XC    CHKKEY,CHKKEY       CLEAR CHECK KEY                              
*                                                                               
         USING TLIND,R4                                                         
         L     R4,AIO                                                           
         MVC   PAGY,TLINAGY        COPY AGENCY AND INVOICE TO SCREEN            
         XC    TLININV,PREPFFS                                                  
         GOTO1 TINVCON,DMCB,TLININV,PINV,DATCON                                 
         DROP  R4                                                               
*                                                                               
         USING TAPDD,R4                                                         
         L     R4,ATAPDEL                                                       
         MVC   PCLI,TAPDCLI        COPY CLIENT, PRODUCT, USE                    
         MVC   PPRD,TAPDPRD        AND CYCLE TO PRINT LINE                      
         OC    PPRD,SPACES                                                      
         MVC   PUSE,TAPDUSE                                                     
*                                                                               
         CLI   TAPDCYCS,0                                                       
         JE    PREP140                                                          
         GOTO1 DATCON,DMCB,(X'11',TAPDCYCS),(8,PCYC)                            
*                                                                               
PREP140  MVC   INVGROSS,TAPDGRS   SAVE PAID GROSS                               
         DROP  R4                                                               
*                                                                               
         USING TACOD,R4                                                         
         L     R4,ATACOEL         COPY COMMERCIAL ID TO PRINT LINE              
         MVC   PCID,TACOCID                                                     
*                                                                               
         LA    RE,TAMEDS          COPY COMMERCIAL MEDIA TO PRINT LINE           
PREP150  CLC   TACOMED,0(RE)                                                    
         JE    PREP160                                                          
         CLI   0(RE),X'FF'                                                      
         JE    PREP170                                                          
         LA    RE,L'TAMEDS(RE)                                                  
         J     PREP150                                                          
PREP160  MVC   PMED,0(RE)                                                       
         DROP  R4                                                               
*                                                                               
         USING TABDD,R4                                                         
PREP170  L     R4,AIO                                                           
         MVI   ELCODE,TABDELQ                                                   
         BRAS  RE,GETEL                                                         
         OC    TABDTOT,TABDTOT     IF INVOICE HAS BEEN BILLED                   
         JZ    PREP180             REPORT ON BILLED TOTAL                       
         MVC   INVGROSS,TABDTOT    ELSE, USE PAYMENT AMOUNT                     
         OI    INVSTAT,BILLED                                                   
*                                                                               
PREP180  MVC   PPHED,=C'BILLED'    COPY BILLED/PAID GROSS                       
         TM    INVSTAT,BILLED      TO PRINT LINE                                
         JO    *+10                                                             
         MVC   PPHED,=C'PAID  '                                                 
         EDIT  INVGROSS,PPDOL,2,MINUS=YES                                       
*                                                                               
         A     R6,INVGROSS         ADD INVOICE GROSS TO REPORT GROSS            
*                                                                               
         MVC   INVKEY,KEY          SAVE INVOICE KEY                             
*                                                                               
         LA    R5,MEDTAB           R5=A(INTERNET/NEWMEDIA TABLE)                
*                                                                               
PREP190  AP    MCOUNT,=P'1'        ADD 1 TO INTERNET/NEW MEDIA COUNT            
*                                                                               
         MVC   PCOD,0(R5)          COPY INTERNET/NEWMEDIA CODE                  
         OC    PCOD,SPACES         TO PRINT LINE                                
*                                                                               
         GOTO1 RECVAL,DMCB,TLMDCDQ,(X'A4',PCOD)                                 
         JNE   PREP200                                                          
*                                                                               
         USING TANAD,R4                                                         
         L     R4,AIO              AND NAME TO PRINT LINE                       
         MVI   ELCODE,TANAELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   PREP200                                                          
         ZIC   RE,TANALEN                                                       
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PNAM(0),TANANAME                                                 
         DROP  R4                                                               
*                                                                               
PREP200  TM    INVSTAT,PRINTED     IF MAIN PRINT LINE FOR INVOICE               
         JZ    PREP250             GO PRINT IT NOW                              
*                                                                               
         TM    OPTIONS,CHECKS      IF PRINTING OUT CHECKS                       
         JZ    PREP250                                                          
*                                                                               
PREP210  MVC   KEY,CHKKEY          SET TO RE-READ LAST CHECK                    
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
*                                                                               
         USING TLCKD,R3                                                         
         OC    CHKKEY,CHKKEY       IF WE HAVEN'T READ A CHECK YET               
         JNZ   PREP220                                                          
         XC    KEY,KEY             INITIALIZE CHECK KEY FOR THIS                
         MVI   TLCKD,TLCKCDQ       INVOICE                                      
         MVC   TLCKAGY,INVKEY+TLINPAGY-TLINPD                                   
         MVC   TLCKINV,INVKEY+TLINPINV-TLINPD                                   
*                                                                               
PREP220  GOTO1 HIGH                READ CHECK KEY                               
*                                                                               
         OC    CHKKEY,CHKKEY       IF WE'VE ALREADY READ THIS CHECK             
         JZ    PREP230                                                          
         GOTO1 SEQ                 GO READ THE NEXT ONE                         
PREP230  CLC   KEY(TLCKSORT-TLCKD),KEYSAVE                                      
         JNE   PREP240                                                          
*                                                                               
         MVC   CHKKEY,KEY          SAVE CHECK KEY                               
         GOTO1 SSNPACK,DMCB,TLCKSSN,PPHED                                       
         DROP  R3                                                               
*                                                                               
         GOTO1 GETREC              GET CHECK RECORD                             
*                                                                               
         USING TAPDD,R4                                                         
         L     R4,AIO              COPY PAYMENT AMOUNT TO                       
         MVI   ELCODE,TAPDELQ      PRINT LINE                                   
         BRAS  RE,GETEL                                                         
         JNE   PREP240                                                          
         EDIT  TAPDGRS,PPDOL,2,MINUS=YES                                        
         DROP  R4                                                               
*                                                                               
         USING TACDD,R4                                                         
         L     R4,AIO              IF CHECK EARNINGS EXIST                      
         MVI   ELCODE,TACDELQ      USE THAT INSTEAD                             
         BRAS  RE,GETEL                                                         
         JNE   PREP240                                                          
         OC    TACDEARN,TACDEARN                                                
         JZ    PREP240                                                          
         EDIT  TACDEARN,PPDOL,2,MINUS=YES                                       
         DROP  R4                                                               
*                                                                               
PREP240  MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
*                                                                               
PREP250  CLC   PRINTD(PRINTLNQ),SPACES                                          
         JE    PREP270                                                          
         GOTO1 SPOOL,DMCB,(R8)     IF THERE IS SOMETHING TO PRINT               
         OI    INVSTAT,PRINTED     PRINT IT                                     
*                                                                               
         CLI   0(R5),X'FF'         IF NOT AT END OF INTERNET/NEW                
         JE    PREP260             MEDIA TABLE                                  
         LA    R5,L'TAMDCODE(R5)   BUMP TO NEXT ENTRY                           
         CLI   0(R5),X'FF'                                                      
         JNE   PREP190                                                          
*                                                                               
PREP260  TM    OPTIONS,CHECKS      IF MORE CHECK TO PRINT                       
         JZ    PREP270             GO PROCESS THEM                              
         CLC   KEY(TLCKSORT-TLCKD),KEYSAVE                                      
         JE    PREP210                                                          
*                                                                               
PREP270  MVC   KEY,INVKEY           RESTORE READ SEQUENCE                       
         GOTO1 HIGH                                                             
         J     PREP20                                                           
*                                                                               
PREP300  CP    ICOUNT,=P'0'         IF ANY INVOICE WERE REPORTED                
         JE    XIT                  PRINT SUMMARY                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGY(13),=C'REPORT TOTALS'                                       
         EDIT  (P4,ICOUNT),PINV,COMMAS=YES                                      
         EDIT  (P4,MCOUNT),(7,PCOD),ALIGN=LEFT,COMMAS=YES                       
         EDIT  (R6),PPDOL,2,COMMAS=YES                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
*                                                                               
PREPFFS  DC    12X'FF'                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
*        MEDIA TABLE                                                  *         
***********************************************************************         
                                                                                
TAMEDS   DS    0CL5                                                             
         DC    CL5'TV'                                                          
         DC    CL5'INET'                                                        
         DC    CL5'NWMED'                                                       
         DC    CL5'RADIO'                                                       
         DC    CL5'CABLE'                                                       
         DC    CL5'PRINT'                                                       
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*        REPORT SPECS FOR INTERNET                                    *         
***********************************************************************         
                                                                                
IRNSPECS SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,120,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
         SSPEC H1,42,C'INTERNET REPORT'                                         
         SSPEC H2,42,C'---------------'                                         
*                                                                               
         SSPEC H7,01,C'AGENCY CLIENT PRODUCT COMMERCIAL   MEDIA'                
         SSPEC H8,01,C'------ ------ ------- ------------ -----'                
         SSPEC H7,42,C'INVOICE USE CYCLE'                                       
         SSPEC H8,42,C'------- --- -----------------'                           
         SSPEC H7,72,C'PAYMENT'                                                 
         SSPEC H8,72,C'-------------------'                                     
         SSPEC H7,92,C'INTERNET'                                                
         SSPEC H8,92,C'-----------------------------------'                     
         DC    H'0'                                                             
*                                                                               
***********************************************************************         
*        REPORT SPECS FOR NEW MEDIA                                   *         
***********************************************************************         
                                                                                
NMRSPECS SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,120,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
         SSPEC H1,42,C'NEW MEDIA REPORT'                                        
         SSPEC H2,42,C'----------------'                                        
*                                                                               
         SSPEC H7,01,C'AGENCY CLIENT PRODUCT COMMERCIAL   MEDIA'                
         SSPEC H8,01,C'------ ------ ------- ------------ -----'                
         SSPEC H7,42,C'INVOICE USE CYCLE'                                       
         SSPEC H8,42,C'------- --- -----------------'                           
         SSPEC H7,72,C'PAYMENT'                                                 
         SSPEC H8,72,C'-------------------'                                     
         SSPEC H7,92,C'NEW MEDIA'                                               
         SSPEC H8,92,C'-----------------------------------'                     
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
                                                                                
MYD      DSECT                                                                  
OPTIONS  DS    X                   OPTIONS STATUS                               
CHECKS   EQU   X'80'               SHOW CHECKS                                  
*                                                                               
FPK      DS    0C                  FILTERS FOR PASSIVE KEY                      
FLTAGY   DS    CL(L'TLINPAGY)      AGENCY FILTER                                
FLTCLI   DS    CL(L'TLINPCLI)      CLIENT FILTER                                
FPKLNQ   EQU   *-FPK                                                            
*                                                                               
FPD      DS    0C                  FILTERS FOR PAYMENT DETAILS                  
FLTPST   DS    CL(L'TAPDCYCS)      CYCLE START FILTER                           
FLTPEN   DS    CL(L'TAPDCYCE)      CYCLE END FILTER                             
FLTPRD   DS    CL(L'TAPDPRD)       PRODUCT FILTER                               
FLTCOM   DS    XL(L'TAPDCOM)       COMMERCIAL FILTER                            
FLTUSE   DS    CL(L'TAPDUSE)       USE FILTER                                   
FPDLNQ   EQU   *-FPD                                                            
*                                                                               
FCO      DS    0C                  FILTERS FOR COMMERCIAL DETAILS               
FLTMED   DS    CL(L'TACOMED)       MEDIA FILTER                                 
FCOLNQ   EQU   *-FCO                                                            
*                                                                               
FMD      DS    0C                  FILTERS FOR INTERNET/NEWMEDIA                
FLTCOD   DS    CL(L'TAMDCODE)      INTERNET/NEWMEDIA CODE FILTER                
FMDLNQ   EQU   *-FMD                                                            
*                                                                               
ATAMDEL  DS    A                   A(INTERNET/NEW MEDIA ELEMENT)                
ATAPDEL  DS    A                   A(PAYMENT DETAILS ELEMENT)                   
ATACOEL  DS    A                   A(COMMERCIAL DETAILS ELEMENT)                
*                                                                               
MEDTAB   DS    XL((20*4)+1)        INTERNET/NEWMEDIA TABLE                      
*                                                                               
INVKEY   DS    XL(L'KEY)           SAVED INVOICE KEY                            
INVGROSS DS    F                   INVOICE PAID/BILLED GROSS                    
INVSTAT  DS    X                   INVOICE STATUS                               
CODFOUND EQU   X'80'               FILTERED CODE FOUND                          
BILLED   EQU   X'40'               INVOICE HAS BEEN BILLED                      
PRINTED  EQU   X'20'               DETAILS PRINTED                              
*                                                                               
CHKKEY   DS    XL(L'KEY)           SAVED CHECK KEY                              
*                                                                               
ICOUNT   DS    PL4                 INVOICE COUNTER                              
MCOUNT   DS    PL4                 INTERNET/NEW MEDIA COUNTER                   
MYDLNQ   EQU   *-MYD                                                            
         EJECT                                                                  
***********************************************************************         
*        DSECT FOR PRINT LINE                                                   
***********************************************************************         
                                                                                
PRINTD   DSECT                                                                  
PAGY     DS    CL6                 AGENCY                                       
         DS    CL1                                                              
PCLI     DS    CL6                 CLIENT                                       
         DS    CL1                                                              
PPRD     DS    CL6                 PRODUCT                                      
         DS    CL2                                                              
PCID     DS    CL12                COMMERCIAL ID                                
         DS    CL1                                                              
PMED     DS    CL5                 MEDIA                                        
         DS    CL1                                                              
PINV     DS    CL6                 INVOICE                                      
         DS    CL2                                                              
PUSE     DS    CL3                 USE                                          
         DS    CL1                                                              
PCYC     DS    CL17                CYCLE                                        
         DS    CL1                                                              
PPHED    DS    CL6                 PAYMENT HEADER                               
         DS    CL1                                                              
PPDOL    DS    CL12                PAYMENT DOLLARS                              
         DS    CL1                                                              
PCOD     DS    CL4                 INTERNET/NEW MEDIA CODE                      
         DS    CL1                                                              
PNAM     DS    CL30                INTERNET/NEW MEDIA NAME                      
PRINTLNQ EQU   *-PRINTD                                                         
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPFBD                                                       
         EJECT                                                                  
*DDGENTWA  (MUST FOLLOW LAST SCREEN)                                            
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*DDPERVALD                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*DDTWADCONS                                                                     
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001TAREP25   11/10/06'                                      
         END                                                                    
