*          DATA SET ACBAT40    AT LEVEL 021 AS OF 05/03/02                      
*PHASE T61B40A                                                                  
         TITLE 'OVERLAY FOR SALE/USE TAX POSTINGS'                              
T61B40   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWSD,T61B40,R8,CLEAR=YES                                    
         USING LWSD,RC                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9              R9=GLOBAL W/S                                
         L     RA,ATWA0                                                         
         USING TWAD,RA             RA=TWA                                       
         L     RE,0(R1)            A(SALES TAX BLOCK)                           
         ST    RE,ADTAXB                                                        
*                                                                               
         CLI   CSACT,ACTCHA        ACTION ITEM/CHANGE                           
         BNE   MAIN                                                             
         ZAP   LASTBASE,=P'0'                                                   
         XC    LASTWKC,LASTWKC                                                  
         XC    COFFICE,COFFICE                                                  
         ZAP   TAXTOT,=P'0'        TAX TOTAL                                    
         B     TAX10                                                            
*                                                                               
MAIN     CLI   0(RE),C'B'                                                       
         BE    INIT10              FIRST TIME - MUST INITIALIZE                 
         CLI   PFKEY,11                                                         
         BE    END10               PF=11 NO MORE INPUT  - RETURN                
         CLI   PFKEY,10                                                         
         BE    NEXT10              PF=10 REFRESH SCREEN FOR NEXT                
         CLI   PFKEY,0                                                          
         BE    TAX10               PF=0 'ENTER' - OK TO EDIT                    
BADPFK   LA    R2,CONACTH                                                       
         MVI   ERRNUM,251          ERROR INVALID PF KEY                         
         B     EXIT                                                             
         EJECT                                                                  
*              SAVE THE CURRENT SCREEN TWA0 IN TWA3                             
*                                                                               
INIT10   LA    RF,STXDATA                                                       
         LA    R1,STXLNQ                                                        
         MOVE  ((RF),(R1)),(RE)       SAVE TAX BLOCK                            
         GOTO1 ANTRSES,0                                                        
*                                                                               
*              GET SALES TAX SCREEN                                             
*                                                                               
         XC    DMCB(20),DMCB                                                    
         MVC   DMCB+4(4),=X'D9061BC9'                                           
         GOTO1 CALLOV,DMCB,(0,CONTABH)                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                    CANT READ SCREEN                         
*                                                                               
         LA    RE,CONTABH              TRANSMIT SCREEN                          
         SR    R1,R1                                                            
INIT20   OI    6(RE),X'80'                                                      
         OI    7(RE),X'80'                                                      
         IC    R1,0(RE)                                                         
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BNE   INIT20                                                           
         XC    1(2,RE),1(RE)                                                    
*                                                                               
         ZAP   LASTBASE,=P'0'                                                   
         XC    LASTWKC,LASTWKC                                                  
         XC    COFFICE,COFFICE                                                  
         ZAP   TAXTOT,=P'0'        TAX TOTAL                                    
*                                                                               
*                               FILL IN ANY INFO PASSED FROM CALLER             
         MVC   TAXACCT(12),STXACC+3   JOB TO SCREEN                             
         OC    TAXACCT,SPACES                                                   
         MVC   DETJOB,TAXACCT         SAVE THE JOB FROM DETAIL SCREEN           
         MVC   TAXDOC,STXREF          REFERENCE                                 
         MVC   TAXDATE,STXDTE         DATE                                      
         MVC   TAXORDR,STXORD         ORDER                                     
         MVC   COFFICE,STXCOFF        CREDIT OFFICE                             
*                                                                               
         OC    STXNARR(L'SPACES),SPACES                                         
         CLC   STXNARR(L'SPACES),SPACES                                         
         BE    INIT40                                                           
         LA    R3,L'STXNARR        CHOP AND DISPLAY NARRATIVE                   
         LA    R4,L'TAXNAR1                                                     
*                                                                               
         GOTO1 CHOPPER,DMCB,((R3),STXNARR),((R4),IOAREA),2                      
         L     R5,DMCB+8                                                        
         LTR   R5,R5                                                            
         BZ    INIT40                                                           
         MVC   TAXNAR1,IOAREA                                                   
         CH    R5,=H'1'                                                         
         BE    INIT40                                                           
         MVC   TAXNAR2,IOAREA+L'TAXNAR1                                         
*                                                                               
INIT40   OC    STXAMT,STXAMT                                                    
         BNZ   *+10                                                             
         ZAP   STXAMT,=P'0'         NO TAX AMOUNT PASSED                        
         CP    STXAMT,=P'0'                                                     
         BE    INIT70                                                           
         EDIT  STXAMT,(11,WORK1),2,ALIGN=LEFT,FLOAT=-                           
*                                                                               
INIT70   MVC   TAXTXB,WORK1        DEFAULT IS TRANSACTION AMOUNT                
         LA    R2,TAXACCTH         GET CURSOR TO FIRST REQUIRED FIELD           
         CLI   8(R2),C' '          ACCOUNT                                      
         BNH   INIT72                                                           
         LA    R2,TAXDOCH          DOCUMENT                                     
         CLI   8(R2),C' '                                                       
         BNH   INIT72                                                           
         LA    R2,TAXLOCH          LOCALITY                                     
INIT72   MVI   ERRNUM,X'FE'                                                     
         MVI   MSG,C' '                                                         
         MVC   MSG+1(L'MSG-1),MSG                                               
         MVC   MSG(25),=CL25'INPUT REQUIRED FIELDS'                             
         L     RE,ADTAXB          SCREEN LOADED- OK TO EDIT(NEXT TIME)          
         MVI   0(RE),C'E'                                                       
         B     EXIT                                                             
         EJECT                                                                  
*              RESTORE SAVED SCREEN                                             
*                                                                               
END10    DS    0H                                                               
         L     RE,ADTAXB                                                        
         MVI   0(RE),C'X'          SET STXMODE TO END                           
         GOTO1 AXITSES                                                          
         MVI   CSSPROG,0           RESET THE PF KEYS                            
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              CLEAR SCREEN FOR NEXT INPUT                                      
*                                                                               
NEXT10   DS    0H                                                               
         CP    LASTBASE,=P'0'                                                   
         BE    BADPFK                      NO SAVED AMOUNT                      
         OC    LASTWKC,LASTWKC                                                  
         BZ    BADPFK                      NO SAVED WORK CODE                   
         TWAXC TAXLOCH,TAXLSTH,PROT=Y      CLEAR THE SCREEN                     
         EDIT  LASTBASE,(11,WORK1),2,ALIGN=LEFT,FLOAT=-                         
         MVC   TAXWKC,LASTWKC                                                   
         B     INIT70                       SET-UP FOR NEXT                     
         EJECT                                                                  
*              EDIT THE INPUT                                                   
*                                                                               
TAX10    LA    R2,TAXACCTH                                                      
         MVI   FVMINL,1                                                         
         MVI   BOFLAG1,ACIPRCLI+ACIPRPRO+ACIPRJOB                               
         XC    PSCLICOD,PSCLICOD   CLEAR OUT OLD CODE                           
         XC    PSPROCOD,PSPROCOD                                                
         XC    PSJOBCOD,PSJOBCOD                                                
         GOTO1 AVALCPJ,TAXACCTH                                                 
         BNE   ERRXIT                                                           
         BAS   RE,CHECKACC                                                      
*                                                                               
         MVC   JOB,ACCODE                                                       
         MVC   JBNME,ACNAME                                                     
         MVC   TAXACCN,ACNAME                                                   
         OI    TAXACCNH+6,X'80'                                                 
*                                                                               
         LA    R4,JOB                                                           
         GOTO1 ASETJOB,DMCB,(R4)                                                
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    TAX12               NO                                           
         MVI   ERRNUM,47           YES, ERROR                                   
         B     ERROR                                                            
*                                                                               
         USING GOBLOCKD,R7                                                      
         USING ACTRECD,R6                                                       
TAX12    L     R7,AGOBLOCK                                                      
         LA    R6,IOKEY                                                         
         MVC   ACTKEY,BCSPACES     READ CLIENT RECORD                           
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)                      
         MVC   ACTKACT(3),GOSELCLI                                              
         GOTO1 AGETACT,0                                                        
         BNE   ERRXIT                                                           
*                                                                               
         MVC   CLI,ACCODE          SAVE CLIENT CODE, NAME AND OFFICE            
         MVC   CLNME,ACNAME                                                     
         MVC   OFFICE,GOEFFOFC                                                  
*                                                                               
         MVC   ACTKEY,BCSPACES     READ PRODUCT RECORD                          
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)                      
         MVC   ACTKACT(3),GOSELCLI                                              
         MVC   ACTKACT+3(3),GOSELPRO                                            
*                                                                               
         GOTO1 AGETACT,0                                                        
         BNE   ERRXIT                                                           
*                                                                               
         MVC   PRD,ACCODE          SAVE PRODUCT AND NAME                        
         MVC   PRNME,ACNAME                                                     
         MVC   OFFICE,GOEFFOFC                                                  
*                                                                               
         OC    COFFICE,COFFICE     DO WE HAVE A DETAIL OFFICE?                  
         BZ    TAX12A              NO, DON'T BOTHER WITH THE REST               
*                                                                               
         CLC   JOB+3(12),DETJOB    IS TAX AND DETAIL JOB SAME?                  
         BNE   TAX12A              NO, USE OFFICE TAX JOB                       
         MVC   OFFICE,COFFICE      YES, USE DETAIL JOB                          
         OC    OFFICE,SPACES                                                    
*                                                                               
TAX12A   MVC   DOCNO,SPACES        DOCUMENT(REFERENCE)                          
         LA    R2,TAXDOCH                                                       
         BAS   RE,ANY                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DOCNO(0),8(R2)                                                   
*                                                                               
         LA    R2,TAXDATEH                                                      
         GOTO1 DATCON,DMCB,(5,0),(1,PDATE)                                      
         CLI   5(R2),0                                                          
         BE    TAX13               DEFAULT DATE IS TODAY                        
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   *+12                                                             
         MVI   ERRNUM,13                                                        
         B     ERROR                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(1,PDATE)                                   
*                                                                               
*                                                                               
TAX13    MVC   ORDERNO,SPACES        ORDER NUMBER                               
         LA    R2,TAXORDRH                                                      
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    TAX15                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ORDERNO(0),8(R2)                                                 
*                                                                               
         EJECT                                                                  
         USING PSTD,R5                                                          
TAX15    ZAP   SCRTOT,=P'0'        SCREEN TOTAL                                 
         LA    R5,PSTABLE                                                       
         LA    R0,4*MXLNES         NUMBER PER LINE * MAX NUMBER                 
TAX17    MVC   PSTACC,SPACES       INITIALIZE POSTING ENTRY                     
         MVC   PSTNME,SPACES                                                    
         XC    PSTPCT,PSTPCT                                                    
         ZAP   PSTAMT,=P'0'                                                     
         LA    R5,PSTLNQ(R5)                                                    
         BCT   R0,TAX17                                                         
*                                                                               
*                                                                               
         LA    R4,TAXLOCH          LOCALITY                                     
         USING TXD,R4                                                           
         LA    R5,PSTABLE                                                       
TAX20    LA    R2,TXLOCH                                                        
         BAS   RE,ANY              IS REQUIRED                                  
         MVI   ERRNUM,2                                                         
         CLI   5(R2),1                                                          
         BNH   ERROR                                                            
         BAS   RE,VALOC            VALIDATE LOCALITY                            
*                                                                               
*              VALIDATE BASIS                                                   
*                                                                               
         LA    R2,TXBASH                                                        
         CP    LASTBASE,=P'0'      ALREADY HAVE BASIS                           
         BNE   *+8                 NO NEED TO REQUIRE INPUT                     
         BAS   RE,ANY                                                           
         CLI   5(R2),0                                                          
         BE    TAX30               NO OVERRIDE OF BASIS                         
         ZIC   R0,5(R2)                                                         
         GOTO1 AMTVAL,DMCB,8(R2),(R0)  VALID RATE BASIS                         
         MVI   ERRNUM,25                                                        
         CLI   0(R1),0                                                          
         BNE   ERRXIT                                                           
         L     R1,4(R1)                                                         
         LA    R1,0(R1)                                                         
         ZAP   LASTBASE,0(8,R1)                                                 
         EJECT                                                                  
*              VALIDATE WORKCODE                                                
*                                                                               
TAX30    LA    R2,TXWKCH                                                        
         OC    LASTWKC,LASTWKC     ALREADY HAVE WORKCODE                        
         BNZ   *+8                 NO NEED TO REQUIRE INPUT                     
         BAS   RE,ANY                                                           
         CLI   5(R2),0                                                          
         BE    TAX40               NO OVERRIDE OF WORKCODE                      
         MVI   ERRNUM,INVWC                                                     
         CLC   TXWKC,=C'99'                                                     
         BE    ERROR                                                            
         GOTO1 AGETWC,TXWKC        VALIDATE WORK CODE                           
         BNE   ERROR                                                            
         MVC   LASTWKC,TXWKC                                                    
         MVI   ERRNUM,INVWC                                                     
         LA    R1,GOUWLIST         CHECK IT'S A BILLABLE W/C                    
         LA    R0,6                                                             
*                                                                               
TAX35    CLC   LASTWKC,0(R1)                                                    
         BE    ERROR                                                            
         LA    R1,2(R1)                                                         
         BCT   R0,TAX35                                                         
         DROP  R7                                                               
*                                                                               
*              COMPUTE THE TAX AMOUNT FOR EACH ENTRY                            
*                                                                               
         USING PSTD,R5                                                          
TAX40    LA    R0,4                MAX OF 4 POSTING PER LINE                    
TAX43    CLC   PSTACC,SPACES       NO POSTING ACCOUNT- OK TO SKIP               
         BE    TAX45                                                            
         ZAP   PSTBAS,LASTBASE                                                  
         ZAP   PL13,LASTBASE       AMOUNT                                       
         MP    PL13,PSTPCT         X PERCENT 4DP                                
         SRP   PL13,64-6,5                                                      
         ZAP   PSTAMT,PL13         TAX AMOUNT                                   
         MVC   PSTWKC,LASTWKC      WORKCODE                                     
TAX45    LA    R5,PSTLNQ(R5)                                                    
         BCT   R0,TAX43                                                         
*                                                                               
TAX50    LA    R4,TXLNQ(R4)        GET R2 TO NEXT LINE                          
         LA    R3,TAXNARRH                                                      
         CR    R4,R3                                                            
         BNL   TAX70               END OF SCREEN                                
         CLI   TXLOCH+5,0          IF ANY INPUT VALIDATE LINE                   
         BNE   TAX20                                                            
         CLI   TXBASH+5,0                                                       
         BNE   TAX20                                                            
         CLI   TXWKCH+5,0                                                       
         BNE   TAX20                                                            
         B     TAX50               ELSE SKIP THE LINE                           
*                                                                               
TAX70    BAS   RE,POSTIT           MAKE THE POSTINGS                            
*                                                                               
         MVC   WORK1,SPACES          SET-UP THE TOTAL LINE                      
         LA    R2,WORK1                                                         
         CP    SCRTOT,=P'0'                                                     
         BE    TAX73                                                            
         MVC   0(13,R2),=C'SCREEN TOTAL='                                       
         LA    R2,13(R2)                                                        
         EDIT  SCRTOT,(11,0(R2)),2,ALIGN=LEFT,FLOAT=-                           
TAX73    CP    TAXTOT,=P'0'                                                     
         BE    TAX75                                                            
         LA    R2,WORK1+20                                                      
         MVC   0(10,R2),=C'TAX TOTAL='                                          
         LA    R2,10(R2)                                                        
         EDIT  TAXTOT,(11,0(R2)),2,ALIGN=LEFT,FLOAT=-                           
TAX75    LA    R3,L'WORK1                                                       
         GOTO1 SQUASHER,DMCB,WORK1,(R3)                                         
         MVC   TAXTOTL,WORK1                                                    
         OI    TAXTOTLH+6,X'80'                                                 
*                                                                               
         MVC   WORK(L'TAXLOC),TAXLOC          PUT AN * IN FRONT OF              
         MVI   TAXLOC,C'*'                    FIRST UNIT TO AVOID               
         MVC   TAXLOC+1(L'TAXLOC-1),WORK      DOUBLE INPUT                      
         OI    TAXLOCH+6,X'80'                                                  
*                                                                               
         MVI   ERRNUM,X'FE'                                                     
         MVC   MSG,=CL60'INPUT COMPLETE ENTER NEXT'                             
         LA    R2,TAXLOCH                                                       
         ST    R2,FADR                                                          
*                                                                               
         CLI   CSACT,ACTCHA        ACTION ITEM/CHANGE                           
         BNE   EXIT                                                             
         GOTO1 AXITSES                                                          
         B     EXIT                                                             
         EJECT                                                                  
*              VALIDATE 4 LEVELS OF LOCALITY                                    
*                                                                               
         USING TXD,R4                                                           
         USING PSTD,R5                                                          
VALOC    NTR1                                                                   
         ZIC   R3,TXLOCH+5                                                      
         OC    TXLOC,SPACES                                                     
*                                    CHECK FOR DUPLICATE CODES                  
*                                                                               
         LA    R1,TAXLOCH            R1 TO FIRST LOCALITY FIELD                 
VALOC02  CR    R4,R1                 R4 IS AT CURRENT LOCALITY FIELD            
         BE    VALOC04               ALL PREVIOUS FIELDS CHECKED                
         CLC   TXLOC,8(R1)                                                      
         BE    DUPERR                DUPLICATE FIELD                            
         LA    R1,TXLNQ(R1)                                                     
         B     VALOC02                                                          
*                                                                               
VALOC04  MVC   LWKEY,SPACES                                                     
         LA    R7,LWKEY            BUILD TAX KEY                                
         USING ACKEYD,R7                                                        
         MVI   ACUTTYPE,ACUTEQU                                                 
         MVI   ACUTSREC,ACUTSEQU                                                
         MVC   ACUTCMP,COMPANY                                                  
         MVC   ACUTLOC(2),TXLOC                                                 
         BAS   RE,RDHIGH                                                        
         CLC   LWKEY,LWIO                                                       
         BNE   BADLOC               MISSING LEVEL ONE                           
         LA    R6,LWIO                                                          
         BAS   RE,GETAX             POST ACCOUNT/NAME/PCT                       
         SH    R3,=H'2'                                                         
         BNP   EXIT                ONE LEVEL CODE                               
*                                                                               
         LA    R5,PSTLNQ(R5)        R5 TO NEXT POST SAVE AREA                   
         MVC   ACUTLOC(4),TXLOC                                                 
         BAS   RE,RDHIGH                                                        
         CLC   LWKEY,LWIO                                                       
         BNE   BADLOC              MISSING LEVEL TWO                            
         LA    R6,LWIO                                                          
         BAS   RE,GETAX             POST ACCOUNT/NAME/PCT                       
         SH    R3,=H'2'                                                         
         BNP   EXIT                2 LEVEL CODE                                 
*                                                                               
         LA    R5,PSTLNQ(R5)        R5 TO NEXT POST SAVE AREA                   
         MVC   ACUTLOC(6),TXLOC                                                 
         BAS   RE,RDHIGH                                                        
         CLC   LWKEY,LWIO                                                       
         BNE   BADLOC              MISSING LEVEL THREE                          
         LA    R6,LWIO                                                          
         BAS   RE,GETAX            POST ACCOUNT/NAME/PCT                        
         SH    R3,=H'2'            NOW GET LEVEL 3 ACCOUNT                      
         BNP   EXIT                3 LEVEL CODE                                 
*                                                                               
         LA    R5,PSTLNQ(R5)        R5 TO NEXT POST SAVE AREA                   
         MVC   ACUTLOC(8),TXLOC     SET FOR FULL KEY                            
         BAS   RE,RDHIGH                                                        
         CLC   LWKEY,LWIO                                                       
         BNE   BADLOC              MISSING LEVEL FOUR                           
         LA    R6,LWIO                                                          
         BAS   RE,GETAX             POST ACCOUNT/NAME/PCT                       
         B     EXIT                                                             
         EJECT                                                                  
*               POST ACCOUNT NAMES/CODES/ RATE TO TABLE                         
*                                                                               
         USING PSTD,R5                                                          
GETAX    NTR1                                                                   
         BAS   RE,GETNAME                                                       
         MVC   TXDATA(36),WORK     DISPLAY LOCALITY NAME                        
         OI    TXDATAH+6,X'80'                                                  
         MVC   PSTLCNM,WORK        AND SAVE IN POSTING TABLE                    
*                                                                               
         MVI   ELCODE,X'5F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+8                                                              
         B     NORATE                                                           
*                                                                               
         USING ACTAXEL,R6                                                       
GETAX03  CLI   ACTAXLEN,ACTAXLQ2                                                
         BL    *+16                                                             
         MVC   PSTACC(1),COMPANY                                                
         MVC   PSTACC+1(14),ACTAXACC    SAVE THE POSTING ACCOUNT                
         MVC   PSTLOC,LWKEY             SAVE LOCALITY KEY                       
         CLC   ACTAXEFF,PDATE                                                   
         BH    *+16                 RATE NOT YET IN EFFECT                      
         MVC   PSTEFF,ACTAXEFF      SAVE EFFECTIVE DATE                         
         ZAP   PSTPCT,ACTAXRTE      AND RATE                                    
         BAS   RE,NEXTEL                                                        
         BE    GETAX03                                                          
         OC    PSTPCT,PSTPCT                                                    
         BZ    NORATE              NO EFFECTIVE RATE                            
*                                                                               
         MVC   KEY,SPACES          VALIDATE THE CREDIT                          
         MVC   KEY(15),PSTACC                                                   
         SR    R6,R6               NO PROFILES                                  
         BAS   RE,GETACC           GET ACCOUNT NAME                             
         BAS   RE,CHECKACC         CHECK VALID FOR POSTING                      
         MVC   PSTNME,ACCTNAME     SAVE THE NAME                                
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
*                                                                               
GETNAME  NTR1                                                                   
         MVC   WORK,SPACES                                                      
         MVI   ELCODE,X'20'        GET ACCOUNT NAME INTO WORK                   
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
         USING ACNAMED,R6                                                       
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     EXIT                                                             
         MVC   WORK(0),ACNMNAME                                                 
         DROP  R6                                                               
         EJECT                                                                  
*              BUILD POSTING RECORDS                                            
*                                                                               
         USING DLDESCD,R2                                                       
         USING PSTD,R5                                                          
POSTIT   NTR1                                                                   
         LA    R2,IOAREA+2                                                      
         USING DLDESCD,R2                                                       
         MVI   DLDSEL,X'64'                                                     
         MVC   DLDSREF,DOCNO                                                    
         MVC   DLDSDATE,PDATE                                                   
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
         XC    DLDSSTAT+1(6),DLDSSTAT+1                                         
         XC    DLDSNARR,DLDSNARR                                                
         GOTO1 ANARRSCN,DMCB,TAXNAR1H,DLDSNARR                                  
         LA    R1,DLDSNARR                                                      
         SR    R1,R2               R1 = ELEMENT - NARRATIVE                     
         AR    R1,R6               R6 = L'NARRATIVE                             
         STC   R1,DLDSLEN                                                       
         AR    R2,R1               R2 TO NEXT ELEMENT                           
*                                                                               
         LA    R5,PSTABLE                                                       
         LA    R3,MXLNES           MAXIMUM NUMBER OF LINES                      
POST10   LA    R0,4                NUMBER PER LINE                              
         MVI   ACTIVITY,C'N'                                                    
         ZAP   TRANSAMT,=P'0'                                                   
*                                                                               
POST20   CLC   PSTACC,SPACES                                                    
         BE    POST40              NO ENTRY                                     
         MVI   ACTIVITY,C'Y'                                                    
*                                                                               
         CLC   ORDERNO,SPACES                                                   
         BE    POST23                   NO ORDER NUMBER                         
         USING ACNOD,R2                                                         
         MVC   ACNOEL(2),=X'2508'                                               
         MVC   ACNO(6),ORDERNO                                                  
         LA    R2,8(R2)                  R2 TO NEXT ELEMENT                     
*                                                                               
         USING ACTAXEL,R2                                                       
POST23   XC    0(ACTAXLQ1,R2),0(R2)   ADD TAX ELEMENT                           
         MVI   ACTAXEL,X'5F'                                                    
         MVI   ACTAXLEN,ACTAXLQ2                                                
         MVC   ACTAXEFF,PSTEFF        EFFECTIVE DATE                            
         ZAP   ACTAXRTE,PSTPCT        RATE                                      
         ZAP   ACTAXBAS,PSTBAS        BASIS                                     
         MVC   ACTAXLOC,PSTLOC        LOCALITY KEY                              
         ZIC   R1,ACTAXLEN                                                      
         AR    R2,R1                                                            
*                                                                               
         CLC   PSTACC+1(2),=C'SV'                                               
         BE    POST24                                                           
         CLC   PSTACC+1(2),=C'SX'                                               
         BNE   POST25                                                           
*                                                                               
         USING PAKELD,R2                                                        
POST24   MVI   PAKEL,PAKELQ                                                     
         MVI   PAKLN,PAKLNQ                                                     
         MVC   PAKACC,PSTACC                                                    
         MVC   PAKOFF,OFFICE                                                    
         MVC   PAKCON,CLI                                                       
         CLC   PSTACC+1(2),=C'SX'                                               
         BNE   *+10                                                             
         MVC   PAKCON,JOB                                                       
         MVC   PAKDATE,PDATE                                                    
         MVC   PAKREF,DOCNO                                                     
         ZIC   R1,PAKLN                                                         
         AR    R2,R1                                                            
*                                                                               
         USING DLPOSTD,R2                                                       
POST25   MVI   DLPSEL,X'69'        DEBIT POSTING                                
         MVI   DLPSLEN,X'71'       LENGTH                                       
         MVC   DLPSDBAC,JOB        JOB ACCOUNT                                  
         MVC   DLPSDBNM,JBNME      JOB NAME                                     
         MVC   DLPSCRAC,PSTACC     CREDIT ACCOUNT                               
         MVC   DLPSCRNM,PSTNME     CREDIT ACCOUNT NAME                          
         MVI   DLPSTYPE,0          MAIN ACCOUNTING ENTRY                        
         ZAP   DLPSAMNT,PSTAMT     AMOUNT                                       
         MVC   DLPSANAL,OFFICE     OFFICE                                       
         CLC   DLPSDBAC+1(2),=C'SJ'                                             
         BNE   *+10                                                             
         MVC   DLPSANAL,PSTWKC     FOR PRODUCTION USE WORKCODE                  
         AP    TRANSAMT,PSTAMT     FOR ITEM TOTAL                               
         AP    SCRTOT,PSTAMT       SCREEN TOTAL                                 
         AP    TAXTOT,PSTAMT       TAX TOTAL                                    
         ZIC   R1,DLPSLEN                                                       
         AR    R2,R1                                                            
         EJECT                                                                  
*                                     SET-UP FOR CREDIT POSTING                 
*                                                                               
*                                                                               
         USING ACOTHERD,R2                                                      
POST30   MVC   ACOTEL(2),=X'230F'       BUILD 'OTHERS' ELEMENT FOR              
         MVC   ACOTNUM(13),SPACES       PRODUCT AND JOB                         
         MVC   ACOTNUM(6),PRD+6                                                 
         MVC   ACOTNUM+6(6),JOB+9                                               
         ZIC   R1,ACOTLEN                                                       
         AR    R2,R1                                                            
*                                                                               
         USING ACTAXEL,R2                                                       
         XC    0(ACTAXLQ1,R2),0(R2)   ADD TAX ELEMENT                           
         MVI   ACTAXEL,X'5F'                                                    
         MVI   ACTAXLEN,ACTAXLQ2                                                
         MVC   ACTAXEFF,PSTEFF        EFFECTIVE DATE                            
         ZAP   ACTAXRTE,PSTPCT        RATE                                      
         ZAP   ACTAXBAS,PSTBAS        BASIS                                     
         MVC   ACTAXLOC,PSTLOC        LOCALITY                                  
         ZIC   R1,ACTAXLEN                                                      
         AR    R2,R1                                                            
*                                                                               
         USING DLPOSTD,R2                                                       
         MVI   DLPSEL,X'6A'        CREDIT POSTING                               
         MVI   DLPSLEN,X'71'       LENGTH                                       
         MVC   DLPSDBAC,CLI        CONTRA ACCOUNT IS CLIENT                     
         MVC   DLPSDBNM,CLNME                                                   
*                                                                               
         CLC   PSTACC+1(2),=C'SX'                                               
         BNE   *+16                                                             
         MVC   DLPSDBAC,JOB        FOR SX IT'S CLI/PRD/JOB                      
         MVC   DLPSDBNM,JBNME                                                   
*                                                                               
         MVC   DLPSCRAC,PSTACC     CREDIT ACCOUNT                               
         MVC   DLPSCRNM,PSTNME     CREDIT ACCOUNT NAME                          
         MVI   DLPSTYPE,0          MAIN ACCOUNTING ENTRY                        
         ZAP   DLPSAMNT,PSTAMT     AMOUNT                                       
         MVC   DLPSANAL,OFFICE     OFFICE                                       
         ZIC   R1,DLPSLEN                                                       
         AR    R2,R1                                                            
         MVI   0(R2),0                                                          
*                                                                               
POST40   LA    R5,PSTLNQ(R5)       UP TO 4 POSTINGS PER LINE                    
         BCT   R0,POST20                                                        
         CLI   ACTIVITY,C'Y'                                                    
         BNE   POST50                                                           
         LA    RF,IOAREA-1         GET LENGTH OF IOAREA                         
         SR    R2,RF                                                            
         STH   R2,HALF                                                          
         MVC   IOAREA(2),HALF                                                   
         BAS   RE,PUTDAY           ADD THE ACCDAY FILE                          
*                                                                               
*              ADD ENTRY TO TWA1                                                
*                                                                               
         XC    WORK(20),WORK                                                    
         MVC   WORK(6),DOCNO                                                    
         MVC   WORK+10(4),DISKADDR                                              
         OI    WORK+10,X'80'       TAX ITEM - TURN 80 OF DISKSDDR               
         BAS   RE,ADTWA1                                                        
*                                                                               
POST50   LA    R2,IOAREA+2         SET R2 FOR NEXT ITEM                         
         USING DLDESCD,R2                                                       
         ZIC   R1,DLDSLEN                                                       
         AR    R2,R1                                                            
         BCT   R3,POST10           UP TO MAX ON SCREEN                          
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
RDHIGH   ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'ACCOUNT',LWKEY,LWIO               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVRE                                                         
         BR    RE                                                               
*                                                                               
BADLOC   MVI   ERRNUM,NOTFOUND        RECORD NOT FOUND                          
         MVC   BADACCNT(8),LWKEY+3    LOCALITY CODE                             
         OC    BADACCNT(8),SPACES                                               
         B     ERRXIT                                                           
*                                                                               
NORATE   MVI   ERRNUM,X'FE'                                                     
         MVC   MSG(20),=CL30'NO TAX RATE FOR'                                   
         MVC   XTRAMESS(8),LWKEY+3    LOCALITY CODE                             
         OC    XTRAMESS(8),SPACES                                               
         B     ERRXIT                                                           
*                                                                               
DUPERR   MVI   ERRNUM,35                                                        
         B     ERRXIT                                                           
         EJECT                                                                  
       ++INCLUDE ACBATCODE                                                      
         EJECT                                                                  
*              CONSTANTS                                                        
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR LOCAL W/S                                              
*                                                                               
LWSD     DSECT                                                                  
ADTAXB   DS    A                   A(USERS INPUT BLOCK)                         
ELCODE   DS    CL1                                                              
ACTIVITY DS    CL1                                                              
SCRTOT   DS    PL6                                                              
PL13     DS    PL13                                                             
SAVRE    DS    F                                                                
*                                                                               
OFFICE   DS    CL2                                                              
DOCNO    DS    CL6                                                              
PDATE    DS    CL3                                                              
ORDERNO  DS    CL6                                                              
*                                                                               
JOB      DS    CL15                JOB     CODE                                 
JBNME    DS    CL36                        NAME                                 
PRD      DS    CL15                PRODUCT CODE                                 
PRNME    DS    CL36                        NAME                                 
CLI      DS    CL15                CLIENT  CODE                                 
CLNME    DS    CL36                        NAME                                 
KEY      DS    CL49                                                             
         SPACE 2                                                                
*                                                                               
* ACBATSTAX                                                                     
       ++INCLUDE ACBATSTAX                                                      
*                                                                               
* ACGOBLOCK                                                                     
* GOBLOCKC DS    CL(GOBLOCKX-GOBLOCK)                                           
*                                                                               
*                                                                               
LWKEY    DS    CL42                                                             
LWIO     DS    1000C                                                            
MXLNES   EQU   10                  MAXIMUM NUMBER OF LINES                      
         DS    0F                                                               
IOAREA   DS    2000C                                                            
PSTABLE  DS    (MXLNES*PSTLINE)C  TABLE OF POSTING ACCOUNTS (14 LINES)          
LWSX     DS    0C                                                               
         EJECT                                                                  
*              DSECT TO COVER INPUT LINE                                        
TXD      DSECT                                                                  
TXLOCH   DS    CL(L'TAXLOCH)       HEADER FOR LOCALITY                          
TXLOC    DS    CL(L'TAXLOC)        LOCALITY                                     
TXLOCX   DS    CL(L'TAXLOCX)       TRAILER FOR LOCALITY                         
TXBASH   DS    CL(L'TAXTXBH)       HEADER FOR TAX BASIS                         
TXBAS    DS    CL(L'TAXTXB)        TAX BASIS                                    
TXBASX   DS    CL(L'TAXTXBX)       TRAILER FOR TAX BASIS                        
TXWKCH   DS    CL(L'TAXWKCH)       HEADER FOR WORKCODE                          
TXWKC    DS    CL(L'TAXWKC)        WORKCODE                                     
TXWKCX   DS    CL(L'TAXWKCX)       TRAILER FOR WORKCODE                         
TXDATAH  DS    CL(L'TAXDATAH)      HEADER FOR OTHER DATA                        
TXDATA   DS    CL(L'TAXDATA)       OTHER DATA                                   
TXDATAX  DS    CL(L'TAXDATAX)      TRAILER FOR OTHER DATA                       
TXLNQ    EQU   *-TXD               LENGTH OF INPUT LINE                         
         EJECT                                                                  
*                                                                               
*              DSECT TO COVER POSTING DATA FOR A LINE                           
*                                                                               
PSTD     DSECT                                                                  
PSTACC   DS    CL15                CREDIT ACCOUNT                               
PSTNME   DS    CL36                ACCOUNT NAME                                 
PSTLOC   DS    CL14                LOCALITY                                     
PSTLCNM  DS    CL36                LOCALITY NAME                                
PSTWKC   DS    CL2                 WORKCODE                                     
PSTEFF   DS    CL3                 EFFECTIVE DATE                               
PSTPCT   DS    PL4                 PERCENT                                      
PSTBAS   DS    PL6                 BASIS                                        
PSTAMT   DS    PL6                 POSTING AMOUNT                               
PSTLNQ   EQU   *-PSTD                                                           
PSTLINE  EQU   PSTLNQ*4            4 POSSIBLE ENTRIES PER LINE                  
         EJECT                                                                  
       ++INCLUDE ACBATDSECT                                                     
         ORG   CONTABH                                                          
       ++INCLUDE ACBATC9D                                                       
         ORG   OSVALS+200                                                       
* CALLING OVERLAYS (ACBAT01, ACBAT03) USE FIRST 200 BYTES                       
*                                                                               
USERL    EQU   OSVALSL-200                                                      
USERAREA DS    0F                                                               
LASTBASE DS    PL6                 BASIS                                        
LASTWKC  DS    CL2                 WORKCODE                                     
TAXTOT   DS    PL6                 TOTAL TAX                                    
COFFICE  DS    CL2                 CREDIT OFFICE                                
DETJOB   DS    CL12                JOB FROM DETAIL SCREEN                       
         DS    CL(USERL-(*-USERAREA))   SPARE                                   
         EJECT                                                                  
* ACGENBOTH                                                                     
* ACGENDAY                                                                      
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENDAY                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021ACBAT40   05/03/02'                                      
         END                                                                    
