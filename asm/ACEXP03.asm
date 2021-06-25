*          DATA SET ACEXP03    AT LEVEL 060 AS OF 01/27/04                      
*PHASE T61503A                                                                  
         TITLE 'T61503 - COKE EXPENDITURE - INVOICE ADD/CHA/DIS/DEL'            
T61503   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T61503,R7,RR=R2                                                
         L     RC,0(,R1)                                                        
*                                                                               
         USING GEND,RC                                                          
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         L     R8,ASPOOLD                                                       
*                                                                               
         USING T615FFD,RA                                                       
         USING SYSD,R9                                                          
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R6,SAVXTWA                                                       
*                                                                               
         USING LWSD,R6                                                          
*                                                                               
         ST    R2,RELO                                                          
         GOTO1 AUTH                                                             
         EJECT ,                                                                
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   ACC10                                                            
         BAS   RE,VALKY            VALIDATE THE INPUT FIELDS                    
*                                                                               
*              BUILD KEY FOR TRANSACTION ON SE LEDGER                           
*                                                                               
         USING ACKEYD,R4                                                        
*                                                                               
         LA    R4,KEY                                                           
         MVC   KEY,SPACES                                                       
         MVC   ACKEYACC(1),COMPANY                                              
         MVC   ACKEYACC+1(2),=C'SE'                                             
         MVC   ACKEYACC+3(5),ACN      BOTTLER ACN                               
         MVC   ACKEYACC+8(3),AGYCDE   AGENCY CODE                               
         MVC   ACKEYCON(1),COMPANY                                              
         MVC   ACKEYCON+1(2),=C'3P'                                             
         MVC   ACKEYCON+3(2),PRD      PRODUCT                                   
         MVC   ACKEYCON+5(2),MEDIA    MEDIA                                     
         MVC   ACKEYCON+7(8),VEHICLE  VEHICLE                                   
         MVC   ACKEYDTE,ADVDTE        DATE                                      
         MVC   ACKEYREF,INVOICE       INVOICE                                   
         MVI   ACKEYSBR,0                                                       
         OC    ACKEYACC+1(14),SPACES                                            
         MVC   ACKEYWRK,SPACES                                                  
         OC    ACKEYCON+1(14),SPACES                                            
         OC    ACKEYREF,SPACES                                                  
         MVC   SVKEY,KEY                                                        
         L     R4,AIO                                                           
         MVC   0(42,R4),SVKEY                                                   
         BAS   RE,BUDSP        DISPLAY BUDGET YTD BEFORE NEW ITEM               
         MVC   KEY,SVKEY                                                        
         B     XIT                                                              
         EJECT ,                                                                
ACC10    CLI   MODE,DISPREC        IF MODE IS DISPLAY                           
         BE    ACC15                                                            
         CLI   MODE,XRECADD        OR NEW RECORD ADDED                          
         BE    ACC15                                                            
         CLI   MODE,XRECPUT        OR RECORD CHANGED                            
         BE    ACC15                                                            
         CLI   MODE,XRECDEL        OR DELETED                                   
         BE    ACC15                                                            
         CLI   MODE,XRECREST       OR RESTORED                                  
         BE    ACC15                                                            
         B     ACC20                                                            
*                                                                               
ACC15    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         CLI   MODE,DISPREC                                                     
         BNE   XIT                                                              
         CLI   ACTNUM,ACTSEL                                                    
         BNE   XIT                                                              
         CLI   LOGAPP,C'A'         IF NOT APPROVED OK TO CHANGE                 
         BNE   XIT                                                              
         LA    R3,LISTDIR                                                       
         ZIC   R0,LISTNUM                                                       
*                                                                               
ACC12    CLI   0(R3),C'A'          IF DISPLAY FOR CHANGE                        
         BE    ACC14                                                            
         LA    R3,6(,R3)                                                        
         BCT   R0,ACC12                                                         
         B     XIT                                                              
*                                                                               
ACC14    MVI   0(R3),C' '          IGNORE THIS ENTRY FOR SELECTION              
         B     WARNING             AND PUT OUT WARNING MESSAGE                  
*                                                                               
ACC20    CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BNE   ACC30                                                            
         TM    SVSTAT,X'20'                                                     
         BO    ERR56               ACCOUNT IS LOCKED                            
         BAS   RE,BLDREC                                                        
         B     XIT                                                              
*                                                                               
ACC30    CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   ACC40                                                            
         BAS   RE,KEYDISP                                                       
         B     XIT                                                              
*                                                                               
ACC40    CLI   MODE,RECDEL                                                      
         BNE   ACC50                                                            
         TM    SVSTAT,X'20'                                                     
         BO    ERR56               ACCOUNT IS LOCKED                            
*              KEEP LAST 50 ELEMENT IN CASE RECORD  DELETED                     
         GOTO1 GETL,DMCB,(X'50',AIO),0                                          
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,ELADDR                                                        
         ZIC   R1,1(,R3)                                                        
         BCTR  R1,0                                                             
         EXMVC R1,LASTSUB,0(R3)                                                 
         BAS   RE,DELREC           TEST IF OK TO DELETE                         
         BAS   RE,CHASUB           REMOVE OLD CASH BUCKETS                      
         B     XIT                                                              
*                                                                               
ACC50    CLI   MODE,RECPUT                                                      
         BNE   ACC60                                                            
         TM    SVSTAT,X'20'                                                     
         BO    ERR56               ACCOUNT IS LOCKED                            
         BAS   RE,DELREC           TEST IF OK TO CHANGE                         
         BAS   RE,CHASUB           REMOVE OLD CASH BUCKETS/ADD NEW              
         BAS   RE,UPACN            FOR  CHANGE UPDATE ACN ON 3M                 
         B     XIT                                                              
*                                                                               
ACC60    CLI   MODE,RECADD                                                      
         BNE   ACC70                                                            
         TM    SVSTAT,X'20'                                                     
         BO    ERR56               ACCOUNT IS LOCKED                            
         BAS   RE,UPSUB            UPDATE CASH BUCKETS                          
         BAS   RE,UPACN            FOR ADD OR CHANGE UPDATE ACN ON 3M           
         B     XIT                                                              
*                                                                               
ACC70    CLI   MODE,RECREST                                                     
         BNE   XIT                                                              
         TM    SVSTAT,X'20'                                                     
         BO    ERR56               ACCOUNT IS LOCKED                            
         BAS   RE,UPSUB            UPDATE CASH BUCKETS                          
         B     XIT                                                              
         EJECT ,                                                                
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         MVC   LOGAPP,SPACES                                                    
         OI    LOGAPPH+6,X'80'                                                  
         MVC   LOGGAMT,SPACES                                                   
         OI    LOGGAMTH+6,X'80'                                                 
         MVC   LOGNAMT,SPACES                                                   
         OI    LOGNAMTH+6,X'80'                                                 
         MVC   LOGCSHD,SPACES                                                   
         OI    LOGCSHDH+6,X'80'                                                 
         MVC   LOGCDAM,SPACES                                                   
         OI    LOGCDAMH+6,X'80'                                                 
         MVC   LOGGYTD,SPACES                                                   
         OI    LOGGYTDH+6,X'80'                                                 
         MVC   LOGBAMT,SPACES                                                   
         OI    LOGBAMTH+6,X'80'                                                 
         MVC   LOGPBUD,SPACES                                                   
         OI    LOGPBUDH+6,X'80'                                                 
         MVC   LOGYEAR,SPACES                                                   
         OI    LOGYEARH+6,X'80'                                                 
         MVC   LOGCTGY,SPACES                                                   
         OI    LOGCTGYH+6,X'80'                                                 
         MVC   LOGCMT,SPACES                                                    
         OI    LOGCMTH+6,X'80'                                                  
         MVC   LOGCMT2,SPACES                                                   
         OI    LOGCMT2H+6,X'80'                                                 
*                                                                               
         GOTO1 GETL,DMCB,(X'44',AIO),0                                          
         L     R3,ELADDR                                                        
*                                                                               
         USING TRANSD,R3                                                        
*                                                                               
         CLI   TRNSLEN,30                                                       
         BL    DISPLAY1                                                         
         MVI   BLOCK,C' '                                                       
         MVC   BLOCK+1(200),BLOCK                                               
         ZIC   R5,TRNSLEN                                                       
         SH    R5,=H'29'                                                        
         CH    R5,=H'49'                                                        
         BH    DISPLAYA                                                         
         EXMVC R5,LOGCMT,TRNSNARR                                               
         B     DISPLAY1                                                         
*                                                                               
DISPLAYA MVC   LOGCMT,TRNSNARR                                                  
         SH    R5,=H'50'                                                        
         BM    DISPLAY1                                                         
         CH    R5,=H'49'                                                        
         BL    *+8                                                              
         LH    R5,=H'49'                                                        
         EXMVC R5,LOGCMT2,TRNSNARR+50                                           
*                                                                               
DISPLAY1 GOTO1 GETL,DMCB,(X'50',AIO),0                                          
         L     R3,ELADDR                                                        
*                                                                               
         USING TRCASHD,R3                                                       
*                                                                               
         EDIT  (P6,TRCSGRS),(10,LOGGAMT),2,FLOAT=-,ALIGN=LEFT                   
         EDIT  (P6,TRCSNET),(10,LOGNAMT),2,FLOAT=-,ALIGN=LEFT                   
*                                                                               
         GOTO1 GETL,DMCB,(X'46',AIO),0                                          
         L     R3,ELADDR                                                        
*                                                                               
         USING TRPAYD,R3                                                        
*                                                                               
         CP    TRPYCD,=P'0'                                                     
         BE    DISPLY1A            NO CASH DISCOUNT                             
         MVI   LOGCSHD,C'Y'                                                     
         EDIT  (P6,TRPYCD),(10,LOGCDAM),2,FLOAT=-,ALIGN=LEFT                    
*                                                                               
DISPLY1A CLI   TRPYLEN,TRPYLN2Q                                                 
         BL    DISPLAY2                                                         
         MVC   LOGCTGY,TRPYCAT                                                  
*                                                                               
DISPLAY2 GOTO1 GETL,DMCB,(X'60',AIO),0                                          
         L     R3,ELADDR                                                        
*                                                                               
         USING TRSTATD,R3                                                       
*                                                                               
         OC    TRSTUPDT,TRSTUPDT                                                
         BZ    DISPLAY3            NOT APPROVED                                 
         MVC   LOGAPP(8),=C'APPROVED'                                           
         GOTO1 DATCON,DMCB,(2,TRSTUPDT),(8,LOGAPP+9)                            
*                                                                               
DISPLAY3 BAS   RE,BUDSP           DISPLAY BUDGETS                               
         B     XIT                                                              
         EJECT ,                                                                
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
BLDREC   NTR1                                                                   
*              KEEP LAST 50 ELEMENT IN CASE RECORD  CHANGED                     
         GOTO1 GETL,DMCB,(X'50',AIO),0                                          
         CLI   ELERR,0                                                          
         BNE   BLDREC10                                                         
         L     R3,ELADDR                                                        
         ZIC   R1,1(,R3)                                                        
         BCTR  R1,0                                                             
         EXMVC R1,LASTSUB,0(R3)                                                 
*                                                                               
BLDREC10 GOTO1 DELL,DMCB,(X'44',AIO),0                                          
         GOTO1 DELL,DMCB,(X'46',AIO),0                                          
         GOTO1 DELL,DMCB,(X'50',AIO),0                                          
*                                                                               
         LA    R2,LOGGAMTH         CHECK GROSS AMOUNT                           
         GOTO1 ANY                                                              
         LA    R3,LOGGAMT                                                       
         ZIC   R0,LOGGAMTH+5                                                    
         CLC   AGYCDE,=C'100'                                                   
         BH    *+14                                                             
         ZAP   NETPCT,=P'10000'   FOR B.B 000-099 DEFAULT NET = GROSS           
         B     BLDREC18             DON'T ALLOW LETTERS                         
*                                                                               
         ZAP   NETPCT,=P'9250'      DEFAULT IS 92.50 PERCENT                    
         CLC   AGYCDE,=C'900'       FOR AGENCY 100-899                          
         BL    *+10                                                             
         ZAP   NETPCT,=P'8500'     FOR MCCANN DEFAULT IS 85.00                  
         CLI   LOGGAMT,C'F'                                                     
         BNE   BLDREC12                                                         
         ZAP   NETPCT,=P'10000'    F IN GROSS  THEN NET = GROSS                 
         SH    R0,=H'1'                                                         
         LA    R3,1(,R3)                                                        
*                                                                               
BLDREC12 CLI   LOGGAMT,C'P'                                                     
         BNE   BLDREC14                                                         
         ZAP   NETPCT,=P'9250'     P IN GROSS THEN NET = 92.50 PERCENT          
         SH    R0,=H'1'                                                         
         LA    R3,1(,R3)                                                        
*                                                                               
BLDREC14 CLI   LOGGAMT,C'S'                                                     
         BNE   BLDREC16                                                         
         ZAP   NETPCT,=P'9166'     S IN GROSS THEN NET = 91.66 PERCENT          
         SH    R0,=H'1'                                                         
         LA    R3,1(,R3)                                                        
*                                                                               
BLDREC16 CLI   LOGGAMT,C'V'                                                     
         BNE   BLDREC17                                                         
         ZAP   NETPCT,=P'8850'     V IN GROSS THEN NET = 88.50 PERCENT          
         SH    R0,=H'1'                                                         
         LA    R3,1(,R3)                                                        
*                                                                               
BLDREC17 CLI   LOGGAMT,C'X'                                                     
         BNE   BLDREC18                                                         
         ZAP   NETPCT,=P'8900'     V IN GROSS THEN NET = 89.00 PERCENT          
         SH    R0,=H'1'                                                         
         LA    R3,1(,R3)                                                        
*                                                                               
BLDREC18 GOTO1 CASHVAL,DMCB,(R3),(R0)                                           
         CLI   DMCB,X'FF'                                                       
         BE    FLDINV                                                           
         L     R1,DMCB+4                                                        
         CVD   R1,DUB                                                           
         ZAP   GROSS,DUB                                                        
*                                                                               
         ZAP   PWRK,DUB            CALCULATE NET                                
         MP    PWRK,NETPCT                                                      
         AP    PWRK,=P'5000'                                                    
         CP    PWRK,=P'0'                                                       
         BNL   *+10                                                             
         SP    PWRK,=P'10000'                                                   
         DP    PWRK,=P'10000'                                                   
         ZAP   NET,PWRK(10)                                                     
*                                                                               
         LA    R2,LOGNAMTH         CHECK NET AMOUNT                             
         CLI   5(R2),0                                                          
         BE    BLDREC20                                                         
         CLC   AGYCDE,=C'000'                                                   
         BE    FLDINV              FOR AGENCY 000 NET IS NOT ALLOWED            
         ZIC   R0,LOGNAMTH+5                                                    
         GOTO1 CASHVAL,DMCB,LOGNAMT,(R0)                                        
         CLI   DMCB,X'FF'                                                       
         BE    FLDINV                                                           
         L     R1,DMCB+4                                                        
         CVD   R1,DUB                                                           
         ZAP   NET,DUB                                                          
*                                                                               
BLDREC20 ZAP   DUB,GROSS         CHECK GROSS MUST BE HIGHER THAN NET            
         CVB   RF,DUB                                                           
         LPR   RF,RF                                                            
         ZAP   DUB,NET                                                          
         CVB   RE,DUB                                                           
         LPR   RE,RE                                                            
         LA    R2,LOGNAMTH                                                      
         CR    RE,RF                                                            
         BH    FLDINV              NET IS HIGHER THAN GROSS                     
*                                                                               
         LA    R2,LOGCSHDH                                                      
         ZAP   CDPCT,=P'0'                                                      
         ZAP   CSHD,=P'0'                                                       
         CLC   LOGMED,=C'PN'                                                    
         BNE   BLDREC50            NOT PRINT NEWSPAPER                          
         CLC   AGYCDE,=C'000'       IF BOTTLER NO CD                            
         BE    BLDREC50                                                         
         CLI   LOGCSHDH+5,0        IF SPECIFIED USE IT                          
         BNE   BLDREC30                                                         
         CLC   AGYCDE,=C'900'      IF NOT SPECIFIED AND AGENCY BUY              
         BL    BLDREC50            NO CD                                        
         B     BLDREC40            DEFAULT FOR MCCANN IS CD                     
*                                                                               
BLDREC30 CLI   LOGCSHD,C'N'                                                     
         BE    BLDREC50            DON'T TAKE C.D.                              
         CLI   LOGCSHD,C'Y'                                                     
         BNE   FLDINV                                                           
*                                                                               
BLDREC40 ZAP   CDPCT,=P'200'                                                    
         ZAP   PWRK,NET                                                         
         MP    PWRK,CDPCT                                                       
         AP    PWRK,=P'5000'                                                    
         CP    PWRK,=P'0'                                                       
         BNL   *+10                                                             
         SP    PWRK,=P'10000'                                                   
         DP    PWRK,=P'10000'                                                   
         ZAP   CSHD,PWRK(10)                                                    
*                                                                               
*              BUILD ELEMENTS                                                   
*                                                                               
         USING TRANSD,R3                                                        
*                                                                               
BLDREC50 LA    R3,EXPELM                                                        
         XC    EXPELM,EXPELM                                                    
         MVI   TRNSEL,X'44'                                                     
         MVC   TRNSLEN,=AL1(TRNSNARR-TRANSD)                                    
         MVC   TRNSDATE,ADVDTE                                                  
         MVC   TRNSREF,INVOICE                                                  
         OC    TRNSREF,SPACES                                                   
         MVI   TRNSSBRF,0                                                       
         MVI   TRNSTYPE,0                                                       
         MVI   TRNSSTAT,X'80'                                                   
         MVC   TRNSBTCH,SPACES                                                  
         ZIC   R1,TRNSDATE         CONVERT YYMMDD TO MOS                        
         SLL   R1,28                                                            
         SRL   R1,28                                                            
         STC   R1,TRNSBTCH                                                      
         OI    TRNSBTCH,X'F0'                                                   
         ZIC   R1,TRNSDATE+1                                                    
         SLL   R1,28                                                            
         SRL   R1,28                                                            
         STC   R1,TRNSBTCH+1                                                    
         OI    TRNSBTCH+1,X'F0'                                                 
         CLI   TRNSDATE+1,X'10'                                                 
         BL    BLDREC60                                                         
         MVI   TRNSBTCH+1,C'A'                                                  
         CLI   TRNSDATE+1,X'11'                                                 
         BL    BLDREC60                                                         
         MVI   TRNSBTCH+1,C'B'                                                  
         CLI   TRNSDATE+1,X'12'                                                 
         BL    BLDREC60                                                         
         MVI   TRNSBTCH+1,C'C'                                                  
*                                                                               
BLDREC60 ZAP   TRNSAMNT,=P'0'                                                   
         MVC   TRNSANAL,SPACES                                                  
         LA    R2,LOGCMTH                                                       
         XR    R4,R4                                                            
         LA    R0,2                                                             
*                                                                               
BLDREC70 CLI   5(R2),0                                                          
         BE    BLDREC80            NO COMMENT ON THIS LINE                      
         IC    R4,0(,R2)                                                        
         SH    R4,=H'8'                                                         
         ZIC   R1,TRNSLEN                                                       
         LA    RF,TRNSEL(R1)       RF TO NEXT SPACE IN NARRATIVE                
         AR    R1,R4                                                            
         STC   R1,TRNSLEN          FIX ELEMENT LENGTH                           
         BCTR  R4,0                                                             
         EXMVC R4,0(RF),8(R2)      COMMENT TO ELEMENT                           
         EX    R4,*+8                                                           
         B     *+10                                                             
         OC    0(0,RF),SPACES      UPPER CASE THE COMMENT                       
*                                                                               
BLDREC80 IC    R4,0(,R2)                                                        
         AR    R2,R4                                                            
         BCT   R0,BLDREC70         NEXT FIELD                                   
         EJECT ,                                                                
         GOTO1 ADDL,DMCB,AIO,EXPELM           ADD TRANSACTION ELEMENT           
*                                                                               
         USING TRCASHD,R3                                                       
*                                                                               
         XC    EXPELM,EXPELM                                                    
         MVC   TRCSEL(2),=X'500F'                                               
         MVI   TRCSTYPE,C'E'                                                    
         ZAP   TRCSGRS,GROSS                                                    
         ZAP   TRCSNET,NET                                                      
         GOTO1 ADDL,DMCB,AIO,EXPELM           SUBSIDIARY CASH ELEMENT           
*                                                                               
         USING TRPAYD,3                                                         
*                                                                               
BLDREC7  LA    R3,EXPELM                                                        
         XC    EXPELM,EXPELM                                                    
         MVI   TRPYEL,X'46'                                                     
         MVI   TRPYLEN,TRPYLN2Q                                                 
         ZAP   TRPYCD,CSHD                                                      
         MVC   TRPYCLI,SPACES      KEEP VEHICLE NAME IN 46 ELEMENT              
         MVC   TRPYPROD,SPACES                                                  
         MVC   TRPYCLI(36),LOGVEHN                                              
         OC    TRPYCLI(36),SPACES                                               
         MVC   TRPYCAT,LOGCTGY     SPOT ESTIMATE CATEGORY                       
         OC    TRPYCAT,SPACES                                                   
         GOTO1 ADDL,DMCB,AIO,EXPELM           ADD EXTRA PAY ELEMENT             
*                                                                               
*              ADD A 60 ELEMENT                                                 
         GOTO1 GETL,DMCB,(X'60',AIO),0                                          
         CLI   ELERR,0                                                          
         BE    BLDRX               ALREADY HAVE A 60 ELEMENT                    
*                                                                               
         USING TRSELD,R3                                                        
*                                                                               
         LA    R3,EXPELM                                                        
         XC    EXPELM,EXPELM                                                    
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         MVC   TRSDATE,TODAY2      TODAY COMPRESSED                             
         MVC   TRSPMOS,ADVDTE                                                   
         MVC   TRSUSER,TWAORIG                                                  
         GOTO1 ADDL,DMCB,AIO,EXPELM           ADD STATUS ELEMENT                
*                                                                               
BLDRX    B     XIT                                                              
         EJECT ,                                                                
*              VALIDATE KEY FIELDS                                              
         SPACE 1                                                                
         USING ACKEYD,R4                                                        
         SPACE 1                                                                
VALKY    NTR1                                                                   
         MVC   LOGMEDN,SPACES                                                   
         OI    LOGMEDNH+6,X'80'                                                 
         MVC   LOGVEHN,SPACES                                                   
         OI    LOGVEHNH+6,X'80'                                                 
         MVC   LOGACNN,SPACES                                                   
         OI    LOGACNNH+6,X'80'                                                 
         MVC   LOGAGYN,SPACES                                                   
         OI    LOGAGYNH+6,X'80'                                                 
         MVC   LOGPRDN,SPACES                                                   
         OI    LOGPRDNH+6,X'80'                                                 
*                                                                               
*              VALIDATE MEDIA                                                   
*                                                                               
VALMED   LA    R4,KEY                                                           
         MVC   AIO,AIO2                                                         
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),COMPANY                                              
         MVC   ACKEYACC+1(2),=C'3M' MEDIA / VEHICLE LEDGER                      
         LA    R2,LOGMEDH                                                       
         GOTO1 ANY                                                              
         MVC   ACKEYACC+3(2),WORK                                               
         MVC   MEDIA,WORK                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         L     R3,AIO                                                           
         CLC   KEYSAVE(42),0(R3)                                                
         BNE   NOACCT              NO HIGH LEVEL MEDIA VEHILCE                  
         GOTO1 NAMOUT,DMCB,AIO,WORK                                             
         MVC   LOGMEDN,WORK        LEVEL 1 NAME                                 
*                                                                               
*              VALIDATE VEHICLE                                                 
*                                                                               
VALVEH   LA    R2,LOGVEHH                                                       
         GOTO1 ANY                                                              
         MVC   ACKEYACC+5(8),WORK                                               
         MVC   VEHICLE,WORK                                                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         L     R3,AIO                                                           
         CLC   KEYSAVE(42),0(R3)                                                
         BNE   NOACCT                                                           
         GOTO1 NAMOUT,DMCB,AIO,WORK                                             
         MVC   LOGVEHN,WORK        LEVEL 2 NAME                                 
*                                                                               
*              VALIDATE BOTTLER ACN NUMBER                                      
*                                                                               
VALACN   MVC   ACKEYACC(42),SPACES   VALIDATE BOTTLER ACN                       
         MVC   ACKEYACC(1),COMPANY                                              
         MVC   ACKEYACC+1(2),=C'SE'                                             
         LA    R2,LOGACNH                                                       
         CLI   5(R2),0                                                          
         BNE   VALACN3                                                          
         GOTO1 GETL,DMCB,(X'23',AIO),0                                          
         CLI   ELERR,0                                                          
         BE    VALACN1                                                          
         GOTO1 ANY                 NO BOTTLER CODE IN 3M RECORD                 
*                                                                               
         USING ACOTHERD,R3                                                      
*                                                                               
VALACN1  L     R3,ELADDR                                                        
         MVC   LOGACN,ACOTNUM                                                   
         OI    LOGACNH+6,X'80'                                                  
         MVI   5(R2),5                                                          
*                                                                               
VALACN3  ZIC   R1,LOGACNH+5                                                     
         BCTR  R1,0                                                             
         EXMVC R1,ACKEYACC+3,LOGACN                                             
         MVC   ACN,LOGACN                                                       
         OC    ACN,SPACES                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         L     R3,AIO                                                           
         CLC   KEYSAVE(42),0(R3)                                                
         BNE   NOACCT                                                           
         GOTO1 NAMOUT,DMCB,AIO,WORK                                             
         MVC   LOGACNN,WORK        BOTTLER NAME                                 
*                                                                               
*              VALIDATE AGENCY CODE                                             
*                                                                               
VALAGY   LA    R2,LOGAGYH                                                       
         CLI   5(R2),0                                                          
         BE    VALAGY3                                                          
         CLI   TWAOFFC,C'*'                                                     
         BE    VALAGY1                                                          
         CLC   AGYSIGN(5),=C'CCUSA' HARD AS NAILS                               
         BE    VALAGY1             ONLY IPG CAN INPUT ACN NUMBER                
         CLC   AGYSIGN(4),=C'CCAT'                                              
         BNE   FLDINV                                                           
*                                                                               
VALAGY1  CLI   5(R2),3                                                          
         BNE   FLDINV                MUST BE THREE                              
         TM    4(R2),X'08'                                                      
         BNO   ERR03               NOT NUMERIC                                  
         GOTO1 ANY                                                              
         MVC   ACKEYACC+8(3),WORK                                               
         MVC   AGYCDE,WORK                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         L     R3,AIO                                                           
         CLC   KEYSAVE(42),0(R3)                                                
         BNE   NOACCT                                                           
         GOTO1 NAMOUT,DMCB,AIO,WORK                                             
         MVC   LOGAGYN,WORK        AGENCY NAME                                  
         GOTO1 GETL,DMCB,(X'30',AIO),0                                          
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                NO STATUS ELEMENT                            
*                                                                               
         USING ACSTATD,R3                                                       
*                                                                               
         L     R3,ELADDR                                                        
         MVC   SVSTAT,ACSTSTAT                                                  
         B     VALSX                                                            
*                                                                               
*              VALIDATE AGENCY FROM SIGN-ON                                     
*                                                                               
VALAGY3  CLI   AGYNUM,X'FF'                                                     
         BNE   VALAGY4                                                          
         GOTO1 ANY    IF SIGN ON NOT NUMERIC MUST INPUT NUMBER                  
*                                                                               
VALAGY4  MVC   ACKEYACC+8(3),AGYNUM                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         L     R3,AIO                                                           
         CLC   KEYSAVE(42),0(R3)                                                
         BNE   NOACCT                                                           
         MVC   AGYCDE,AGYNUM                                                    
         GOTO1 NAMOUT,DMCB,AIO,WORK                                             
         MVC   LOGAGYN,WORK        AGENCY NAME                                  
*                                                                               
*              VALIDATE SX LEDGER                                               
*                                                                               
VALSX    CLC   AGYCDE,=C'100'                                                   
         BL    VALPRD              DON'T NEED SX FOR BOTTLER BOUGHT             
         CLC   AGYCDE,=C'899'      IS IT MC CANN                                
         BNH   VALSX1              IF NOT, MUST HAVE SX RECORD                  
         B     VALPRD              OK                                           
*                                                                               
VALSX1   MVC   ACKEYACC+1(2),=C'SX'                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         L     R3,AIO                                                           
         CLC   KEYSAVE(42),0(R3)                                                
         BNE   NOACCT                                                           
*                                                                               
*              VALIDATE PRODUCT                                                 
*                                                                               
VALPRD   MVC   ACKEYACC(42),SPACES   VALIDATE PRODUCT                           
         MVC   ACKEYACC(1),COMPANY                                              
         MVC   ACKEYACC+1(2),=C'3P'                                             
         LA    R2,LOGPRDH                                                       
         GOTO1 ANY                                                              
         MVC   ACKEYACC+3(2),WORK                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         L     R3,AIO                                                           
         CLC   KEYSAVE(42),0(R3)                                                
         BNE   NOACCT                                                           
         MVC   PRD,LOGPRD                                                       
         GOTO1 NAMOUT,DMCB,AIO,WORK                                             
         MVC   LOGPRDN,WORK        PRODUCT NAME                                 
         GOTO1 GETL,DMCB,(X'23',AIO),0                                          
         CLI   ELERR,0                                                          
         BNE   NOACCT              NO BUDGET CODE                               
*                                                                               
         USING ACOTHERD,R3                                                      
*                                                                               
         L     R3,ELADDR                                                        
         MVC   PRDBUDC,ACOTNUM     PRODUCT BUDGET CODE                          
*                                                                               
*              VALIDATE INVOICE                                                 
*                                                                               
VALINV   LA    R2,LOGINVH          INVOICE                                      
         GOTO1 ANY                                                              
         MVC   INVOICE,WORK                                                     
*                                                                               
*              VALIDATE DATE                                                    
*                                                                               
VALDTE   LA    R2,LOGADTEH                                                      
         GOTO1 ANY                                                              
         GOTO1 DATVAL,DMCB,(0,LOGADTE),WORK                                     
         OC    DMCB(4),DMCB                                                     
         BZ    VALDTE3             MM/DD/YY IS VALID                            
         GOTO1 DATCON,DMCB,(0,WORK),(1,ADVDTE)                                  
         B     VALDTE5                                                          
*                                                                               
VALDTE3  CLI   5(R2),6                                                          
         BH    ERR13                                                            
         GOTO1 DATVAL,DMCB,(2,LOGADTE),WORK                                     
         OC    DMCB(4),DMCB                                                     
         BZ    ERR13               MM/YY IS ALSO VALID                          
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,ADVDTE)                                  
*                                                                               
VALDTE5  GOTO1 DATCON,DMCB,(0,WORK),(20,WORK+6)                                 
         CLC   WORK+6(4),=C'1985'  DATE BEFORE 1985 IS INVALID                  
         BL    ERR13                                                            
*                                                                               
VALXIT   MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT ,                                                                
*              DISPLAY THE KEY FIELDS                                           
         SPACE 1                                                                
         USING ACKEYD,R4                                                        
         SPACE 1                                                                
KEYDISP  NTR1                                                                   
         L     R4,AIO                                                           
         MVC   LOGMED,ACKEYCON+5         MEDIA                                  
         MVI   LOGMEDH+5,L'LOGMED                                               
         OI    LOGMEDH+6,X'80'                                                  
         MVC   LOGVEH,ACKEYCON+7         VEHICLE                                
         MVI   LOGVEHH+5,L'LOGVEH                                               
         OI    LOGVEHH+6,X'80'                                                  
         MVC   LOGACN,ACKEYACC+3         BOTTLER ACN                            
         MVI   LOGACNH+5,L'LOGACN                                               
         OI    LOGACNH+6,X'80'                                                  
         MVC   LOGAGY,ACKEYACC+8         AGENCY                                 
         MVI   LOGAGYH+5,L'LOGAGY                                               
         OI    LOGAGYH+4,X'08'           NUMERIC                                
         OI    LOGAGYH+6,X'80'                                                  
         MVC   LOGPRD,ACKEYCON+3         PRODUCT                                
         MVI   LOGPRDH+5,L'LOGPRD                                               
         OI    LOGPRDH+6,X'80'                                                  
         MVC   LOGINV,SPACES                                                    
         MVC   LOGINV,ACKEYREF            INVOICE                               
         MVI   LOGINVH+5,L'LOGINV                                               
         OI    LOGINVH+6,X'80'                                                  
         MVC   LOGADTE,SPACES                                                   
         OI    LOGADTEH+6,X'80'                                                 
         GOTO1 DATCON,DMCB,(1,ACKEYDTE),(8,LOGADTE)  MMMDD/YY                   
         MVI   LOGADTEH+5,L'LOGADTE                                             
*                                                                               
KEYDSPX  BAS   RE,VALKY                                                         
         B     XIT                                                              
         EJECT ,                                                                
*              DISPLAY BUDGET AND YTD AMOUNTS                                   
         SPACE 1                                                                
BUDSP    NTR1                                                                   
         MVC   LOGGYTD,SPACES                                                   
         OI    LOGGYTDH+6,X'80'                                                 
         MVC   LOGBAMT,SPACES                                                   
         OI    LOGBAMTH+6,X'80'                                                 
         MVC   LOGPBUD,SPACES                                                   
         OI    LOGPBUDH+6,X'80'                                                 
         MVC   LOGYEAR,SPACES                                                   
         OI    LOGYEARH+6,X'80'                                                 
         L     R4,AIO                                                           
         ZAP   YTD,=P'0'                                                        
         ZAP   BUD,=P'0'                                                        
         MVC   YEAR,ACKEYDTE                     YY   PACKED                    
         GOTO1 DATCON,DMCB,(1,ACKEYDTE),(20,DUB) YYYYMMDD                       
         MVI   LOGYEAR,C'('                      (                              
         MVC   LOGYEAR+1(4),DUB                   YYYY                          
         MVI   LOGYEAR+5,C')'                         )                         
         BAS   RE,GTBUD                                                         
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(ACKEYDTE-ACKEYD),ACKEYACC     KEY FOR CONTRA                 
         LA    R4,KEY                                                           
         MVC   ACKEYCON+7(8),SPACES              WITHOUT VEHICLE                
         MVI   ACKEYREF,C'E'                     TOTAL IS BUCKTYPE E            
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         L     R4,AIO                                                           
         CLC   KEYSAVE(ACLENGTH-ACKEYD),0(R4)                                   
         BNE   BUDSP10                                                          
         LA    R3,ACRECORD         GET YTD FROM GROSS(DEBIT) BUCKETS            
*                                                                               
BUDSP3   CLI   0(R3),0                                                          
         BE    BUDSP10                                                          
         CLI   0(R3),X'45'                                                      
         BE    BUDSP7                                                           
*                                                                               
BUDSP5   ZIC   R0,1(,R3)                                                        
         AR    R3,R0                                                            
         B     BUDSP3                                                           
*                                                                               
         USING TRHISTD,R3                                                       
*                                                                               
BUDSP7   CLC   TRHSYEAR,YEAR                                                    
         BNE   BUDSP5                                                           
         AP    YTD,TRHSDR                                                       
         B     BUDSP5                                                           
*                                                                               
BUDSP10  AP    YTD,=P'50'                                                       
         ZAP   PWRK,YTD                                                         
         DP    PWRK,=P'100'                                                     
         ZAP   YTD,PWRK(11)                                                     
*                                                                               
BUDSP30  EDIT  (P6,YTD),(10,LOGGYTD),FLOAT=$                                    
         MVC   LOGBAMT+8(2),=C'$0'                                              
         CP    BUD,=P'0'                                                        
         BE    BUDSP40                                                          
         EDIT  (P6,BUD),(10,LOGBAMT),FLOAT=$                                    
         CLI   QTRNUM,0                                                         
         BE    BUDSP40                                                          
         MVC   LOGYEAR+6(6),LOGYEAR  (YYYY)                                     
         MVC   LOGYEAR(6),=C'X QTR '                                            
         EDIT  (B1,QTRNUM),(1,LOGYEAR)                                          
*                                                                               
BUDSP40  CP    YTD,=P'0'                                                        
         BE    BUDSPX                                                           
         CP    BUD,=P'0'                                                        
         BE    BUDSPX                                                           
         MVC   LOGPBUD+4(6),=C'*OVER*'                                          
         CP    YTD,BUD                                                          
         BH    BUDSPX                                                           
         MVC   LOGPBUD,SPACES                                                   
         ZAP   PWRK,YTD                                                         
         MP    PWRK,=P'1000'                                                    
         DP    PWRK,BUD                                                         
         LA    R3,LOGPBUD+5                                                     
         EDIT  (P7,PWRK),(4,0(R3)),1                                            
         MVI   LOGPBUD+9,C'%'                                                   
*                                                                               
BUDSPX   MVC   AIO,AIO1            RESET IO AREA                                
         B     XIT                                                              
         EJECT ,                                                                
*              GET THE BUDGET AMOUNT                                            
         SPACE 1                                                                
GTBUD    NTR1                                                                   
         L     R4,AIO              TRANSACTION RECORD                           
         MVC   BUDST(1),ACKEYDTE   BUDGET YEAR                                  
         MVI   BUDST+1,1           START AT JANUARY                             
         MVC   BUDEN(1),ACKEYDTE                                                
         MVI   BUDEN+1,X'12'       THRU DECEMBER                                
         MVI   QTRNUM,0            QUARTER NUMBER                               
         CLC   MEDIA,=C'ST'        SPOT TV IS BY QUARTER                        
         B     GTBUD5                                                           
         BNE   GTBUD5                                                           
         LA    R1,BUDPER                                                        
*                                                                               
GTBUD3   CLI   0(R1),X'FF'                                                      
         BE    XIT                                                              
         CLC   0(1,R1),ACKEYDTE+1  MONTH IN TABLE VS KEY MONTH                  
         BE    GTBUD4                                                           
         LA    R1,4(,R1)                                                        
         B     GTBUD3                                                           
*                                                                               
         DROP  R4                                                               
*                                                                               
GTBUD4   MVC   BUDST+1(1),1(R1)    START                                        
         MVC   BUDEN+1(1),2(R1)    END MONTHS FOR QUARTER                       
         MVC   QTRNUM,3(R1)        1,2,3,4 QUARTER                              
*                                                                               
GTBUD5   XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
*                                                                               
         USING ACBTKEY,RF                                                       
*                                                                               
         MVI   ACBTKTYP,ACBTKTEQ   BUILD BUDGET RECORD KEY                      
         MVC   ACBTKACC,ACKEYACC-ACKEYD(R4)                                     
         MVC   ACBTKWRK,SPACES                                                  
         MVC   ACBTKCON,SPACES                                                  
         MVC   ACBTKCON(7),ACKEYCON-ACKEYD(R4)                                  
         MVC   ACBTKCON+3(2),PRDBUDC                                            
         MVI   ACBTKBNO+1,1                                                     
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         L     R4,AIO2                                                          
         CLC   KEYSAVE(ACLENGTH-ACKEYD),0(R4)                                   
         BNE   XIT                 NO BUDGET RECORD                             
         LA    R3,ACRECORD-ACKEYD(,R4)                                          
*                                                                               
GTBUD7   CLI   0(R3),0                                                          
         BE    GTBUD10                                                          
         CLI   0(R3),X'1D'                                                      
         BE    GTBUD9                                                           
*                                                                               
GTBUD8   ZIC   R0,1(,R3)                                                        
         AR    R3,R0                                                            
         B     GTBUD7                                                           
*                                                                               
         USING ACBAD,R3                                                         
*                                                                               
GTBUD9   CLC   ACBAMNTH,BUDST      MUST BE WITHIN BUDGET PERIOD                 
         BL    GTBUD8                                                           
         CLC   ACBAMNTH,BUDEN                                                   
         BH    GTBUD8                                                           
         AP    BUD,ACBABUDG                                                     
         B     GTBUD8                                                           
*                                                                               
GTBUD10  CP    BUD,=P'0'                                                        
         BE    XIT                                                              
         ZAP   PWRK,BUD                                                         
         DP    PWRK,=P'100'                                                     
         ZAP   BUD,PWRK(11)                                                     
         B     XIT                                                              
         EJECT ,                                                                
*              TEST IF TRANSACTION IS ELIGIBLE TO BE DELETED                    
         SPACE 1                                                                
         USING ACKEYD,R4                                                        
         SPACE 1                                                                
DELREC   NTR1                                                                   
         L     R4,AIO                                                           
         MVC   KEY,0(R4)                                                        
         MVC   AIO,AIO2                                                         
         GOTO1 READ                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETL,DMCB,(X'44',AIO2),0                                         
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                NO 64 ELEMENT                                
*                                                                               
         USING TRANSD,R3                                                        
*                                                                               
         L     R3,ELADDR                                                        
         TM    TRNSSTAT,X'20'                                                   
         BO    CANTCHA         CAN'T CHANGE OR DELETE OFFSETTING                
         CP    TRNSAMNT,=P'0'                                                   
         BNE   CANTCHA                                                          
*                                                                               
         GOTO1 GETL,DMCB,(X'60',AIO2),0                                         
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                NO 60 ELEMENT                                
*                                                                               
         USING TRSTATD,R3                                                       
*                                                                               
         L     R3,ELADDR                                                        
         OC    TRSTUPDT,TRSTUPDT                                                
         BNZ   CANTCHA           CAN'T CHANGED OR DELETE IF APPROVED            
         B     XIT                                                              
         EJECT ,                                                                
*              UPDATE ACN NUMBER ON MEDIA/VEHICLE FILE                          
         SPACE 1                                                                
UPACN    NTR1                                                                   
         L     R4,AIO              RECORD ADDED OR CHANGED                      
         MVC   ACN,ACKEYACC+3      SAVE ACN NUMBER FROM SE RECORD               
         MVC   AIO,AIO2                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'3M'                                                  
         MVC   KEY+3(10),ACKEYCON+5   MEDIA/VEHICLE                             
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1            RESET IO AREA                                
         L     R3,AIO2                                                          
         CLC   KEYSAVE(42),0(R3)                                                
         BE    *+6                                                              
         DC    H'0'                CAN'T FIND MEDIA VEHICLE RECORD              
*                                                                               
         GOTO1 GETL,DMCB,(X'23',AIO2),0                                         
         CLI   ELERR,0                                                          
         BE    XIT                 IF ON FILE DON'T UPDATE                      
         LA    R3,EXPELM                                                        
*                                                                               
         USING ACOTHERD,R3                                                      
*                                                                               
         XC    EXPELM,EXPELM       ADD ACN NUMBER                               
         MVC   ACOTEL(2),=X'230F'  TO MEDIA VEHICLE RECORD                      
         MVC   ACOTNUM(13),SPACES                                               
         MVC   ACOTNUM(5),ACN                                                   
         GOTO1 ADDL,DMCB,AIO2,EXPELM                                            
         MVC   AIO,AIO2                                                         
         GOTO1 WRITE               WRITE MEDIA VEHICLE RECORD                   
         MVC   AIO,AIO1            RESET IO AREA                                
         B     XIT                                                              
         EJECT ,                                                                
*              UPDATE SUBSIDIARY CASH BUCKETS GROSS AND NET                     
         SPACE 1                                                                
UPSUB    NTR1  ,                                                                
         LA    RE,TRNBLK                                                        
         LA    RF,TRNBLKL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,ATIA                                                          
         L     RF,=A(14*1024)      CLEAR THE TIA                                
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   TRNCOMF,ACOMFACS                                                 
         MVC   TRNREC,AIO                                                       
         L     RE,ATIA             USE THE TIA FOR BUFFERS                      
         STCM  RE,15,TRNACC                                                     
         LA    RE,2048(,RE)                                                     
         STCM  RE,15,TRNBUK                                                     
         LA    RE,2048(,RE)                                                     
         STCM  RE,15,TRNCAC                                                     
         LA    RE,2048(,RE)                                                     
         STCM  RE,15,TRNOFA                                                     
         LA    RE,2048(,RE)                                                     
         ST    RE,ALEDGER                                                       
         LA    RE,LEDGTAB                                                       
         ST    RE,TRNLDG                                                        
         MVI   TRN#LDGS,1          PASS NUMBER OF LEDGER TABLE ENTRIES          
*                                                                               
         USING CPYELD,R3                                                        
*                                                                               
         LA    R3,COMPEL                                                        
         MVC   TRNCPYS1,CPYSTAT1                                                
         MVC   TRNCPYS2,CPYSTAT2                                                
         MVC   TRNCPYS3,CPYSTAT3                                                
         MVC   TRNCPYS4,CPYSTAT4                                                
         CLI   CPYLN,CPYLN2Q       TEST LONG ELEMENT                            
         BL    UPSUB02                                                          
         MVC   TRNCPYS5,CPYSTAT5                                                
         MVC   TRNCPYS6,CPYSTAT6                                                
         MVC   TRNCPYS7,CPYSTAT7                                                
         MVC   TRNCPYS8,CPYSTAT8                                                
         CLI   CPYLN,CPYLN3Q                                                    
         BL    UPSUB02                                                          
         MVC   TRNCPYS9,CPYSTAT9                                                
         MVC   TRNCPYSA,CPYSTATA                                                
                                                                                
UPSUB02  LA    R0,PALAREA                                                       
         STCM  R0,15,TRNPAL                                                     
                                                                                
         MVC   TRNBMOS,ADVDTE      YYMM PACKED                                  
         MVC   TRNPUSER,TWAORIG                                                 
         MVC   TRNCACNM,LOGPRDN    C/A NAME=PRODUCT NAME                        
         OC    TRNCACNM,SPACES                                                  
*                                                                               
         BAS   RE,GETLED           GET LEDGER TABLE ENTRY FOR SE                
*                                                                               
         MVI   TRNINDS1,TRNIDNAT+TRNIDNAD+TRNIDNMR                              
         GOTO1 ADDTRN,TRNBLK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACKEYD,R4                                                        
*                                                                               
         L     R4,TRNREC                                                        
         MVC   ACKEYCON+7(8),SPACES                                             
         OI    TRNINDS1,TRNIDNUB    SKIP ACCT BALANCE UPDATE                    
         GOTO1 GETL,DMCB,('SCIELQ',(R4)),0                                      
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING SCIELD,R3                                                        
*                                                                               
         L     R3,ELADDR                                                        
         MVI   SCITYPE,SCITCOKE    FUDGE TYPE TO ADD 'E' BUCKET                 
         GOTO1 ADDTRN,TRNBLK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   SCITYPE,SCITGRNT    RESTORE TYPE 'E'                             
*                                                                               
         MVI   TRNINDS,TRNILAST                                                 
         GOTO1 ADDTRN,TRNBLK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   ACKEYCON+7(8),VEHICLE                                            
         B     XIT                                                              
*                                                                               
         DROP  R3                                                               
         EJECT ,                                                                
*              SUB-ROUTINE TO GET A LEDGER RECORD AND BUILD A LEDGER            
*              TABLE ENTRY                                                      
         SPACE 1                                                                
         PUSH  USING                                                            
         SPACE 1                                                                
GETLED   NTR1  ,                                                                
         MVC   SVKEY,KEY           SAVE THE KEY                                 
         L     R3,TRNREC           R5=A(TRANSACTION RECORD)                     
         LA    R5,LEDGTAB          R6=A(LEDGER TABLE ENTRY)                     
         LA    R4,KEY                                                           
         MVC   KEY(42),SPACES                                                   
         MVC   ACKEYACC(3),0(R3)   GET COMPANY/UNIT/LEDGER                      
         MVC   AIO,ALEDGER                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   ACKEYACC,KEYSAVE                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         MVC   AIO,AIO1            AND IO AREA POINTER                          
         L     R4,ALEDGER          R4=A(LEDGER RECORD)                          
*                                                                               
         USING LDGTABD,R5                                                       
*                                                                               
GETLED2  XC    LDGTABD(LDGTABL),LDGTABD                                         
         MVC   LDGTUL,ACKEYACC+1                                                
         LA    R3,ACRECORD                                                      
         SR    R0,R0                                                            
*                                                                               
GETLED4  CLI   0(R3),0             TEST FOR EOR                                 
         BE    GETLEDX             YES                                          
         CLI   0(R3),ACLTELQ       TEST FOR LEDGER ELEMENT                      
         BNE   GETLED5             NO                                           
*                                                                               
         USING ACLEDGD,R3                                                       
*                                                                               
         MVC   LDGTTYPE,ACLTTYPE                                                
         MVC   LDGTLIKE,ACLTLIKE                                                
         MVC   LDGTOFFP,ACLTOFF                                                 
         B     GETLED8                                                          
*                                                                               
GETLED5  CLI   0(R3),ACHRELQ                                                    
         BNE   GETLED6                                                          
*                                                                               
         USING ACHEIRD,R3                                                       
*                                                                               
         MVC   LDGTLVA,ACHRLEVA                                                 
         MVC   LDGTLVB,ACHRLEVB                                                 
         MVC   LDGTLVC,ACHRLEVC                                                 
         MVC   LDGTLVD,ACHRLEVD                                                 
         B     GETLED8                                                          
*                                                                               
GETLED6  CLI   0(R3),ACSTELQ                                                    
         BNE   GETLED8                                                          
*                                                                               
         USING ACSTATD,R3                                                       
*                                                                               
         MVC   LDGTSEC,ACSTSECY+1                                               
*                                                                               
GETLED8  IC    R0,1(,R3)                                                        
         AR    R3,R0                                                            
         B     GETLED4                                                          
*                                                                               
GETLEDX  B     XIT                                                              
         POP   USING                                                            
         EJECT ,                                                                
*              CHANGE SUBSIDIARY CASH BUCKETS                                   
         SPACE 1                                                                
CHASUB   NTR1                                                                   
*                                                                               
*              GET CURRENT 50 ELEMENT                                           
*                                                                               
         GOTO1 GETL,DMCB,(X'50',AIO),0                                          
         L     R3,ELADDR                                                        
*                                                                               
         USING TRCASHD,R3                                                       
*                                                                               
         CLI   MODE,RECDEL                                                      
         BNE   CHASUB1                                                          
         ZAP   GROSS,TRCSGRS                                                    
         ZAP   NET,TRCSNET                                                      
         ZAP   TRCSGRS,=P'0'       IF DELETE ZERO CURRENT BUCKETS               
         ZAP   TRCSNET,=P'0'                                                    
*                                                                               
*              REDUCE CURRENT BY AMOUNT PREVIOUSLY IN BUCKETS                   
*                                                                               
CHASUB1  LA    RF,LASTSUB          50 BEFORE CHANGE                             
         SP    TRCSGRS,TRCSGRS-TRCASHD(L'TRCSGRS,RF)                            
         SP    TRCSNET,TRCSNET-TRCASHD(L'TRCSNET,RF)                            
         BAS   RE,UPSUB      UPDATE BUCKITS                                     
         ZAP   TRCSGRS,GROSS     PUT AMOUNTS IN TRANSACTION                     
         ZAP   TRCSNET,NET       BEFORE ADD OR DELETE                           
         B     XIT                                                              
         EJECT ,                                                                
         SPACE 1                                                                
*                                                                               
CANTCHA  MVC   CONHEAD(L'MSG2),MSG2                                             
         LA    R2,LOGGAMTH                                                      
         B     MYEND                                                            
*                                                                               
WARNING  MVC   CONHEAD(L'MSG3),MSG3                                             
         LA    R2,LOGGAMTH                                                      
         B     MYEND                                                            
*                                                                               
MYEND    MVI   ERROR,X'FE'                                                      
         B     THEEND                                                           
         EJECT ,                                                                
         SPACE 1                                                                
ERR03    MVI   ERROR,3                                                          
         B     ACMESG                                                           
*                                                                               
ERR13    MVI   ERROR,13                                                         
         B     ACMESG                                                           
*                                                                               
ERR17    MVI   ERROR,ACCINVAL                                                   
         B     ACMESG                                                           
*                                                                               
ERR18    MVI   ERROR,18                                                         
         B     ACMESG                                                           
*                                                                               
ERR56    MVI   ERROR,56            ACCOUNT IS LOCKED                            
         LA    R2,LOGAGYH                                                       
         B     ACMESG                                                           
*                                                                               
ERR61    MVI   ERROR,NOHIGHER                                                   
         B     ACMESG                                                           
*                                                                               
ERR62    MVI   ERROR,ONETO255                                                   
         B     ACMESG                                                           
*                                                                               
ERR63    MVI   ERROR,ACTOOLNG                                                   
         B     ACMESG                                                           
*                                                                               
FLDINV   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
*                                                                               
         USING GETTXTD,RE                                                       
NOACCT   LA    RE,GETTXTCB                                                      
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMTYP,GTMERR       C'E' ERROR TYPE                              
         LA    RF,KEYSAVE+1                                                     
         STCM  RF,7,GTATXT         ADDRESS OF TEXT TO REPLACE &T                
         MVI   GTLTXT,L'ACTKULA    LENGTH OF TEXT TO REPLACE &T                 
         LHI   RF,AE$SINAC                                                      
         STCM  RF,3,GTMSGNO        MESSAGE NUMBER                               
         MVI   GTMSYS,6            USE ACCOUNT MESSAGES (SYST 6)                
         DROP  RE                                                               
         OI    GENSTAT2,USGETTXT   X'10' USE GETTXT INSTEAD OF GETMSG           
         B     THEEND                                                           
*                                                                               
ACMESG   MVI   GETMSYS,6           ACCOUNT MESSAGES (SYST 6)                    
         OI    GENSTAT2,USMYERSY                                                
*                                                                               
THEEND   GOTO1 EXIT                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT ,                                                                
*              CONSTANTS                                                        
         SPACE 1                                                                
MSG2     DC    C'**ERROR** CAN''T CHANGE OR DELETE APPROVED ITEMS'              
MSG3     DC    C'*WARNING* CAN''T CHANGE OR DELETE APPROVED ITEMS'              
         EJECT ,                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
*              TABLES                                                           
         SPACE 1                                                                
*              SPOT MONTH                                                       
*              BUDGET START MONTH                                               
*              BUDGET END MONTH                                                 
*              QUARTER NUMBER                                                   
BUDPER   DC    X'01',X'0103',X'01'                                              
         DC    X'02',X'0103',X'01'                                              
         DC    X'03',X'0103',X'01'                                              
         DC    X'04',X'0406',X'02'                                              
         DC    X'05',X'0406',X'02'                                              
         DC    X'06',X'0406',X'02'                                              
         DC    X'07',X'0709',X'03'                                              
         DC    X'08',X'0709',X'03'                                              
         DC    X'09',X'0709',X'03'                                              
         DC    X'10',X'1012',X'04'                                              
         DC    X'11',X'1012',X'04'                                              
         DC    X'12',X'1012',X'04'                                              
         DC    X'FF'                                                            
         EJECT ,                                                                
*              DSECT FOR THIS MODULE                                            
         SPACE 1                                                                
LWSD     DSECT                                                                  
RELO     DS    A                                                                
ALEDGER  DS    A                                                                
*                                                                               
ACN      DS    CL5                                                              
AGYCDE   DS    CL3                                                              
PRD      DS    CL2                                                              
MEDIA    DS    CL2                                                              
VEHICLE  DS    CL8                                                              
ADVDTE   DS    CL3                                                              
INVOICE  DS    CL6                                                              
GROSS    DS    PL6                                                              
NET      DS    PL6                                                              
NETPCT   DS    PL3                                                              
CDPCT    DS    PL2                                                              
CSHD     DS    PL6                                                              
PWRK     DS    CL13                                                             
YEAR     DS    CL1                                                              
YTD      DS    PL6                                                              
BUD      DS    PL6                                                              
BUDST    DS    CL2                                                              
BUDEN    DS    CL2                                                              
PRDBUDC  DS    CL2                                                              
QTRNUM   DS    CL1                                                              
SVKEY    DS    CL42                                                             
SVSTAT   DS    CL1                 ACCOUNT STATUS                               
*                                                                               
LASTSUB  DS    CL20                KEEP LAST 50 ELEMENT                         
*                                                                               
LEDGTAB  DS    XL(LDGTABL+1)                                                    
*                                                                               
         SPACE 1                                                                
       ++INCLUDE ACADDTRND                                                      
         SPACE 1                                                                
LWSEND   EQU   *                                                                
         EJECT ,                                                                
       ++INCLUDE ACEXPWORKD                                                     
         EJECT ,                                                                
       ++INCLUDE ACEXPFFD                                                       
         EJECT ,                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE ACEXPFCD                                                       
         EJECT ,                                                                
       ++INCLUDE DDGENTWA                                                       
         EJECT ,                                                                
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACLDGTABD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACLDGTABD                                                      
         PRINT ON                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*FAGETTXTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
*ACMSGEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACMSGEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060ACEXP03   01/27/04'                                      
         END                                                                    
