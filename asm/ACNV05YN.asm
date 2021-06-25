*          DATA SET ACNV05YN   AT LEVEL 034 AS OF 11/06/00                      
*PHASE ACNV5YNA ACNV05YN                                                        
         TITLE 'ACCPAK CONVERSION - YNRO  HOOK'                                 
ACNV05YN CSECT                                                                  
         PRINT NOGEN                                                            
BGN      DS    0D                                                               
         NMOD1 0,*YNHK*,R8                                                      
         USING ACNVD,R9              STORAGE COMMON WITH BASE                   
         L     R2,AOUT                                                          
         USING ACTRECD,R2                                                       
         MVI   COMPSTA,COMPOTON      SET CONVERT OLD TO NEW                     
         MVC   NEW1RLVS,=X'0204060C' SET NEW 1R LEVELS                          
*                                                                               
         CLI   MODE,CHNGACC          TEST CHANGE ACCOUNT                        
         BNE   INPMODE                                                          
         B     CHNGAC                                                           
*                                                                               
XNO      LTR   RB,RB        DDD                                                 
         B     XIT                                                              
XYES     CR    RB,RB                                                            
XIT      XIT1                                                                   
*                                                                               
INPMODE  MVC   BALOFF,DFLTOFF      BALANCE OFFICE IS DEFAULT OFFICE             
         EJECT                                                                  
***********************************************************************         
* GET ROUTINE BASED ON UNIT/ LEDGER                                   *         
***********************************************************************         
                                                                                
ULCHK    CLC   ACTKUNT(2),LASTUL   TEST SAME AS UNIT/LEDGER                     
         BNE   ULCHK3                                                           
         LA    RE,LASTUL           USE SAME ROUTINE                             
         B     ULCHK9                                                           
*                                                                               
ULCHK3   XC    LASTUL,LASTUL       CLEAR ADDRESS                                
         MVC   LASTUL(2),ACTKUNT   BUT SAVE UNIT LEDGER                         
         LA    RE,ULTAB            FIND NEW ROUTINE                             
*                                                                               
ULCHK5   CLC   ACTKUNT(2),0(RE)    MATCH UNIT/LEDGER                            
         BE    ULCHK7                                                           
         LA    RE,L'ULTAB(RE)                                                   
         CLI   0(RE),EOT                                                        
         BNE   ULCHK5                                                           
         B     XIT                                                              
*                                                                               
ULCHK7   MVC   LASTUL,0(RE)        SAVE FOR NEXT TIME                           
*                                                                               
ULCHK9   ICM   RF,15,4(RE)         SPECIAL ROUTINE BY U/L                       
         BZ    XIT                 NO ROUTINE FOR THIS U/L                      
         BR    RF                                                               
         EJECT                                                                  
***********************************************************************         
* LEDGER CONTROL TABLE                                                *         
***********************************************************************         
                                                                                
ULTAB    DS    0D                                                               
         DC    C'SB',AL2(0),AL4(ROUT1)                                          
         DC    C'SC',AL2(0),AL4(ROUT1)                                          
         DC    C'SI',AL2(0),AL4(ROUT1)                                          
         DC    C'SJ',AL2(0),AL4(ROUT1)                                          
         DC    C'SK',AL2(0),AL4(ROUT1)                                          
         DC    C'SP',AL2(0),AL4(ROUT1)                                          
         DC    C'SQ',AL2(0),AL4(ROUT1)                                          
         DC    C'SS',AL2(0),AL4(ROUT1)                                          
         DC    C'ST',AL2(0),AL4(ROUT1)                                          
         DC    C'SU',AL2(0),AL4(ROUT1)                                          
         DC    C'SV',AL2(0),AL4(ROUT1)                                          
         DC    C'SZ',AL2(0),AL4(ROUT1)                                          
         DC    C'S9',AL2(0),AL4(ROUT1)                                          
         DC    C'1N',AL2(0),AL4(ROUT1)                                          
         DC    C'11',AL2(0),AL4(ROUT1)                                          
         DC    C'12',AL2(0),AL4(ROUT1)                                          
         DC    C'2C',AL2(0),AL4(ROUT1)                                          
         DC    C'27',AL2(0),AL4(ROUT1)                                          
*                                                                               
         DC    C'SR',AL2(0),AL4(ROUT2)                                          
         DC    C'1C',AL2(0),AL4(ROUT2)                                          
         DC    C'1R',AL2(0),AL4(ROUT2)                                          
         DC    C'14',AL2(0),AL4(ROUT2)                                          
         DC    C'2D',AL2(0),AL4(ROUT2)                                          
         DC    C'29',AL2(0),AL4(ROUT2)                                          
*                                                                               
         DC    AL1(EOT)                                                         
*                                                                               
LASTUL   DC    XL(L'ULTAB)'00'                                                  
         EJECT                                                                  
***********************************************************************         
* ROUT1   NO SPECIAL CHANGES                                          *         
***********************************************************************         
                                                                                
ROUT1    CLI   MODE,PROCAFT        PROCESS INPUT AFTER CONVERSION               
         BNE   R1OUT                                                            
         BAS   RE,UPBAL            SET BBF BY OFFICE                            
         B     XIT                                                              
*                                                                               
R1OUT    CLI   MODE,PROCOUT        PROCESS FINAL OUTPUT                         
         BNE   R1MRG                                                            
         B     XIT                                                              
*                                                                               
R1MRG    CLI   MODE,MRGEPRO        MERGE PROFILE                                
         BNE   XIT                                                              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUT2  FIX LEDGER RECORD                                            *         
***********************************************************************         
                                                                                
ROUT2    CLI   MODE,PROCAFT        PROCESS INPUT AFTER CONVERSION               
         BNE   R2OUT                                                            
         BAS   RE,UPBAL            SET BBF BY OFFICE                            
         BAS   RE,OFFACC           FIX OFFICE IN HIGH LEVEL ACCOUNT             
         BAS   RE,LDGUP            FIX LEDGER RECORDS                           
         B     XIT                                                              
*                                                                               
R2OUT    CLI   MODE,PROCOUT        PROCESS FINAL OUTPUT                         
         BNE   R2MRG                                                            
         B     XIT                                                              
*                                                                               
R2MRG    CLI   MODE,MRGEPRO        MERGE PROFILE                                
         BNE   XIT                                                              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* UPDATE BALANCE ENTRY IN OFFICE ACCOUNT TABLE                        *         
***********************************************************************         
                                                                                
UPBAL    CLI   RECTYP,ACRTACTL                                                  
         BNER  RE                                                               
UPBAL0   NTR1  ,                                                                
         L     R2,AOUT             GET BALANCE ELEMENT                          
         LA    R4,ACTRFST                                                       
         SR    R0,R0                                                            
UPBAL3   CLI   0(R4),0             EOR?                                         
         BE    XIT                 JUST LEAVE IF NO BALANCE ELEMENT             
         USING ABLELD,R4                                                        
         CLI   0(R4),ABLELQ                                                     
         BE    UPBAL5                                                           
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     UPBAL3                                                           
*                                                                               
UPBAL5   CP    ABLFRWD,=P'0'        TEST ANY BALANCE                            
         BE    XIT                                                              
         USING OFATABD,R5                                                       
         L     R5,AOFATAB           SEE IF ENTRY EXISTS IF OFF\ACCT             
*                                                                               
UPBAL7   CLI   0(R5),OFATEOTQ       TABLE FOR THIS OFFICE                       
         BE    UPBAL9                                                           
         CLC   OFATOFFC,BALOFF      OFFICE FOR BALANCE FORWARD                  
         BE    UPBAL11                                                          
         LA    R5,OFATABLN(R5)                                                  
         B     UPBAL7                                                           
*                                                                               
UPBAL9   MVC   OFATOFFC,BALOFF      ADD NEW ENTRY                               
         ZAP   OFATBALF,=P'0'       INITIALIZE BBF                              
         ZAP   OFATTOTD,=P'0'                                                   
         ZAP   OFATTOTC,=P'0'                                                   
         MVC   OFATLMOS,EFFS                                                    
         XC    OFATHMOS,OFATHMOS                                                
         MVI   OFATABLN(R5),X'FF'   MARK NEW END OF TABLE                       
*                                                                               
UPBAL11  AP    OFATBALF,ABLFRWD     ACCUMULATE BBF                              
         B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS LEDGER RECORD                                               *         
***********************************************************************         
                                                                                
         USING LDGRECD,R3                                                       
LDGUP    CLI   RECTYP,ACRTLDG                                                   
         BNER  RE                                                               
LDGUP1   NTR1  ,                                                                
         LA    R4,LDGTAB                                                        
         L     R3,AOUT                                                          
         USING LDGRECD,R3                                                       
LDGUP3   CLC   LDGKUNT(2),0(R4)                                                 
         BE    LDGUP5                                                           
         LA    R4,L'LDGTAB(R4)                                                  
         CLI   0(R4),X'FF'                                                      
         BNE   LDGUP3                                                           
         B     XIT                                                              
*                                                                               
LDGUP5   SR    R1,R1                                                            
         LA    R3,LDGRFST          GET TO FIRST ELEMENT                         
LDGUP7   CLI   0(R3),0                                                          
         BE    XIT                                                              
         CLI   0(R3),ACLELQ        ACCOUNT LENGTHS ELEMENT                      
         BE    LDGUP11                                                          
         CLI   0(R3),LDGELQ        LEDGER ELEMENT                               
         BE    LDGUP13                                                          
LDGUP9   IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     LDGUP7                                                           
*                                                                               
         USING ACLELD,R3                                                        
LDGUP11  ICM   RF,15,4(R4)         RF=A(NEW LENGTHS ELEMENT)                    
         BZ    LDGUP9                                                           
         MVC   ACLVALS(L'ACLVALS*4),0(RF)                                       
         B     LDGUP9                                                           
*                                                                               
         USING LDGELD,R3                                                        
LDGUP13  CLI   2(R4),0             TEST NEW OFFPOS                              
         BE    LDGUP9                                                           
         MVC   LDGOPOS,2(R4)                                                    
         B     LDGUP9                                                           
         DROP  R3                                                               
*                                                                               
OFFPP2   EQU   LDGOKEY2+2          OFFPOS=+2                                    
OFFPP1   EQU   LDGOKEY2+1          OFFPOS=+1                                    
*                                                                               
         DS    0F                                                               
LDGTAB   DS    0XL8                                                             
         DC    C'SR',AL1(OFFPP2),AL1(0),AL4(0)                                  
         DC    C'1C',AL1(OFFPP1),AL1(0),AL4(LVL1C)                              
         DC    C'1R',AL1(OFFPP1),AL1(0),AL4(LVL1R)                              
         DC    C'2D',AL1(OFFPP1),AL1(0),AL4(LVL2D)                              
         DC    C'29',AL1(OFFPP1),AL1(0),AL4(LVL29)                              
         DC    X'FF'                                                            
*                                                                               
LVL1C    DC    AL1(02),CL15'OFFICE'                                             
         DC    AL1(05),CL15'CLIENT'                                             
         DC    AL1(08),CL15'DIVISION'                                           
         DC    AL1(12),CL15'PRODUCT'                                            
*                                                                               
LVL1R    DC    AL1(02),CL15'OFFICE'                                             
         DC    AL1(04),CL15'DEPARTMENT'                                         
         DC    AL1(06),CL15'SUB-DEPARTMENT'                                     
         DC    AL1(12),CL15'PERSON'                                             
*                                                                               
LVL2D    DC    AL1(02),CL15'OFFICE'                                             
         DC    AL1(12),CL15'DEPARTMENT'                                         
         DC    AL1(0),CL15' '                                                   
         DC    AL1(0),CL15' '                                                   
*                                                                               
LVL29    DC    AL1(02),CL15'OFFICE'                                             
         DC    AL1(05),CL15'CLIENT'                                             
         DC    AL1(08),CL15'DIVISION'                                           
         DC    AL1(12),CL15'PRODUCT'                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* CHANGE ACCOUNT CODE                                                 *         
***********************************************************************         
                                                                                
CHNGAC   MVC   OACT,SRCHARG        SET OLD ACCOUNT                              
         CLC   ACTKULA,=CL14'SRX03LM 0'                                         
         BNE   *+14                                                             
         MVC   NACT,=CL14'SRXCALM 0'                                            
         B     CHNGAC3                                                          
*                                                                               
         CLC   ACTKULA,=CL14'SRX0BLM 0'                                         
         BNE   *+14                                                             
         MVC   NACT,=CL14'SRXD3LM 0'                                            
         B     CHNGAC3                                                          
*                                                                               
         CLC   ACTKULA,=CL14'SRC03LM 0'                                         
         BNE   *+14                                                             
         MVC   NACT,=CL14'SRCCALM 0'                                            
         B     CHNGAC3                                                          
*                                                                               
         CLC   ACTKULA,=CL14'SRC0BLM 0'                                         
         BNE   *+14                                                             
         MVC   NACT,=CL14'SRCD3LM 0'                                            
         B     CHNGAC3                                                          
*                                                                               
         BAS   RE,REPOFC           REPLACE OFFICE                               
         CLC   NACT,SPACE          TEST ACCOUNT CHANGED                         
         BE    XNO                                                              
CHNGAC3  L     R4,HKNEWA                                                        
         MVC   0(14,R4),NACT                                                    
         B     XYES                ACCOUNT CHANGED                              
         EJECT                                                                  
***********************************************************************         
* CHANGE OFFICE IN KEY FOR ACCOUNT HIGH                               *         
***********************************************************************         
                                                                                
OFFACC   CLI   RECTYP,ACRTACTH                                                  
         BNER  RE                                                               
*                                                                               
OFFACC3  MVC   OACT,ACTKUNT        SET OLD ACCOUNT                              
         LR    R0,RE                                                            
         BAS   RE,REPOFC                                                        
         LR    RE,R0                                                            
         CLC   NACT,SPACE          TEST ACCOUNT CHANGED                         
         BER   RE                                                               
         MVC   ACTKUNT(14),NACT                                                 
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* REPLACE EMBEDDED OFFICE IN ACCOUNT FIELD                            *         
***********************************************************************         
                                                                                
REPOFC   NTR1  ,                                                                
         MVC   NACT,SPACE                                                       
*                                                                               
         CLC   OACT(2),=C'SR'      FOR SR                                       
         BNE   REPOFC3                                                          
*                                                                               
         LA    RF,ACTKACT                                                       
         CLC   ACTKUNT(2),=C'SJ'   FOR SJ USE CLIENT CODE                       
         BE    REPOFC1                                                          
*                                                                               
         LA    RF,INTKCLT-INTRECD(R2)   INTERAGENCY                             
         CLI   RECTYP,ACRTINT                                                   
         BE    REPOFC1                                                          
*                                                                               
         LA    RF,MPDKCLI-MPDRECD(R2)    MEDIA DETAIL                           
         CLI   RECTYP,ACRTMPD                                                   
         BE    REPOFC1                                                          
*                                                                               
         LA    RF,OACT+5           CLIENT IN ACCOUNT                            
*                                                                               
REPOFC1  CLI   0(RF),C' '         TEST ANY CLIENT CODE                          
         BNH   REPOFC3                                                          
         BAS   RE,COFC                                                          
         CLC   CLNOF,SPACE                                                      
         BE    REPOFC3                                                          
         MVC   NACT,OACT                                                        
         MVC   NACT+3(2),CLNOF     USE NEW OFFICE                               
         B     XIT                                                              
*                                                                               
REPOFC3  LA    RE,OFFPTAB           CONVERT OFFICE IN ACCOUNT                   
REPOFC5  CLI   0(RE),EOT                                                        
         BE    XIT                                                              
         CLC   OACT(2),0(RE)        MATCH UNIT/LEDGER                           
         BE    REPOFC7                                                          
         LA    RE,L'OFFPTAB(RE)                                                 
         B     REPOFC5                                                          
*                                                                               
REPOFC7  SR    RF,RF                                                            
         IC    RF,2(RE)            RF=OFFPOS                                    
         BCTR  RF,0                                                             
         LA    R5,OACT+2(RF)       R5=A(OLD OFFICE)                             
         CLI   0(R5),C' '          TEST HIGH LEVEL                              
         BE    XIT                                                              
*                                                                               
**                                 FOR YNR                                      
         CLC   OACT(2),=C'SR'      SR LEDGER - HAS 2 BYTES ALREADY              
         BE    *+14                            RESERVED. DON'T SHIFT            
**                                                                              
         CLI   OACT+13,C' '        TEST ACCOUNT TOO LONG                        
         BE    *+6                                                              
         DC    H'0'                LAST MUST BE BLANK                           
         MVC   NACT(2),OACT        U/L                                          
         LA    R5,OACT+2           R5=FROM ACCOUNT                              
         LA    R4,NACT+2           R4=TO ACCOUNT                                
         SR    R1,R1                                                            
         IC    R1,2(RE)            R1=OFFPOS                                    
         SH    R1,=H'2'                                                         
         BM    *+14                OFFPOS=1                                     
         EX    R1,*+4                                                           
         MVC   0(0,R4),0(R5)       FIRST PART OF KEY(UP TO OFFICE)              
         LA    R4,3(R1,R4)         R4=A(AREA AFTER OFFICE)                      
**                                 FOR YNR                                      
         CLC   NACT(2),=C'SR'      SR LEDGER - HAS 2 BYTES ALREADY              
         BNE   *+6                             RESERVED. DON'T SHIFT            
         BCTR  R4,0                                                             
**                                                                              
         LA    R5,2(R1,R5)         R5=A(REMAINING DATA)                         
         LA    RF,L'ACTKACT-1                                                   
         SR    RF,R1               RF=LENGTH TO BE MOVED                        
         EX    RF,*+4                                                           
         MVC   0(0,R4),0(R5)       MOVE NEXT PART OF KEY (AFTER OFFICE)         
*                                                                               
         IC    RF,2(RE)            RF=OFFPOS                                    
         BCTR  RF,0                                                             
         LA    R4,NACT+2(RF)       R4=A(NEW OFFICE)                             
         LA    R5,OACT+2(RF)       R5=A(OLD OFFICE)                             
**                                 FOR YNR                                      
         CLC   NACT(2),=C'SR'      SR LEDGER - HAS 2 BYTES ALREADY              
         BNE   *+8                             USE SECOND                       
         LA    R5,1(R5)                                                         
**                                                                              
         ICM   R0,15,OFFNUM                                                     
         ICM   R1,15,OFFTAB                                                     
*                                                                               
REPOFC11 CLC   0(1,R5),0(R1)        OLD OFFICE CODE                             
         BE    REPOFC13                                                         
         LA    R1,OFFLNQ(R1)                                                    
         BCT   R0,REPOFC11                                                      
*                                                                               
         CLI   RECTYP,ACRTNBT      SKIP NOT FOUND IN BATCH RECORDS              
         BE    XIT                                                              
         DC    H'0'                                                             
*                                                                               
REPOFC13 MVC   0(2,R4),1(R1)       SET NEW OFFICE                               
         B     XIT                                                              
*                                                                               
OFFPTAB  DS    0XL3                                                             
         DC    C'SR',AL1(2)                                                     
         DC    C'1R',AL1(1)                                                     
         DC    C'14',AL1(2)                                                     
         DC    C'2D',AL1(1)                                                     
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* GET OFFICE CODE FROM CLIENT OFFICE LIST                             *         
*  RF=A(CLIENT CODE)                                                  *         
***********************************************************************         
                                                                                
COFC     NTR1  ,                                                                
         MVC   CLNOF,SPACE                                                      
         ICM   R0,15,CLONUM        R0=NMBER IN CLIENT/OFFICE TABLE              
         ICM   R1,15,CLOTAB        R1=A(CLIENT/OFFICE TABLE)                    
*                                                                               
COFC3    CLC   0(3,RF),0(R1)       MATCH CODE TO TABLE                          
         BE    COFC5                                                            
         LA    R1,CLOLNQ(R1)                                                    
         BCT   R0,COFC3                                                         
         B     XIT                                                              
*                                                                               
COFC5    MVC   CLNOF,3(R1)         SAVE NEW OFFICE                              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DELETE LEDGER OR ACCOUNT HIGH FROM OLD                              *         
***********************************************************************         
                                                                                
DELLG    CLI   RECTYP,ACRTLDG      LEDGER                                       
         BNER  RE                                                               
         OI    HKSTA,HKSDEL        DELETE                                       
         B     XIT                                                              
*                                                                               
DELHI    CLI   RECTYP,ACRTACTH     ACCOUNT HIGH                                 
         BNER  RE                                                               
         OI    HKSTA,HKSDEL        DELETE                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DELETE GENERAL LEDGER POINTERS FROM OLD                             *         
***********************************************************************         
                                                                                
DELPTR   CLI   RECTYP,ACRTACTH     HAS TO BE ACCT HIGH OR LOW                   
         BE    *+10                                                             
         CLI   RECTYP,ACRTACTL                                                  
         BNER  RE                                                               
DELPTR1  NTR1  ,                                                                
         L     R2,AOUT             INPUT FROM "NEW" LINKED ACCOUNTS             
         LA    R5,ACTRFST                                                       
         SR    R0,R0                                                            
DELPTR3  CLI   0(R5),0             NO NEW POINTER                               
         BE    XIT                                                              
         CLI   0(R5),GLPELQ        GET NEW G\L POINTER                          
         BE    DELPTR5                                                          
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     DELPTR3                                                          
*                                                                               
DELPTR5  MVI   0(R5),X'FF'         DELETE OLD POINTER                           
         GOTO1 HELLO,DMCB,(C'D',ACCMST),(X'FF',AOUT),0,0                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* UPDATE GENERAL LEDGER POINTERS FROM NEW                             *         
***********************************************************************         
                                                                                
GLPTR    CLI   RECTYP,ACRTACTH     HAS TO BE ACCT HIGH OR LOW                   
         BE    *+10                                                             
         CLI   RECTYP,ACRTACTL                                                  
         BNER  RE                                                               
GLPTR0   NTR1  ,                                                                
         L     R2,AINP             INPUT FROM "NEW" LINKED ACCOUNTS             
         LA    R5,ACTRFST                                                       
         SR    R0,R0                                                            
GLPTR3   CLI   0(R5),0             NO NEW POINTER                               
         BE    XIT                                                              
         CLI   0(R5),GLPELQ        GET NEW G\L POINTER                          
         BE    GLPTR5                                                           
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     GLPTR3                                                           
*                                                                               
GLPTR5   L     R2,AOUT             OUTPUT FROM CONVERTED FILE                   
         LA    R4,ACTRFST                                                       
GLPTR7   CLI   0(R4),0                                                          
         BE    GLPTR11                                                          
         CLI   0(R4),GLPELQ        GET OLD G\L POINTER                          
         BE    GLPTR9                                                           
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     GLPTR7                                                           
                                                                                
                                                                                
GLPTR9   MVI   0(R4),X'FF'         DELETE OLD POINTER                           
         GOTO1 HELLO,DMCB,(C'D',ACCMST),(X'FF',AOUT),0,0                        
GLPTR11  GOTO1 HELLO,DMCB,(C'P',ACCMST),AOUT,(R5) ADD NEW POINTER               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* UPDATE GENERAL LEDGER SPECIAL POINTERS FOR SB AND SE                *         
***********************************************************************         
                                                                                
GLSPT    CLI   RECTYP,ACRTACTL                                                  
         BNER  RE                                                               
GLSPT0   NTR1  ,                                                                
         L     R2,AOUT                                                          
         LA    R4,ACTRFST                                                       
         SR    R0,R0                                                            
GLSPT3   CLI   0(R4),0             EOR?                                         
         BE    GLSPT7              YES, CREATE GEN LDG POINTER                  
         CLI   0(R4),GLPELQ        FOUND GEN LDG POINTER                        
         BE    GLSPT5                                                           
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     GLSPT3                                                           
                                                                                
GLSPT5   MVI   0(R4),X'FF'         MARK ELEMENT FOR DELETION                    
         GOTO1 HELLO,DMCB,(C'D',ACCMST),(X'FF',AOUT),0,0                        
*                                                                               
                                                                                
GLSPT7   LA    R5,ELEMENT                                                       
         USING GLPELD,R5                                                        
         XC    ELEMENT,ELEMENT       SPACE OUT ELEMENT                          
         MVI   GLPEL,GLPELQ                                                     
         MVI   GLPLN,GLPLN1Q         SET MIN LENGTH                             
         MVC   GLPACC1,ACTKULA       MOVE IN CONVERTED ACCOUNT                  
         MVC   GLPACC1(2),=CL2'GB'   SB GOES TO GB                              
         CLC   ACTKUNT(2),=CL2'SB'                                              
         BE    GLSPT9                                                           
         MVC   GLPACC1(2),=CL2'GP'   SE GOES TO GP                              
         CLC   ACTKUNT(2),=CL2'SE'                                              
         BE    GLSPT9                                                           
         DC    H'0'                                                             
*                                                                               
GLSPT9   MVC   GLPSUB,GLPACC1                                                   
         GOTO1 HELLO,DMCB,(C'P',ACCMST),AOUT,(R5)                               
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* SPECIAL ANALYSIS POINTER FOR SI                                     *         
***********************************************************************         
                                                                                
SIANL    CLI   RECTYP,ACRTACTL                                                  
         BNER  RE                                                               
SIANL0   NTR1  ,                                                                
         L     R2,AOUT                                                          
         LA    R4,ACTRFST                                                       
         SR    R0,R0                                                            
SIANL3   CLI   0(R4),0             FIND CURRENT ANALYSIS POINTER                
         BE    SIANL9                                                           
         USING SPAELD,R4                                                        
         CLI   0(R4),SPAELQ                                                     
         BNE   SIANL5                                                           
         CLI   SPATYPE,SPATANAL                                                 
         BE    SIANL7                                                           
SIANL5   IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     SIANL3                                                           
                                                                                
SIANL7   MVI   0(R4),X'FF'                                                      
         GOTO1 HELLO,DMCB,(C'D',ACCMST),(X'FF',AOUT),0,0                        
                                                                                
SIANL9   MVC   SRCHARG(L'ACCO),ACTKULA      CONVERT OLD ACCOUNT                 
         GOTO1 ASRCH,SRCHEA                                                     
         BNE   XIT                                                              
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   SPAEL,SPAELQ                                                     
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATANAL                                                 
         MVC   SPAAANAL,ACTKACT                                                 
         GOTO1 HELLO,DMCB,(C'P',ACCMST),AOUT,(R4)                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SET LOCKED BIT                                                      *         
***********************************************************************         
                                                                                
         USING ACTRECD,R2                                                       
SETLCK   CLI   RECTYP,ACRTACTL                                                  
         BNER  RE                                                               
SETLCK0  NTR1  ,                                                                
         L     R2,AOUT                                                          
         LA    R4,ACTRFST                                                       
         SR    R0,R0                                                            
SETLCK3  CLI   0(R4),0             EOR                                          
         BE    SETLCK7             ADD STATUS ELEMENT                           
         USING RSTELD,R4                                                        
         CLI   0(R4),RSTELQ        TEST STATUS ELEMENT                          
         BE    SETLCK5                                                          
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     SETLCK3                                                          
*                                                                               
SETLCK5  OI    RSTSTAT1,RSTSACIL   SET ACCOUNT IS LOCKED                        
         B     XIT                                                              
*                                                                               
         USING RSTELD,R4                                                        
SETLCK7  LA    R4,ELEMENT          ADD NEW STATUS ELEMENT                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   RSTEL,RSTELQ                                                     
         MVI   RSTLN,RSTLN3Q                                                    
         MVI   RSTFILT4,C' '                                                    
         GOTO1 DATCON,DMCB,(5,0),(1,RSTBDATE)                                   
         MVC   RSTTDATE,RSTBDATE                                                
         MVI   RSTFILT3,C' '                                                    
         MVI   RSTFILT1,C' '                                                    
         MVI   RSTFILT2,C' '                                                    
         MVI   RSTCOSTG,C' '                                                    
         MVI   RSTFILT5,C' '                                                    
         OI    RSTSTAT1,RSTSACIL                                                
         GOTO1 HELLO,DMCB,(C'P',ACCMST),AOUT,(R4)                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* MERGE PROFILES FROM STATUS ELEMENTS                                 *         
***********************************************************************         
                                                                                
PRFMRG   CLI   RECTYP,ACRTACTH                                                  
         BE    *+10                                                             
         CLI   RECTYP,ACRTACTL                                                  
         BNER  RE                                                               
PRFMRG0  NTR1  ,                                                                
         L     R2,AINP                                                          
         LA    R5,ACTRFST                                                       
         SR    R0,R0                                                            
PRFMRG3  CLI   0(R5),0             TEST NEW STATUS ELEMENT                      
         BE    XIT                                                              
         CLI   0(R5),RSTELQ                                                     
         BE    PRFMRG5                                                          
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     PRFMRG3                                                          
*                                                                               
PRFMRG5  L     R2,AOUT                                                          
         LA    R4,ACTRFST                                                       
PRFMRG7  CLI   0(R4),0                                                          
         BE    PRFMRG9                                                          
         CLI   0(R4),RSTELQ        FIND OLD STATUS ELEMENT                      
         BE    PRFMRG11                                                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     PRFMRG7                                                          
*                                                                               
PRFMRG9  GOTO1 HELLO,DMCB,(C'P',ACCMST),AOUT,(R5) ADD STATUS TO OLD             
         B     XIT                                                              
*                                                                               
         USING RSTELD,R4                                                        
N        USING RSTELD,R5                                                        
PRFMRG11 NI    RSTSTAT1,X'FF'-RSTSGPEI  TURNOFF STAFF=Y                         
         NI    RSTSTAT1,X'FF'-RSTSEADD          DEPT=Y                          
*                                                                               
         TM    N.RSTSTAT1,RSTSGPEI                                              
         BZ    *+8                                                              
         OI    RSTSTAT1,RSTSGPEI   SET STAFF=Y                                  
*                                                                               
         TM    N.RSTSTAT1,RSTSEADD                                              
         BZ    *+8                                                              
         OI    RSTSTAT1,RSTSEADD   SET DEPT=Y                                   
         B     XIT                                                              
         DROP  N,R4                                                             
         EJECT                                                                  
***********************************************************************         
* DELETE VEND2C                                                                 
***********************************************************************         
                                                                                
DELV2C   CLI   RECTYP,ACRTACTH                                                  
         BE    *+10                                                             
         CLI   RECTYP,ACRTACTL                                                  
         BNER  RE                                                               
DELV2C0  NTR1  ,                                                                
         L     R2,AOUT                                                          
         LA    R4,ACTRFST                                                       
         SR    R0,R0                                                            
DELV2C3  CLI   0(R4),0                                                          
         BE    XIT                                                              
         CLI   0(R4),RSTELQ                                                     
         BE    DELV2C5                                                          
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DELV2C3                                                          
*                                                                               
         USING RSTELD,R4                                                        
DELV2C5  NI    RSTSTAT1,X'FF'-RSTSVB2C  TURNOFF VEND=2C                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND LITERAL POOL                                          *         
***********************************************************************         
                                                                                
BALOFF   DS    CL2                 OFFICE FOR OPENING BALANCE                   
OACT     DS    CL14                                                             
NACT     DS    CL14                                                             
CLNOF    DS    CL2                                                              
WRK      DS    CL60                                                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
ACNVD    DSECT                                                                  
       ++INCLUDE ACNVWORK                                                       
         EJECT                                                                  
* ACNVDSECT                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACNVDSECT                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034ACNV05YN  11/06/00'                                      
         END                                                                    
