*          DATA SET ACNV05DN   AT LEVEL 019 AS OF 09/19/00                      
*PHASE ACNV5DNA ACNV05DN                                                        
         TITLE 'ACCPAK CONVERSION - DNTOA HOOK'                                 
ACNV05DN CSECT                                                                  
         PRINT NOGEN                                                            
BGN      DS    0D                                                               
         NMOD1 0,*DNHK*,R8                                                      
         USING ACNVD,R9             STORAGE COMMON WITH BASE                    
         MVI   COMPSTA,COMPOTON      SET CONVERT OLD TO NEW                     
*                                                                               
         CLI   MODE,CHNGACC        TEST CHANGE ACCOUNT                          
         BNE   INPMODE                                                          
         B     CHNGAC                                                           
*                                                                               
XNO      LTR   RB,RB                                                            
         B     XIT                                                              
XYES     CR    RB,RB                                                            
XIT      XIT1                                                                   
*                                                                               
INPMODE  L     R2,AOUT                                                          
         USING ACTRECD,R2                                                       
         MVC   BALOFF,DFLTOFF      BALANCE OFFICE IS DEFAULT OFFICE             
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
ULCHK7   MVC   LASTUL,4(RE)        SAVE FOR NEXT TIME                           
ULCHK9   ICM   RF,15,4(RE)         SPECIAL ROUTINE BY U/L                       
         BZ    XIT                 NO ROUTINE FOR THIS U/L                      
         BR    RF                                                               
         EJECT                                                                  
***********************************************************************         
* LEDGER CONTROL TABLE                                                *         
***********************************************************************         
                                                                                
ULTAB    DS    0D                                                               
         DC    C'GB',AL2(0),AL4(GBINP)                                          
         DC    C'GP',AL2(0),AL4(GPINP)                                          
*                                                                               
         DC    C'SA',AL2(0),AL4(SAINP)                                          
         DC    C'SB',AL2(0),AL4(SBINP)                                          
         DC    C'SC',AL2(0),AL4(SCINP)                                          
         DC    C'SE',AL2(0),AL4(SEINP)                                          
         DC    C'SG',AL2(0),AL4(SGINP)                                          
         DC    C'SI',AL2(0),AL4(SIINP)                                          
         DC    C'SJ',AL2(0),AL4(SJINP)                                          
         DC    C'SK',AL2(0),AL4(SKINP)                                          
         DC    C'SP',AL2(0),AL4(SPINP)                                          
         DC    C'SQ',AL2(0),AL4(SQINP)                                          
         DC    C'SR',AL2(0),AL4(SRINP)                                          
         DC    C'SS',AL2(0),AL4(SSINP)                                          
         DC    C'ST',AL2(0),AL4(STINP)                                          
         DC    C'SV',AL2(0),AL4(SVINP)                                          
         DC    C'SW',AL2(0),AL4(SWINP)                                          
         DC    C'SZ',AL2(0),AL4(SZINP)                                          
         DC    C'S9',AL2(0),AL4(S9INP)                                          
*                                                                               
         DC    C'1C',AL2(0),AL4(ONECINP)                                        
         DC    C'1N',AL2(0),AL4(ONENINP)                                        
         DC    C'1P',AL2(0),AL4(ONEPINP)                                        
         DC    C'1R',AL2(0),AL4(ONERINP)                                        
         DC    C'11',AL2(0),AL4(ONE1INP)                                        
         DC    C'12',AL2(0),AL4(ONE2INP)                                        
         DC    C'13',AL2(0),AL4(ONE3INP)                                        
         DC    C'14',AL2(0),AL4(ONE4INP)                                        
*                                                                               
         DC    C'2D',AL2(0),AL4(TWODINP)                                        
         DC    C'2P',AL2(0),AL4(TWOPINP)                                        
         DC    C'28',AL2(0),AL4(TWO8INP)                                        
         DC    C'29',AL2(0),AL4(TWO9INP)                                        
*                                                                               
         DC    AL1(EOT)                                                         
*                                                                               
LASTUL   DC    XL(L'ULTAB)'00'                                                  
         EJECT                                                                  
***********************************************************************         
* GB\GP ETC.   NO ACCOUNT CHANGES                                     *         
***********************************************************************         
                                                                                
GBINP    DS    0H                                                               
GPINP    DS    0H                                                               
SBINP    DS    0H                                                               
SEINP    DS    0H                                                               
SGINP    DS    0H                                                               
SIINP    DS    0H                                                               
SJINP    DS    0H                                                               
SKINP    DS    0H                                                               
SPINP    DS    0H                                                               
SQINP    DS    0H                                                               
SSINP    DS    0H                                                               
STINP    DS    0H                                                               
SVINP    DS    0H                                                               
SWINP    DS    0H                                                               
SZINP    DS    0H                                                               
S9INP    DS    0H                                                               
ONENINP  DS    0H                                                               
ONEPINP  DS    0H                                                               
ONE1INP  DS    0H                                                               
ONE2INP  DS    0H                                                               
TWO8INP  DS    0H                                                               
PRC1AFT  DS    0H                                                               
         CLI   MODE,PROCAFT        PROCESS INPUT AFTER CONVERSION               
         BNE   PRC1OUT                                                          
         BAS   RE,UPBAL            SET BBF BY OFFICE                            
         B     XIT                                                              
*                                                                               
PRC1OUT  CLI   MODE,PROCOUT        PROCESS FINAL OUTPUT                         
         BNE   PRC1MRG                                                          
         B     XIT                                                              
*                                                                               
PRC1MRG  CLI   MODE,MRGEPRO        MERGE PROFILE                                
         BNE   XIT                                                              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SA/SR/13/14/1R/29  REPLACE OFFICE CODE                              *         
***********************************************************************         
                                                                                
SAINP    DS    0H                                                               
SRINP    DS    0H                                                               
ONERINP  DS    0H                                                               
ONE3INP  DS    0H                                                               
ONE4INP  DS    0H                                                               
TWODINP  DS    0H                                                               
TWOPINP  DS    0H                                                               
PRC2AFT  DS    0H                                                               
         CLI   MODE,PROCAFT        PROCESS INPUT AFTER CONVERSION               
         BNE   PRC2OUT                                                          
         CLI   RECTYP,ACRTLDG      LEDGER RECORD                                
         BE    XIT                                                              
         BAS   RE,OFFACC           FIX OFFICE IN THE ACCOUNT                    
         BAS   RE,UPBAL            SET BBF BY OFFICE                            
         B     XIT                                                              
*                                                                               
PRC2OUT  CLI   MODE,PROCOUT        PROCESS FINAL OUTPUT                         
         BNE   PRC2MRG                                                          
         B     XIT                                                              
*                                                                               
PRC2MRG  CLI   MODE,MRGEPRO        MERGE PROFILE                                
         BNE   XIT                                                              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SC/1C/29   LINKED LEDGERS                                           *         
***********************************************************************         
                                                                                
SCINP    DS    0H                                                               
ONECINP  DS    0H                                                               
TWO9INP  DS    0H                                                               
PRC3AFT  DS    0H                                                               
         CLI   MODE,PROCAFT        PROCESS INPUT AFTER CONVERSION               
         BNE   PRC3OUT                                                          
         BAS   RE,DELLG            DELETE OLD LEDGER                            
         BAS   RE,DELHI            DELETE OLD ACCOUNT HIGH                      
         BAS   RE,UPBAL            SET BBF BY OFFICE                            
         B     XIT                                                              
*                                                                               
PRC3OUT  CLI   MODE,PROCOUT        PROCESS FINAL OUTPUT                         
         BNE   PRC3MRG                                                          
         B     XIT                                                              
*                                                                               
PRC3MRG  CLI   MODE,MRGEPRO        MERGE PROFILE                                
         BNE   XIT                                                              
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
* CHANGE ACCOUNT CODE                                                 *         
***********************************************************************         
                                                                                
CHNGAC   MVC   OACT,SRCHARG        SET OLD ACCOUNT                              
         BAS   RE,REPOFC           REPLACE OFFICE                               
         CLC   NACT,SPACE          TEST ACCOUNT CHANGED                         
         BE    XNO                                                              
         L     R4,HKNEWA                                                        
         MVC   0(14,R4),NACT                                                    
         B     XYES                ACCOUNT CHANGED                              
         EJECT                                                                  
***********************************************************************         
* CHANGE OFFICE IN KEY FOR ACCOUNT HIGH                               *         
***********************************************************************         
                                                                                
OFFACC   CLI   RECTYP,ACRTACTH                                                  
         BNER  RE                                                               
         CLC   ACTKUNT(2),=C'SR'                                                
         BNE   OFFACC3                                                          
         CLC   ACTKACT+1(11),SPACE                                              
         BNE   OFFACC3                                                          
         OI    HKSTA,HKSDEL               DELETE HIGHEST LEVEL                  
         B     XIT                                                              
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
         CLC   OACT(2),=C'SR'                                                   
         BNE   REPOFC3                                                          
         MVC   WRK(12),OACT+2      FOR SR DROP COMPANY CODE                     
         MVC   OACT+2(12),SPACE                                                 
         MVC   OACT+2(11),WRK+1    THEN CONVERT OFFICE                          
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
         DC    C'SA',AL1(1)                                                     
         DC    C'SR',AL1(1)                                                     
         DC    C'13',AL1(5)                                                     
         DC    C'14',AL1(2)                                                     
         DC    C'1R',AL1(1)                                                     
         DC    C'2D',AL1(1)                                                     
         DC    C'2P',AL1(1)                                                     
         DC    AL1(EOT)                                                         
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
**PAN#1  DC    CL21'019ACNV05DN  09/19/00'                                      
         END                                                                    
