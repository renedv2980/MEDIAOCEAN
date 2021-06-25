*          DATA SET ACNV05BS   AT LEVEL 007 AS OF 09/19/00                      
*PHASE ACNV5BSA ACNV05BS                                                        
         TITLE 'ACCPAK CONVERSION - BSNY HOOK'                                  
ACNV05BS CSECT                                                                  
         PRINT NOGEN                                                            
BGN      DS    0D                                                               
         NMOD1 0,*BSHK*,R8                                                      
         USING ACNVD,R9             STORAGE COMMON WITH BASE                    
*                                                                               
         CLI   MODE,CHNGACC        TEST CHANGE ACCOUNT                          
         BNE   INPMODE                                                          
         L     R4,HKNEWA                                                        
         CLC   SRCHARG(2),=C'1P'   TEST 1P ACCOUNT                              
         BNE   CHNGA3                                                           
         MVC   0(14,R4),=CL14'1P999999999999'                                   
         B     XIT                                                              
*                                                                               
CHNGA3   CLC   SRCHARG(2),=C'SR'   TEST SR ACCOUNT                              
         BNE   XIT                                                              
         LA    R3,SRCHARG                                                       
         BAS   RE,MILLER                                                        
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
ULCHK7   MVC   LASTUL,4(RE)        SAVE FOR NEXT TIIME                          
ULCHK9   ICM   RF,15,4(RE)         SPECIAL ROUTINE BY U/L                       
         BZ    XIT                 NO ROUTINE FOR THIS U/L                      
         BR    RF                                                               
         EJECT                                                                  
***********************************************************************         
* LEDGER CONTROL TABLE                                                *         
***********************************************************************         
                                                                                
ULTAB    DS    0D                                                               
         DC    C'GB',AL2(0),AL4(GBINP)                                          
         DC    C'GP',AL2(0),AL4(GBINP)                                          
*                                                                               
         DC    C'SA',AL2(0),AL4(SAINP)                                          
*                                                                               
         DC    C'SB',AL2(0),AL4(SBINP)                                          
*                                                                               
         DC    C'SC',AL2(0),AL4(SCINP)                                          
*                                                                               
         DC    C'SE',AL2(0),AL4(SEINP)                                          
*                                                                               
         DC    C'SI',AL2(0),AL4(SIINP)                                          
*                                                                               
         DC    C'SK',AL2(0),AL4(SKINP)                                          
*                                                                               
         DC    C'SP',AL2(0),AL4(SPINP)                                          
         DC    C'SQ',AL2(0),AL4(SPINP)                                          
         DC    C'SS',AL2(0),AL4(SPINP)                                          
         DC    C'ST',AL2(0),AL4(SPINP)                                          
         DC    C'SU',AL2(0),AL4(SPINP)                                          
*                                                                               
         DC    C'SR',AL2(0),AL4(SRINP)                                          
*                                                                               
         DC    C'SV',AL2(0),AL4(SVINP)                                          
         DC    C'SX',AL2(0),AL4(SVINP)                                          
         DC    C'SY',AL2(0),AL4(SVINP)                                          
*                                                                               
         DC    C'SZ',AL2(0),AL4(SZINP)                                          
         DC    C'S9',AL2(0),AL4(SZINP)                                          
*                                                                               
         DC    C'1C',AL2(0),AL4(ONECINP)                                        
         DC    C'1R',AL2(0),AL4(ONECINP)                                        
         DC    C'11',AL2(0),AL4(ONECINP)                                        
         DC    C'12',AL2(0),AL4(ONECINP)                                        
         DC    C'2D',AL2(0),AL4(ONECINP)                                        
         DC    C'2P',AL2(0),AL4(ONECINP)                                        
*                                                                               
         DC    C'1P',AL2(0),AL4(ONEPINP)                                        
*                                                                               
         DC    C'2C',AL2(0),AL4(TWOCINP)                                        
*                                                                               
         DC    C'27',AL2(0),AL4(TWO7INP)                                        
*                                                                               
         DC    C'28',AL2(0),AL4(TWO8INP)                                        
         DC    C'29',AL2(0),AL4(TWO8INP)                                        
*                                                                               
         DC    AL1(EOT)                                                         
*                                                                               
LASTUL   DC    XL(L'ULTAB)'00'                                                  
         EJECT                                                                  
***********************************************************************         
* GB\GP                                                               *         
***********************************************************************         
                                                                                
GBINP    CLI   MODE,PROCAFT        PROCESS INPUT AFTER CONVERSION               
         BNE   GBOUT                                                            
         BAS   RE,DELHI            DELETE OLD LEDGER AND ACC HIGH               
         BAS   RE,UPBAL            SET BBF BY OFFICE                            
         B     XIT                                                              
*                                                                               
GBOUT    CLI   MODE,PROCOUT        PROCESS FINAL OUTPUT                         
         BNE   XIT                                                              
         BAS   RE,SETLCK           LOCK ALL LOW LEVEL ACCOUNTS                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SA                                                                  *         
***********************************************************************         
                                                                                
*                                                                               
SAINP    CLI   MODE,PROCBFR                                                     
         BNE   SAAFT                                                            
         BAS   RE,DELPTR           DELETE OLD POINTER                           
         B     XIT                                                              
*                                                                               
SAAFT    CLI   MODE,PROCAFT                                                     
         BNE   SAMRG                                                            
         BAS   RE,DELHI            DELETE OLD LEDGER AND ACC HIGH               
         BAS   RE,UPBAL                                                         
         DROP  R2                                                               
*                                                                               
         USING TRNRECD,R2                                                       
         CLI   RECTYP,ACRTTRN      TEST TRANSACTION                             
         BNE   XIT                                                              
         MVC   TRNKOFF,TRNKACT     OFFICE FROM ACCOUNT CODE                     
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
         MVC   TRNOFFC,TRNKOFF     SAME TO ELEMENT                              
         MVC   NEWOFF,TRNOFFC                                                   
         B     XIT                                                              
         DROP  R3                                                               
*                                                                               
SAMRG    CLI   MODE,MRGEPRO                                                     
         BNE   XIT                                                              
         BAS   RE,GLPTR                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SB                                                                  *         
***********************************************************************         
                                                                                
         USING ACTRECD,R2                                                       
SBINP    CLI   MODE,PROCBFR                                                     
         BNE   SBAFT                                                            
         BAS   RE,DELPTR           DELETE OLD POINTER                           
         B     XIT                                                              
*                                                                               
SBAFT    CLI   MODE,PROCAFT        TEST INPUT COMPANY                           
         BNE   SBOUT                                                            
         BAS   RE,DELHI            DELETE OLD LEDGER AND ACC HIGH               
         BAS   RE,UPBAL            BBF OFFCIE                                   
         B     XIT                                                              
*                                                                               
SBOUT    CLI   MODE,PROCOUT        FINAL OUTPUT                                 
         BNE   XIT                                                              
         BAS   RE,GLSPT            UPDATE GENERAL LEDGER POINTER                
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SC                                                                  *         
***********************************************************************         
                                                                                
SCINP    CLI   MODE,PROCAFT        TEST INPUT COMPANY                           
         BNE   XIT                                                              
         BAS   RE,UPBAL            UPDATE BBF                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SE                                                                  *         
***********************************************************************         
                                                                                
SEINP    CLI   MODE,PROCBFR                                                     
         BNE   SEAFT                                                            
         BAS   RE,DELPTR           DELETE OLD POINTER                           
         B     XIT                                                              
*                                                                               
SEAFT    CLI   MODE,PROCAFT                                                     
         BNE   SEOUT                                                            
         BAS   RE,DELHI            DELETE OLD LEDGER AND ACCT HIGH              
         BAS   RE,UPBAL            BBF OFFICE                                   
         B     XIT                                                              
*                                                                               
SEOUT    CLI   MODE,PROCOUT                                                     
         BNE   XIT                                                              
         BAS   RE,GLSPT            UPDATE GENERAL LEDGER POINTER                
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SI                                                                  *         
***********************************************************************         
                                                                                
SIINP    CLI   MODE,PROCAFT                                                     
         BNE   SIOUT                                                            
         BAS   RE,DELHI            DELETE OLD LEDGER AND ACC HIGH               
         BAS   RE,UPBAL            BBF OFFICE                                   
         B     XIT                                                              
*                                                                               
SIOUT    CLI   MODE,PROCOUT                                                     
         BNE   XIT                                                              
         BAS   RE,SIANL             ANALYSIS POINTER                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SK                                                                  *         
***********************************************************************         
                                                                                
SKINP    CLI   MODE,PROCBFR                                                     
         BNE   SKAFT                                                            
         BAS   RE,DELPTR           DELETE OLD POINTER                           
         B     XIT                                                              
*                                                                               
SKAFT    CLI   MODE,PROCAFT                                                     
         BNE   SKOUT                                                            
         BAS   RE,DELHI            DELETE OLD LEDGER AND ACC HIGH               
         BAS   RE,UPBAL            BBF OFFICE                                   
         B     XIT                                                              
*                                                                               
SKOUT    CLI   MODE,PROCOUT                                                     
         BNE   XIT                                                              
         BAS   RE,GLPTR                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SP\SQ\SS\ST\SU                                                      *         
***********************************************************************         
                                                                                
SPINP    CLI   MODE,PROCBFR                                                     
         BNE   SPAFT                                                            
         BAS   RE,DELPTR           DELETE OLD POINTER                           
         B     XIT                                                              
*                                                                               
SPAFT    CLI   MODE,PROCAFT                                                     
         BNE   SPOUT                                                            
         BAS   RE,UPBAL            BBF OFFICE                                   
         B     XIT                                                              
*                                                                               
SPOUT    CLI   MODE,PROCOUT                                                     
         BNE   XIT                                                              
         BAS   RE,GLPTR            G\L POINTERS                                 
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SR                                                                            
***********************************************************************         
                                                                                
SRINP    CLI   MODE,PROCBFR                                                     
         BNE   SRAFT                                                            
         BAS   RE,DELPTR           DELETE OLD POINTER                           
         B     XIT                                                              
*                                                                               
SRAFT    CLI   MODE,PROCAFT                                                     
         BNE   SROUT                                                            
         BAS   RE,DELHI            DELETE OLD LEDGER AND ACC HIGH               
*                                                                               
         CLI   RECTYP,ACRTACTL     TEST ACCOUNT LOW                             
         BNE   *+8                                                              
         BAS   RE,UPBAL            BBF OFFICE                                   
*                                                                               
         L     R3,AINP             R3=A(INPUT ACCOUNT)                          
         LA    R3,ACTKULA-ACTKEY(R3)                                            
         CLC   0(2,R3),=C'SR'                                                   
         BNE   XIT                                                              
         LA    R4,WORK                                                          
         MVC   WORK,SPACE                                                       
         BAS   RE,MILLER           CHECK FOR MILLER\MOLSON RULE                 
         CLC   WORK,SPACE          TEST NEW SR ACCOUNT                          
         BE    SRAFT5                                                           
         MVC   NEWULA,WORK         SET NEW ACCOUNT CODE                         
         MVC   ACTKULA,WORK                                                     
*                                                                               
         USING TRNRECD,R2                                                       
SRAFT5   CLI   RECTYP,ACRTTRN      TEST TRANSACTION                             
         BNE   XIT                                                              
         MVC   TRNKOFF,TRNKACT+1   OFFICE FROM LEVEL 2 OF ACCOUNT               
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
         CLI   TRNEL,TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TRNOFFC,TRNKOFF     SAME TO ELEMENT                              
         B     XIT                                                              
         DROP  R3                                                               
*                                                                               
SROUT    CLI   MODE,PROCOUT                                                     
         BNE   XIT                                                              
         BAS   RE,GLPTR            G\L POINTERS                                 
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SV\SX\SY                                                                      
***********************************************************************         
                                                                                
         USING ACTRECD,R2                                                       
SVINP    CLI   MODE,PROCBFR                                                     
         BNE   SVAFT                                                            
         BAS   RE,DELPTR           DELETE OLD POINTER                           
         B     XIT                                                              
*                                                                               
SVAFT    CLI   MODE,PROCAFT                                                     
         BNE   SVOUT                                                            
         BAS   RE,UPBAL            BBF OFFICE                                   
         B     XIT                                                              
*                                                                               
SVOUT    CLI   MODE,PROCOUT                                                     
         BNE   XIT                                                              
         BAS   RE,GLPTR            G\L POINTERS                                 
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SZ\S9                                                               *         
***********************************************************************         
                                                                                
SZINP    CLI   MODE,PROCAFT                                                     
         BNE   XIT                                                              
         BAS   RE,UPBAL            BBF OFFICE                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* 1C\1R\11\12\2D\2P                                                   *         
***********************************************************************         
                                                                                
ONECINP  CLI   MODE,PROCAFT                                                     
         BNE   XIT                                                              
         BAS   RE,DELHI            DELETE OLD LEDGER AND ACC HIGH               
         BAS   RE,UPBAL            BBF OFFICE                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* 1P                                                                  *         
***********************************************************************         
                                                                                
ONEPINP  CLI   MODE,PROCAFT                                                     
         BNE   XIT                                                              
         BAS   RE,DELHI            DELETE OLD LEDGER AND ACC HIGH               
         CLI   RECTYP,ACRTACTL                                                  
         BL    XIT                                                              
         CLI   RECTYP,ACRTTRN                                                   
         BH    XIT                                                              
         MVC   ACTKULA,=CL14'1P999999999999'                                    
*                                                                               
         CLI   RECTYP,ACRTACTL                                                  
         BNE   XIT                                                              
         BAS   RE,UPBAL            BBF OFFICE                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* 2C                                                                  *         
***********************************************************************         
                                                                                
TWOCINP  CLI   MODE,PROCAFT                                                     
         BNE   XIT                                                              
         BAS   RE,UPBAL                                                         
         BAS   RE,DELV2C         DELETE VEND2C                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* 27                                                                  *         
***********************************************************************         
                                                                                
TWO7INP  CLI   MODE,PROCAFT                                                     
         BNE   XIT                                                              
         BAS   RE,UPBAL            BBF OFFICE                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* 28\29                                                                         
***********************************************************************         
                                                                                
TWO8INP  CLI   MODE,PROCAFT                                                     
         BNE   TWO8MRG                                                          
         BAS   RE,DELHI            DELETE OLD LEDGER AND ACC HIGH               
         BAS   RE,UPBAL            BBF OFFICE                                   
         B     XIT                                                              
*                                                                               
TWO8MRG  CLI   MODE,MRGEPRO                                                     
         BNE   XIT                                                              
         BAS   RE,PRFMRG           COPY PROFILE FROM NEW                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DELETE LEDGER OR ACCOUNT HIGH FROM OLD                              *         
***********************************************************************         
                                                                                
DELHI    CLI   RECTYP,ACRTLDG      LEDGER                                       
         BE    *+10                                                             
         CLI   RECTYP,ACRTACTH     ACCOUNT HIGH                                 
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
* MILLER SR RULES                                                     *         
*   R3=A(INPUT ACCOUNT)                                               *         
*   R4=A(OUTPUT FIELD)                                              *           
***********************************************************************         
                                                                                
MILLER   NTR1  ,                                                                
         LA    R5,MMTAB            POINT TO MILLER\MOLSON RULE TABLE            
MILLER3  CLI   0(R5),0             EOT?                                         
         BE    XIT                                                              
         CLC   2(1,R3),0(R5)       DO WE HAVE TO DO MILLER\MOLSON RULE?         
         BE    MILLER5                                                          
         LA    R5,1(R5)                                                         
         B     MILLER3             LOOP UNTIL WE FIND IT                        
*                                                                               
MILLER5  MVC   0(3,R4),0(R3)       SR + FIRST LEVEL TYPE                        
         MVC   3(2,R4),=C'11'      NEW  SECOND LEVEL OFFICE                     
         MVC   5(9,R4),3(R3)       OLD REGION & ACCOUNT                         
         B     XIT                                                              
*                                                                               
MMTAB    DC    C'IKLMNRTZ234'                                                   
         DC    X'00'                                                            
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
         ZAP   OFATTOTD,=P'0'       INITIALIZE BBF                              
         ZAP   OFATTOTC,=P'0'       INITIALIZE BBF                              
         MVC   OFATLMOS,EFFS                                                    
         XC    OFATHMOS,OFATHMOS                                                
         MVI   OFATABLN(R5),X'FF'   MARK NEW END OF TABLE                       
*                                                                               
UPBAL11  AP    OFATBALF,ABLFRWD     ACCUMULATE BBF                              
         B     XIT                                                              
         DROP  R4,R5                                                            
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
**PAN#1  DC    CL21'007ACNV05BS  09/19/00'                                      
         END                                                                    
