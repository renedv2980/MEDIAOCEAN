*          DATA SET ACINF06    AT LEVEL 015 AS OF 02/21/15                      
*PHASE T60506A,*,NOAUTO                                                         
         TITLE 'ACCOUNTS INFO PROGRAM - RECORD TYPE AC'                         
T60506   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR RECORD TYPE ACCOUNT  IN ACCOUNTS INFO PROGRAM         
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60506)                                               
         DC    A(FILTABLE-T60506)                                               
         DC    A(PREHEADS-T60506)                                               
         DC    A(PRETABLE-T60506)                                               
         DC    A(HEADINGS-T60506)                                               
         DC    A(DATTABLE-T60506)                                               
         DC    A(KNTRYPNT-T60506)                                               
         DC    A(FNTRYPNT-T60506)                                               
         DC    A(DNTRYPNT-T60506)                                               
         DS    A                                                                
         DS    A                                                                
*                                                                               
         EJECT                                                                  
*              KEYS TABLE COVERED BY DSECT KEYTABD                              
         SPACE 1                                                                
KEYTABLE DC    CL10'COMPANY'                                                    
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(MYCO-GWS)                                                    
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'UNIT'                                                       
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(SCANBLCK-GWS)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'LEDGER'                                                     
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(SCANBLCK-GWS+1)                                              
*&&UK*&& DC    AL2(0)                                                           
*&&US*&& DC    X'FF00'                                                          
*                                                                               
         DC    CL10'ACCOUNT'                                                    
         DC    C'O'                                                             
         DC    C'V'                                                             
         DC    X'41'                                                            
         DC    AL1(3)                                                           
         DC    AL1(12)                                                          
         DC    AL2(SCANBLCK-GWS+2)                                              
*&&UK*&& DC    AL2(0)                                                           
*&&US*&& DC    X'FF00'                                                          
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
*                                                                               
*              FILTER TABLE COVERED BY DSECT FILTERSD                           
*                                                                               
FILTABLE DC    CL10'DELETED'                                                    
         DC    CL2'DE'                                                          
         DC    CL8'YES'                                                         
         DC    X'80'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL2(ACSTATUS-ACKEYD)                                             
         DC    AL1(L'ACSTATUS)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'COST'                                                       
         DC    CL2'CO'                                                          
         DC    CL8'YES'                                                         
         DC    X'02'                                                            
         DC    AL2(ASTA-GWS)                                                    
         DC    AL2(RSTSTAT2-RSTELD)                                             
         DC    AL1(L'RSTSTAT2)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'COST'                                                       
         DC    CL2'CO'                                                          
         DC    CL8'NO'                                                          
         DC    X'01'                                                            
         DC    AL2(ASTA-GWS)                                                    
         DC    AL2(RSTSTAT2-RSTELD)                                             
         DC    AL1(L'RSTSTAT2)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'FILTER1'                                                    
         DC    CL2'F1'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ACOMFILT-GWS)                                                
         DC    AL2(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'FILTER2'                                                    
         DC    CL2'F2'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ACOMFILT-GWS)                                                
         DC    AL2(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'FILTER3'                                                    
         DC    CL2'F3'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ACOMFILT-GWS)                                                
         DC    AL2(2)                                                           
         DC    AL1(L'ACSTANAL)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'FILTER4'       FILTER #4                                    
         DC    CL2'F4'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ACOMFILT-GWS)                                                
         DC    AL2(3)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'FILTER5'       FILTER #5                                    
         DC    CL2'F5'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ACOMFILT-GWS)                                                
         DC    AL2(4)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'IND'                                                        
         DC    CL2'IN'                                                          
         DC    CL8'YES'                                                         
         DC    X'08'                                                            
         DC    AL2(ASTA-GWS)                                                    
         DC    AL2(ACSTSTX-ACSTATD)                                             
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'ANALYSIS'                                                   
         DC    CL2'AN'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ASTA-GWS)                                                    
         DC    AL2(ACSTCOST-ACSTATD)                                            
         DC    AL1(L'ACSTCOST)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'LEVEL'                                                      
         DC    CL2'LE'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ALEVEL-GWS)                                                  
         DC    AL2(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'LEVEL'                                                      
         DC    CL2'LE'                                                          
         DC    CL8'1-2'                                                         
         DC    X'04'                                                            
         DC    AL2(ALEVEL-GWS)                                                  
         DC    AL2(3)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'NUM2'                                                       
         DC    CL2'N2'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ANO2-GWS)                                                    
         DC    AL2(ACNO-ACNOD)                                                  
         DC    AL1(7)                                                           
         DC    AL2(0)                                                           
*                                                                               
*&&UK                                                                           
         DC    CL10'UFORA'                                                      
         DC    CL2'UA'                                                          
*&&                                                                             
*&&US                                                                           
         DC    CL10'OFFICE'                                                     
         DC    CL2'OF'                                                          
*&&                                                                             
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(APRO-GWS)                                                    
         DC    AL2(ACPROFFC-ACPROFD)                                            
         DC    AL1(L'ACPROFFC)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'SECOVER'       SECURITY NUMBER GTR THAN NNN                 
         DC    CL2'SO'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ASTA-GWS)                                                    
         DC    AL2(ACSTSECY+1-ACSTATD)                                          
         DC    AL1(3)                                                           
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'SECUPTO'       SECURITY NUMBER NOT GTR THAN NNN             
         DC    CL2'SU'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ASTA-GWS)                                                    
         DC    AL2(ACSTSECY+1-ACSTATD)                                          
         DC    AL1(3)                                                           
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'SECURITY'                                                   
         DC    CL2'SE'                                                          
         DC    CL8'YES'                                                         
         DC    X'80'                                                            
         DC    AL2(0)                                                           
         DS    5C                                                               
*                                                                               
*                                                                               
         DC    CL10'STATUS'                                                     
         DC    CL2'ST'                                                          
         DC    CL8'CLOSED'                                                      
         DC    X'40'                                                            
         DC    AL2(ASTA-GWS)                                                    
         DC    AL2(ACSTSTAT-ACSTATD)                                            
         DC    AL1(L'ACSTSTAT)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'STATUS'                                                     
         DC    CL2'ST'                                                          
         DC    CL8'DEPTS'                                                       
         DC    X'10'                                                            
         DC    AL2(ASTA-GWS)                                                    
         DC    AL2(ACSTSTAT-ACSTATD)                                            
         DC    AL1(L'ACSTSTAT)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'STATUS'                                                     
         DC    CL2'ST'                                                          
         DC    CL8'LOCKED'                                                      
         DC    X'20'                                                            
         DC    AL2(ASTA-GWS)                                                    
         DC    AL2(ACSTSTAT-ACSTATD)                                            
         DC    AL1(L'ACSTSTAT)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'STATUS'                                                     
         DC    CL2'ST'                                                          
         DC    CL8'STAFF'                                                       
         DC    X'80'                                                            
         DC    AL2(ASTA-GWS)                                                    
         DC    AL2(ACSTSTAT-ACSTATD)                                            
         DC    AL1(L'ACSTSTAT)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF FILTER TABLE                          
*                                                                               
         EJECT                                                                  
*              PRE-HEADING LINE - TO CONTAIN CONSTANT ELEMENTS OF               
*                                 EXPRESSIONS IN FORM X=Y,A=B                   
*                                                                               
*              CL39      C                      SCREEN COLS  2-40               
*              CL39      C                      SCREEN COLS 41-79               
*                                                                               
PREHEADS DC    CL39'UNIT=                                / '                    
         DC    CL39'LEDGER=                                '                    
*                                                                               
*              PRE-HEADING DATA TABLE -  9 BYTE ENTRIES                         
*                                                                               
*              CONTENTS AS FOR DISPLAY DATA TABLE BELOW                         
*                                                                               
PRETABLE DC    AL2(AKEY-GWS)       UNIT CODE                                    
         DC    AL2(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL1(7)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(AUNITNAM-GWS)   UNIT NAME                                    
         DC    AL2(0)                                                           
         DC    AL1(29)                                                          
         DC    AL2(0)                                                           
         DC    AL1(9)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(AKEY-GWS)       LEDGER CODE                                  
         DC    AL2(2)                                                           
         DC    AL1(1)                                                           
*&&UK*&& DC    AL2(0)                                                           
*&&US*&& DC    X'FF00'                                                          
         DC    AL1(48)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ALEDGNAM-GWS)   LEDGER NAME                                  
         DC    AL2(0)                                                           
*&&UK*&& DC    AL1(30)                                                          
*&&US*&& DC    AL1(29)                                                          
         DC    AL2(0)                                                           
*&&UK*&& DC    AL1(50)                                                          
*&&US*&& DC    AL1(51)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    X'FF'               END OF PRE-HEADING DATA TABLE                
*                                                                               
*              SCREEN HEADINGS - 2 LINES                                        
*                                                                               
*              CL39      C         FIRST LINE - SCREEN COLS  2-40               
*              CL39      C         2ND   LINE -                                 
*              CL39      C         FIRST LINE - SCREEN COLS 41-79               
*              CL39      C         2ND   LINE                                   
*                                                                               
HEADINGS DC    CL39'ACCOUNT CODE ------------ACCOUNT NAME--'                    
         DC    CL39'                                       '                    
         DC    CL39'--------- FILTS/STAT  ------DATES------'                    
         DC    CL39'          12345APCLDM BAL B/FD ACTIVITY'                    
*                                                                               
         EJECT                                                                  
*                                                                               
*              DISPLAY DATA TABLE COVERED BY DSECT DATTABD                      
*                                                                               
DATTABLE DC    AL2(AKEY-GWS)       ACCOUNT CODE                                 
         DC    AL2(3)                                                           
         DC    AL1(12)                                                          
         DC    AL2(0)                                                           
         DC    AL1(2)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ANAM-GWS)       A/C NAME, IND=Y, NUM2 & SECURITY             
         DC    AL2(ACNMNAME-ACNAMED)                                            
         DC    AL1(L'ACNMNAME)                                                  
         DC    X'FF00'                                                          
         DC    AL1(15)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ASTA-GWS)       FILTER 1                                     
         DC    AL2(ACSTFILT-ACSTATD)                                            
         DC    AL1(1)                                                           
         DC    AL2(EDITSTOP-GWS)                                                
         DC    AL1(51)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ASTA-GWS)       FILTER 2                                     
         DC    AL2(20)                                                          
         DC    AL1(1)                                                           
         DC    AL2(EDITSTOP-GWS)                                                
         DC    AL1(52)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ASTA-GWS)       FILTER 3                                     
         DC    AL2(ACSTANAL-ACSTATD)                                            
         DC    AL1(L'ACSTANAL)                                                  
*&&UK                                                                           
         DC    AL2(EDITSTOP-GWS)                                                
*&&                                                                             
*&&US                                                                           
         DC    X'FF00'                                                          
*&&                                                                             
         DC    AL1(53)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ASTA-GWS)       FILTER 4                                     
         DC    AL2(ACSTSUB-ACSTATD)                                             
         DC    AL1(L'ACSTSUB)                                                   
         DC    AL2(EDITSTOP-GWS)                                                
         DC    AL1(54)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ASTA-GWS)       FILTER 5                                     
         DC    AL2(ACSTFLT5-ACSTATD)                                            
         DC    AL1(1)                                                           
         DC    AL2(EDITSTOP-GWS)                                                
         DC    AL1(55)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ASTA-GWS)       COSTING GROUP FROM STATUS ELEM               
         DC    AL2(ACSTCOST-ACSTATD)                                            
         DC    AL1(L'ACSTCOST)                                                  
         DC    AL2(EDITACDE-GWS)                                                
         DC    AL1(56)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(APOS-GWS)       COSTING GROUP FROM POSTING ELEM              
         DC    AL2(ACSPACCT-ACSPECD)                                            
         DC    AL1(1)                                                           
         DC    AL2(EDITACDE-GWS)                                                
         DC    AL1(56)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ASTA-GWS)       STATUS BYTE                                  
         DC    AL2(ACSTSTAT-ACSTATD)                                            
         DC    AL1(L'ACSTSTAT)                                                  
         DC    AL2(EDITSTAT-GWS)                                                
         DC    AL1(57)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ASTA-GWS)       COST = Y/N                                   
         DC    AL2(RSTSTAT2-RSTELD)                                             
         DC    AL1(L'RSTSTAT2)                                                  
         DC    AL2(EDITCOST-GWS)                                                
         DC    AL1(61)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ASTA-GWS)       DATE BAL B/F                                 
         DC    AL2(ACSTBFDT-ACSTATD)                                            
         DC    AL1(L'ACSTBFDT)                                                  
         DC    AL2(EDITDATE-GWS)                                                
         DC    AL1(63)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ASTA-GWS)       DATE OF LAST TRANSACTION                     
         DC    AL2(ACSTLAST-ACSTATD)                                            
         DC    AL1(L'ACSTLAST)                                                  
         DC    AL2(EDITDATE-GWS)                                                
         DC    AL1(72)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    X'FF'               END OF DISPLAY DATA TABLE                    
         EJECT                                                                  
*              HANDLE LEDGER/ACCOUNT KEY INPUT (US ONLY)                        
         SPACE 1                                                                
KNTRYPNT DS    0D                                                               
         NMOD1 0,**ACKI**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60506,RB                                                        
         USING KEYTABD,R4          R4=A(KEYTAB ENTRY)                           
         SPACE 1                                                                
         CLI   SCANBLCK,C'T'       SPECIAL FOR UNIT T                           
         BNE   KEXIT                                                            
         CLI   KEYNAME,C'L'        LEDGER ROUTINE                               
         BNE   KNTR4                                                            
         GOTO1 VHEXIN,DMCB,(R2),WORK,2  REQUIRE VALID HEX INPUT                 
         OC    DMCB+12(4),DMCB+12                                               
         BZ    KERROR                                                           
         LA    R2,WORK                                                          
         LA    R3,1                                                             
         B     KEXIT                                                            
         SPACE 1                                                                
KNTR4    LA    R2,1(R2)            ACCOUNT STARTS IN NEXT POSITION              
         B     KEXIT                                                            
         SPACE 1                                                                
KERROR   MVI   DMCB,1                                                           
         MVI   ERROR,LEDGNVAL                                                   
         B     *+8                                                              
KEXIT    MVI   DMCB,0                                                           
         XIT1  REGS=(R2,R3)        RETURN ADDRESS & LENGTH                      
         EJECT                                                                  
FNTRYPNT DS    0D                  HANDLE SECURITY NUMBER FILTER                
         NMOD1 0,**ACFI**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60506,RB                                                        
         USING FTBD,R6                                                          
         ZIC   R1,FTBVAL                                                        
         CLC   SECURITY,FTBVAL     IF SECURITY OF THIS RECORD IS GTR            
         BH    *+8                 THAN THE FILTER VALUE SET THE                
         LA    R1,1(R1)            COMPARAND TO EQUAL TO THE FILTER             
         STC   R1,0(R2)            VALUE - OTHERWISE TO UNEQUAL                 
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
DNTRYPNT DS    0D                                                               
         NMOD1 4,**ACCO**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60506,RB                                                        
         USING T605TWA,RA                                                       
         L     R9,AKEY                                                          
         LA    R9,2(R9)            LEDGER CODE                                  
         CR    R9,R2                                                            
         BNE   T6060                                                            
         BCTR  R9,0                                                             
         CLI   0(R9),C'T'          SPECIAL FOR UNIT T                           
         BNE   T606X                                                            
         GOTO1 VHEXOUT,DMCB,(R2),WORK,1,=C'TOG' DISPLAY HEX LEDGER              
         LA    R3,2                                                             
         LA    R2,WORK                                                          
         B     T606X               RETURN NEW LENGTH & ADDRESS                  
         EJECT                                                                  
T6060    L     R9,ASTA             WHICH FIELD ARE WE HANDLING                  
         USING ACSTATD,R9                                                       
         LA    R5,ACSTANAL                                                      
         CR    R2,R5                                                            
         BNE   T6063                                                            
         CLI   0(R2),C' '                                                       
         BH    T606X                                                            
         MVI   0(R2),C'.'                                                       
         B     T606X                                                            
         EJECT                                                                  
T6063    MVI   WORK,C' '           HANDLE A/C NAME, SALES A/C AND NUM2          
         MVC   WORK+1(L'WORK-1),WORK                                            
         LA    R2,WORK             R2 = A(DISPLAY FIELD)                        
         SR    R4,R4               R4 = NUMBER OF EXTRA LINES                   
         LR    R5,R2               R5 = DISPLAY FIELD LINE POINTER              
         SPACE 1                                                                
T6064    L     R6,ANAM             SET UP NAME                                  
         USING ACNAMED,R6                                                       
         ZIC   R7,ACNMLEN                                                       
         SH    R7,=H'3'                                                         
         BM    T6067                                                            
         LA    R5,WORK+36                                                       
         EX    R7,*+8                                                           
         B     T6067                                                            
         MVC   WORK(0),ACNMNAME                                                 
         SPACE 1                                                                
T6067    ICM   R6,15,ASTA          SET UP IND=Y ON NEXT LINE                    
         BZ    T6068                                                            
         USING ACSTATD,R6                                                       
         TM    ACSTSTX,X'08'                                                    
         BZ    T6068                                                            
         MVC   0(5,R5),=C'IND=Y'                                                
         LA    R5,4(R5)                                                         
         SPACE 1                                                                
T6068    ICM   R6,15,ANO2          SET UP NUM2=NNN.. ON NEXT LINE               
         BZ    T6069                                                            
         USING ACNOD,R6                                                         
         IC    R7,ACNOLEN                                                       
         SH    R7,=H'3'                                                         
         BM    T6069                                                            
         CLI   0(R5),C'Y'                                                       
         BNE   *+12                                                             
         MVI   1(R5),C','                                                       
         LA    R5,2(R5)                                                         
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   5(0,R5),ACNO                                                     
         MVC   0(5,R5),=C'NUM2='                                                
*&&US                                                                           
         TM    LEDGSTAT,X'10'      SEE IF THIS IS VEHICLE NUMBER                
         BZ    *+10                                                             
         MVC   0(5,R5),=C'VEHL='                                                
*&&                                                                             
         SPACE 1                                                                
T6069    CLI   WORK+36,C' '                                                     
         BE    T6069A                                                           
         LA    R5,36(R5)                                                        
         LA    R4,1(R4)                                                         
         SPACE 1                                                                
T6069A   TM    OPTIONS,X'80'       OPTIONAL SECURITY NUMBER & SOURCE            
         BNO   T606X                                                            
         LA    R4,1(R4)            BUMP LINE COUNT                              
         MVC   0(9,R5),=CL9'SECURITY='                                          
         EDIT  SECURITY,(3,9(R5)),ALIGN=LEFT,WRK=EDITWRK                        
         OI    9(R5),C'0'                                                       
         CLI   SECURITY,0                                                       
         BE    T606X                                                            
         LR    R1,R0               UNLESS SECURITY IS ZERO APPEND               
         LA    R5,9(R1,R5)         SOURCE OF SECURITY IN BRACKETS IE            
         MVI   0(R5),C'('          EITHER 'LEDGER' OR THE DESCRIPTION           
         CLC   SECURITY,SAVESECY   OF THE ACCOUNT LEVEL RESPONSIBLE             
         BNE   *+14                                                             
         MVC   1(6,R5),=CL6'LEDGER'                                             
         B     T606C                                                            
         LA    R6,SAVESECY+1                                                    
         L     R7,ASAVEHIE                                                      
         USING ACHEIRD,R7                                                       
         LA    R7,ACHRDESA                                                      
         DROP  R7                                                               
         LA    R8,3                                                             
T606A    CLC   SECURITY,0(R6)                                                   
         BE    T606B                                                            
         LA    R6,1(R6)                                                         
         LA    R7,16(R7)                                                        
         BCT   R8,T606A                                                         
T606B    MVC   1(L'ACHRDESA,R5),0(R7)                                           
T606C    LA    R5,15(R5)                                                        
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVI   1(R5),C')'                                                       
         SPACE 1                                                                
T606X    MVI   DMCB,0                                                           
         XIT1  REGS=(R2,R4)                                                     
         DROP  R6                                                               
*                                                                               
******* VARIABLES ********                                                      
EDITWRK  DS    CL17                                                             
**************************                                                      
************** ++INCLUDE ACGENFILE COMMENTED OUT BY DEIS                        
************** ACGENFILE IS ALREADY INCLUDED IN ACINFDSECT                      
*****  ++INCLUDE ACGENFILE                                                      
         PRINT OFF                                                              
*                                                                               
*              NESTED INCLUDE FOR ACINFDSECT                                    
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE ACINFDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015ACINF06   02/21/15'                                      
         END                                                                    
