*          DATA SET RENWR10    AT LEVEL 032 AS OF 10/24/11                      
*          DATA SET RENRG02    AT LEVEL 018 AS OF 05/09/97                      
*PHASE T83010B,*                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE GETBROAD                                                               
*INCLUDE GETDAY                                                                 
         TITLE 'T83010 RRGON REC PASS NRGON DATA TO WRITER'                     
***********************************************************************         
* PROFILE BIT USE:                                                    *         
* BYTE 0, X'80'  -  SUPPRESS BUDGET COLUMN                            *         
* BYTE 0, X'40'  -  SUPPRESS BUDGET IF REQUEST IS FROM STATION SIDE.  *         
* BYTE 0,X'20'   -  USE STRAIGHT MONTH TO MONTH, NOT 4/5 WK PACING    *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
* HISTORY OF CHANGES                                                  *         
***********************************************************************         
*                                                                     *         
* 02/26/98  23 BGRI ADD EQV TBL FOR AFFL/RANK/TVB/STA TYPE            *         
* 03/03/98  24 DROP CODE TO BACK UP 1 YEAR - PRIOR TO INIT0610        *         
* 03/10/98  25 MODIFIED RRGW DSECT/ADDED CODE TO GET AFF              *         
* 03/24/98  26 FIX STATION - WAS XXXX-F SHOULD BE XXXX-FM             *         
*              FIX PASSED STATION ADDRESS                             *         
* 06/29/99  27 BYPASS TYPE REP - 01                                   *         
* 03/24/00  28 Y2K BUG                                                *         
* 05/23/02  29 ADD SALESPERSON TO FILTAB                              *         
*                                                                     *         
*                    ***  END TOMBSTONE  ***                          *         
***********************************************************************         
*                                                                     *         
* REGISTER USAGE -                                                    *         
*        R0 - WORK REG                                                *         
*        R1 - WORK REG                                                *         
*        R2 -                                                         *         
*        R3 -                                                         *         
*        R4 - WORK REG & KEY DSECT POINTER                            *         
*        R5 - WORK REG                                                *         
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM   *         
*              FOR DSECT IN VALREC                                    *         
*        R7 -                                                         *         
*        R8 - SECOND BASE                                             *         
*        R9 - POINTER TO SYSD                                         *         
*        RA - POINTER TO RRGWRI DSECT FROM THE WRITER                 *         
*        RB - FIRST BASE                                              *         
*        RC - POINTER TO GEND                                         *         
*        RD - SAVE AREA POINTER                                       *         
*        RE - GOTO1 REG                                               *         
*        RF - GOTO1 REG                                               *         
***********************************************************************         
T83010   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 2500,T83010,R8,RR=R2                                             
         L     RA,0(R1)                                                         
         USING RRGWRI,RA                                                        
         CLC   =C'**WRIT**',RRGWID                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING GEND,RC                                                          
         ST    RD,BASERD                                                        
         LA    R9,IO                                                            
         LA    R9,4016(,R9)        2 2000 BYTE I/O + 2 8 BYTE LABELS            
         USING SYSD,R9                                                          
         USING SYSD,R9                                                          
         ST    RC,SVRC                                                          
         SPACE                                                                  
         LA    R0,8                                                             
         LA    R1,RRGWDT1          FIELD TYPE                                   
         SPACE                                                                  
SWCODE   LA    RE,WRICODEL                                                      
         LA    RF,WRICODE          WRITER - NRGON CODES                         
SWCODE10 CLC   0(1,R1),0(RF)                                                    
         BNE   *+14                                                             
         MVC   0(1,R1),1(RF)       SWITCH TO NRGON CODE                         
         B     SWCODE20                                                         
         SPACE                                                                  
         LA    RF,2(,RF)                                                        
         BCT   RE,SWCODE10                                                      
         SPACE                                                                  
         CLI   0(R1),X'07'         THIS WRITER GROUP CODE                       
         BNE   SWCODE20                                                         
         CLI   2(R1),C' '          THIS ONLY 1 CHARACTER                        
         B     *+8                                                              
         MVI   0(R1),QLGRGRP       SWITCH TO NRGON CODE                         
         SPACE                                                                  
SWCODE20 LA    R1,9(,R1)                                                        
         BCT   R0,SWCODE                                                        
         SPACE                                                                  
         CLI   RRGWOFFL,C'Y'                                                    
         B     *+8                                                              
         BAS   RE,PRTRRGW                                                       
         SPACE                                                                  
         MVI   RRGWERR,0                                                        
         MVI   RRGWSTAT,0                                                       
         SPACE                                                                  
         LR    RE,RC                                                            
         LA    RF,1500                                                          
         SLL   RF,3                                                             
         XCEF                                                                   
         ST    R2,RELO                                                          
*        ST    R9,SYSPARMS                                                      
         ST    RC,SVRC                                                          
         LA    R0,2                ONLY 2 I/O AREAS                             
         LA    R1,IO-8                                                          
         LA    RE,X'F1'                                                         
         LA    RF,AIO1                                                          
GNINIT10 MVC   0(8,R1),=C'**I/O1**'                                             
         STC   RE,5(R1)                                                         
         LA    R1,8(,R1)                                                        
         ST    R1,0(,RF)                                                        
         LA    R1,2000(,R1)                                                     
         LA    RE,1(,RE)                                                        
         LA    RF,4(,RF)                                                        
         BCT   R0,GNINIT10                                                      
         MVC   AIO,AIO1                                                         
         MVC   KEY-8(8),=C'**KEYS**'                                            
         MVC   DUB-8(8),=C'**DUB***'                                            
         MVC   ADDAY-8(8),=C'*EXTRNS*'                                          
         MVC   BOOKVAL-8(8),=C'*CORERES'                                        
         MVC   DATADISP-8(8),=C'*SYSCON*'                                       
         MVI   DATADISP+1,34                                                    
         SPACE                                                                  
         MVC   KEY,RRGWKEY                                                      
         MVC   KEYSAVE,RRGWKEYS                                                 
         MVC   QREGION(L'RRGWFILT),RRGWFILT                                     
         SPACE                                                                  
         CLI   RRGWCNTL,C'V'       VALIDATE ONLY?                               
         BE    *+14                                                             
         SPACE                                                                  
         OC    RRGWDAT1,RRGWDAT1   ALREADY PASSING DATA                         
         BNZ   LREC0010                                                         
         SPACE                                                                  
         CLI   RRGWOFFL,C'Y'       IF OFFLINE, OPEN FILE                        
         BNE   INIT                                                             
         SPACE                                                                  
         CLI   RRGWOPEN,C'Y'       HAS FILE BEEN OPENED                         
         BE    INIT                                                             
         SPACE                                                                  
         L     R2,AIO1                                                          
         GOTO1 RRGWDMGR,DMCB,=C'DMOPEN',=C'REP',RRGLST,(R2)                     
         MVI   RRGWOPEN,C'Y'       SET FILE HAS BEEN OPENED                     
         B     INIT                                                             
         DS    0D                                                               
RRGLST   DC    CL8'NRRGNEW'                                                     
         DC    C'X'                                                             
         SPACE                                                                  
* CONVERSION TABLE NEW WRITER DATA TYPE CODES TO NRGON CODES                    
         SPACE                                                                  
         DS    0H                                                               
WRICODE  DC    X'2A',AL1(QLOWN)    NWR/NRRGON OWNER                             
         DC    X'2B',AL1(QLMKT)    NWR/NRRGON MARKET                            
         DC    X'1A',AL1(QLAGY)    NWR/NRRGON AGENCY                            
         DC    X'32',AL1(QLCON)    NWR/NRRGON CONTRACT TYPE                     
         DC    X'3B',AL1(QLDCT)    NWR/NRRGON DEV CONTRACT TYPE                 
         DC    X'81',AL1(QLAFF)    NWR/NRRGON AFFILIATE                         
         DC    X'82',AL1(QLRNK)    NWR/NRRGON RANK                              
         DC    X'83',AL1(QLTVB)    NWR/NRRGON TVB                               
         DC    X'84',AL1(QLSTY)    NWR/NRRGON STATION TYPE                      
WRICODEL EQU   (*-WRICODE)/2                                                    
         SPACE 3                                                                
DATATYER MVI   RRGWERR,RRGWER08    DATA TYPE COMBO NOT ON FILE                  
         B     ERREXIT                                                          
         SPACE                                                                  
VKNODATA MVI   RRGWERR,RRGWER10                                                 
         SPACE                                                                  
ERREXIT  MVI   RRGWSTAT,01                                                      
         SPACE                                                                  
EXIT     DS   0H                                                                
         MVC   RRGWKEY,KEY                                                      
         MVC   RRGWKEYS,KEYSAVE                                                 
         MVC   RRGWFILT,QREGION                                                 
         CLI   RRGWOFFL,C'Y'                                                    
         B     *+8                                                              
         BAS   RE,PRTRRGW                                                       
         SPACE                                                                  
         LA    R0,8                                                             
         LA    R1,RRGWDT1          FIELD TYPE                                   
         SPACE                                                                  
CODESW   LA    RE,WRICODEL                                                      
         LA    RF,WRICODE          WRITER - NRGON CODES                         
CODESW10 CLC   0(1,R1),1(RF)                                                    
         BNE   *+14                                                             
         MVC   0(1,R1),0(RF)         SWITCH TO NRGON CODE                       
         B     CODESW20                                                         
         SPACE                                                                  
         LA    RF,2(,RF)                                                        
         BCT   RE,CODESW10                                                      
         SPACE                                                                  
         CLI   0(R1),QLGRGRP       THIS WRITER GROUP CODE                       
         B     CODESW20                                                         
         MVI   0(R1),QLGRP         SWITCH TO NRGON CODE                         
         SPACE                                                                  
CODESW20 LA    R1,9(,R1)                                                        
         BCT   R0,CODESW                                                        
         SPACE                                                                  
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        INITIALIZE ROUTINE                                                     
*                                                                               
INIT     DS   0H                                                                
         BAS   RE,SETREPFL         SET UP REP FILE VALUES                       
*                                                                               
         XC    AFSTFLD,AFSTFLD                                                  
*                                                                               
* CLEAR ALL FILTERS                                                             
*                                                                               
         XC    QREGION(QFILTLEN),QREGION                                        
*                                                                               
         XC    ELEM,ELEM           BUILD 1ST PART OF RRGON KEY IN ELEM          
         SPACE                                                                  
         BAS   RE,RSETRRGO         SWITCH BACK TO RRGON FILE VALUES             
         SPACE                                                                  
         XC    KEY,KEY             READ RRGON HEADER RECORD                     
         LA    R7,KEY                                                           
         USING RORECD,R7                                                        
         MVC   ROKREP,RRGWREP                                                   
         MVI   ROKHD1+1,X'01'                                                   
         L     R7,AIO1                                                          
         MVC   KEYSAVE,KEY                                                      
         SPACE                                                                  
         GOTO1 RRGWDMGR,DMCB,=C'DMRDHI',=C'RRGNEW',KEY,(R7)                     
         SPACE                                                                  
         CLC   ROKEY,KEY                                                        
         BNE   VKNODATA                                                         
         L     R7,AIO                                                           
         MVC   SVTYPES,RODHDOPT    SAVE DOLLAR TYPES AVAILABLE                  
         SPACE                                                                  
         LA    R0,L'SVTYPES                                                     
         LA    R1,SVTYPES                                                       
         SPACE                                                                  
         CLI   RRGWDOLT,0           SPECIFIC DOLLAR TYPE REQUESTED?             
         BE    INIT0060             NO, USE WHATEVER ON FILE                    
         SPACE                                                                  
INIT0050 CLC   RRGWDOLT,0(R1)       THIS A VALID TYPE FOR THIS REP              
         BE    INIT0070             YES                                         
         LA    R1,1(,R1)                                                        
         BCT   R0,INIT0050                                                      
         SPACE                                                                  
         MVI   RRGWERR,RRGWER0F                                                 
         B     ERREXIT                                                          
         SPACE                                                                  
INIT0060 CLI   0(R1),0             THIS A VALID TYPE                            
         BNE   INIT0070             YES                                         
         LA    R1,1(,R1)                                                        
         BCT   R0,INIT0060                                                      
         SPACE                                                                  
         MVI   RRGWERR,RRGWER11                                                 
         B     ERREXIT                                                          
         SPACE                                                                  
INIT0070 DS   0H                   SAVE WHATEVER IT IS                          
         MVC   RRGWDOLT,0(R1)                                                   
         SPACE                                                                  
         CLI   RRGWDOLT,C'A'       ALL DOLLARS BECOMES NULL                     
         BNE   *+8                  NO                                          
         MVI   RRGWDOLT,0                                                       
         SPACE                                                                  
* SEE IF OFFICE OR STATION HAVE BEEN REQUESTED *                                
         SPACE                                                                  
         MVI   RRGWSTSW,0                                                       
         MVI   RRGWOFSW,0                                                       
         SPACE                                                                  
         LA    R0,8                                                             
         LA    R1,RRGWDT1                                                       
INIT0074 CLI   0(R1),QLSTA                                                      
         BNE   *+8                                                              
         MVI   RRGWSTSW,C'Y'                                                    
         SPACE                                                                  
         CLI   0(R1),QLOFF                                                      
         BNE   *+8                                                              
         MVI   RRGWOFSW,C'Y'                                                    
         CLI   0(R1),0                                                          
         BE    *+12                                                             
         LA    R1,9(,R1)                                                        
         BCT   R0,INIT0074                                                      
         SPACE                                                                  
         XC    ELEM,ELEM                                                        
*                                                                               
* NEED TO FILL FILTER FIELDS FROM RRGWRI DSECT                                  
*                                                                               
         LA    R2,RRGWDT1          FIELD TYPE                                   
         LA    R3,RRGWDA1          FIELD DATA                                   
         LA    R4,ELEM                                                          
         LA    R0,8                                                             
         SPACE                                                                  
INIT0100 DS   0H                                                                
         CLI   0(R2),1             IS THIS A MYTHICAL REP REQUEST?              
         BE    INIT0160             YES, BYPASS ALTOGETHER                      
         SPACE                                                                  
         L     R1,=A(FILTAB)                                                    
         A     R1,RELO                                                          
         SPACE                                                                  
INIT0102 CLC   0(1,R1),0(R2)                                                    
         BE    INIT0104                                                         
         LA    R1,4(,R1)                                                        
         CLI   0(R1),0                                                          
         BNE   INIT0102                                                         
         DC    H'0'                                                             
         SPACE                                                                  
INIT0104 DS   0H                                                                
         LH    RE,2(R1)                                                         
         AR    RE,R9                                                            
         ZIC   RF,1(R1)                                                         
         BCTR  RF,0                                                             
         EX    RF,INITMVC                                                       
         SPACE                                                                  
         CLI   RRGWSTSW,C'Y'       WAS STATION REQUESTED                        
         BNE   INIT0106                                                         
         SPACE                                                                  
         CLI   0(R2),QLAFF         CK AFFIL LATER                               
         BE    INIT0150                                                         
         SPACE                                                                  
         CLI   0(R2),QLOWN         CK OWNER LATER                               
         BE    INIT0150                                                         
         SPACE                                                                  
         CLI   0(R2),QLRNK         CK RANK LATER                                
         BE    INIT0150                                                         
         SPACE                                                                  
         CLI   0(R2),QLTVB         CK TVB LATER                                 
         BE    INIT0150                                                         
         SPACE                                                                  
         CLI   0(R2),QLGRGRP       CK GROUP LATER                               
         BE    INIT0150                                                         
         SPACE                                                                  
         CLI   0(R2),QLGRP         CK GROUP LATER                               
         BE    INIT0150                                                         
         SPACE                                                                  
INIT0106 CLI   RRGWOFSW,C'Y'       WAS OFFICE REQUESTED                         
         BNE   INIT0107                                                         
         SPACE                                                                  
         CLI   0(R2),QLREG         CK REGION LATER                              
         BE    INIT0150                                                         
         SPACE                                                                  
INIT0107 DS   0H                                                                
         MVC   0(1,R4),0(R2)       MOVE DATA TYPE TO ELEM                       
         LA    R4,1(,R4)                                                        
         SPACE                                                                  
         CLI   0(R2),QLSTA                                                      
         BNE   INIT0110                                                         
         SPACE                                                                  
         OC    0(8,R3),0(R3)       ANY FILTER?                                  
         BZ    INIT0150             NO                                          
         SPACE                                                                  
         LA    R1,4(,RE)                                                        
         CLI   3(R3),C' '                                                       
         BH    *+6                                                              
         BCTR  R1,0                                                             
         SPACE                                                                  
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),4(R3)                                                    
         CLI   4(R3),C' '                                                       
         BNE   INIT0108                                                         
         MVC   1(2,R1),=C'TV'                                                   
         B     INIT0150                                                         
         SPACE                                                                  
INIT0108 DS    0H                                                               
         CLI   4(R3),C'A'                                                       
         BE    *+12                                                             
         CLI   4(R3),C'F'                                                       
         BNE   *+12                                                             
         MVI   2(R1),C'M'                                                       
         B     INIT0150                                                         
         SPACE                                                                  
*MN      CLC   4(R3),C'C'                                                       
         CLI   4(R3),C'C'                                                       
         BNE   INIT0150                                                         
         GOTO1 =A(GETCOMBO),RR=RELO    YES-GET THE COMBINED STATIONS            
         B     INIT0150                                                         
         SPACE                                                                  
INITMVC  MVC   0(0,RE),0(R3)                                                    
         SPACE                                                                  
INIT0110 CLI   0(R2),QLAGY                                                      
         BNE   INIT0150                                                         
         SPACE                                                                  
         OC    0(8,R3),0(R3)       ANY FILTER?                                  
         BZ    INIT0150             NO                                          
         SPACE                                                                  
         ST    R0,FULL                                                          
         SPACE                                                                  
         MVC   QAGY,BLANKS                                                      
         LA    R0,6                                                             
         LA    RE,QAGY                                                          
         LR    RF,R3                                                            
INIT0112 CLI   0(RF),C'-'                                                       
         BE    INIT0114                                                         
         CLI   0(RF),C' '                                                       
         BNH   INIT0118                                                         
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,INIT0112                                                      
INIT0114 LA    RE,QAGY+4                                                        
         LA    RF,1(,RF)                                                        
         LA    R0,2                                                             
         SPACE                                                                  
INIT0116 CLI   0(RF),C' '                                                       
         BNH   INIT0118                                                         
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,INIT0116                                                      
         SPACE                                                                  
INIT0118 L     R0,FULL                                                          
         SPACE                                                                  
INIT0150 DS   0H                                                                
         LA    R3,9(,R3)                                                        
         SPACE                                                                  
INIT0160 DS   0H                                                                
         LA    R2,9(,R2)                                                        
         CLI   0(R2),0             END OF DATA TYPES                            
         BE    INIT0200                                                         
         BCT   R0,INIT0100                                                      
         SPACE                                                                  
* FOR SETS, NEED TO ACCESS FILTAB, AND SPREAD DATA AND TYPES *                  
* AND USE CODE BELOW                                                            
         SPACE                                                                  
INIT0200 DS   0H                                                                
         CLI   ELEM,0              CHECK FOR NO ROWS                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         CLI   ELEM+3,0            TOO MANY FIELDS REQUESTED?                   
         BNE   FLDCTER              YES, MORE THAN 3                            
*                                                                               
         BAS   RE,RSETRRGO         BACK TO RRGON FILE VALUES                    
*                                                                               
         XC    KEY,KEY             READ RRGON RECORD TYPE HEADER                
         LA    R7,KEY                                                           
         USING RORECD,R7                                                        
         MVC   ROKREP,RRGWREP                                                   
         MVC   ROKHD2RQ,RRGWDOLT   ALL/CONFIRMED/DIRECT/UNCONFIRMED             
         MVI   ROKHD2+1,X'02'                                                   
         MVC   ROKHD2TY,ELEM                                                    
         SPACE                                                                  
         BAS   RE,VALCOM           GO CHECK OUT AS IS                           
         BE    INIT0300                                                         
         CLI   ELEM+1,0            MORE THAN ONE DATA TYPE                      
         BE    NOENTERR             NO, BAD REQUEST                             
         CLI   ELEM+2,0            THREE ENTRIES                                
         BNE   INIT0260             YES                                         
         SPACE                                                                  
* SWAP ENTRIES                                                                  
         SPACE                                                                  
         MVC   BYTE,ROKHD2TY                                                    
         MVC   ROKHD2TY(1),ROKHD2TY+1                                           
         MVC   ROKHD2TY+1(1),BYTE                                               
         BAS   RE,VALCOM           GO CHECK OUT AS IS                           
         BE    INIT0300                                                         
         SPACE                                                                  
NOENTERR MVI   RRGWERR,RRGWER01    COMBO NOT IN TABLE                           
         B     ERREXIT                                                          
         SPACE                                                                  
VKPERERR MVI   RRGWERR,RRGWER09    REQUEST DATES NOT ON FILE                    
         B     ERREXIT                                                          
         SPACE                                                                  
FLDCTER  MVI   RRGWERR,RRGWER03    MORE THAN 3 FIELDS REQUESTED                 
         B     ERREXIT                                                          
         SPACE                                                                  
INIT0260 DS    0H                                                               
         MVC   FULL,ROKHD2TY           ABC                                      
         MVC   ROKHD2TY(1),FULL        A                                        
         MVC   ROKHD2TY+1(1),FULL+2    C                                        
         MVC   ROKHD2TY+2(1),FULL+1    B                                        
         BAS   RE,VALCOM           GO CHECK OUT                                 
         BE    INIT0300                                                         
         SPACE                                                                  
         MVC   ROKHD2TY(1),FULL+1      B                                        
         MVC   ROKHD2TY+1(1),FULL      A                                        
         MVC   ROKHD2TY+2(1),FULL+2    C                                        
         BAS   RE,VALCOM           GO CHECK OUT                                 
         BE    INIT0300                                                         
         SPACE                                                                  
         MVC   ROKHD2TY(1),FULL+1      B                                        
         MVC   ROKHD2TY+1(1),FULL+2    C                                        
         MVC   ROKHD2TY+2(1),FULL      A                                        
         BAS   RE,VALCOM           GO CHECK OUT                                 
         BE    INIT0300                                                         
         SPACE                                                                  
         MVC   ROKHD2TY(1),FULL+2      C                                        
         MVC   ROKHD2TY+1(1),FULL      A                                        
         MVC   ROKHD2TY+2(1),FULL+1    B                                        
         BAS   RE,VALCOM           GO CHECK OUT                                 
         BE    INIT0300                                                         
         SPACE                                                                  
         MVC   ROKHD2TY(1),FULL+2      C                                        
         MVC   ROKHD2TY+1(1),FULL+1    B                                        
         MVC   ROKHD2TY+2(1),FULL      A                                        
         BAS   RE,VALCOM           GO CHECK OUT                                 
         BNE   NOENTERR                                                         
         B     INIT0300                                                         
         SPACE                                                                  
* CHECK OUT LCOMBOS TABLE FOR VALID ENTRY                                       
         SPACE                                                                  
VALCOM   DS   0H                                                                
         L     R1,=A(LCOMBOS)      YES - FIND RECORD TYPE FOR LIST              
         A     R1,RELO                                                          
         MVI   BYTE,0                                                           
         USING LCOMBOSD,R1                                                      
*                                                                               
VALCOM20 DS   0H                                                                
         CLC   ROKHD2TY,LALTKEY    ENTRY EQUAL TO FILTERS ENTERED               
         BER   RE                                                               
*                                                                               
*        CLC   =X'070405',ROKHD2TY TEMP                                         
*        BNE   *+6                                                              
*        DC    H'0'                TEMP                                         
*        CLC   =X'070405',LALTKEY  TEMP                                         
*        BNE   *+6                                                              
*        DC    H'0'                TEMP                                         
         LA    R1,LCNEXT                                                        
         CLI   0(R1),X'FF'         END OF TABLE                                 
         BNE   VALCOM20             NOT YET                                     
*                                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
         DROP  R1                                                               
         SPACE                                                                  
INIT0300 DS   0H                                                                
         MVC   ELEM(3),ROKHD2TY                                                 
         MVC   SVKEY(L'ROKDTLTY),ELEM   SAVE THE KEY                            
         LA    R4,ELEM                                                          
         LA    R5,ELEM+8                                                        
         SPACE                                                                  
         OC    QGROUP,QGROUP                                                    
         BZ    INIT0310                                                         
         SPACE                                                                  
         CLI   RRGWSTSW,C'Y'       IS STATION ONE OF THE FLDS REQUESTED         
         BE    INIT0310                                                         
         SPACE                                                                  
         CLI   QGROUP,C'*'         SETS                                         
         BE    INIT0304             YES                                         
         CLI   QGROUP+1,0                                                       
         BNE   INIT0304                                                         
         SPACE                                                                  
         MVI   BYTE,QLGRGRP                                                     
         BAS   RE,FIND             GO FIND FIELD                                
         MVC   0(1,RF),QGROUP                                                   
         B     INIT0310                                                         
         SPACE                                                                  
FIND     LR    R0,RE                                                            
         LR    RE,R4                                                            
         LR    RF,R5                                                            
         LA    R1,3                                                             
FIND10   CLC   BYTE,0(RE)                                                       
         BE    FIND20                                                           
         LA    RE,1(,RE)                                                        
         LA    RF,8(,RF)                                                        
         BCT   R1,FIND10                                                        
         DC    H'0'                                                             
FIND20   LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
INIT0304 MVI   BYTE,QLGRP                                                       
         BAS   RE,FIND             GO FIND FIELD                                
         MVC   0(2,RF),QGROUP                                                   
         SPACE                                                                  
*                                                                               
INIT0310 OC    QSTA,QSTA                                                        
         BZ    INIT0320                                                         
         SPACE                                                                  
         MVI   BYTE,QLSTA                                                       
         BAS   RE,FIND             GO FIND FIELD                                
         MVC   0(7,RF),QSTA                                                     
         SPACE                                                                  
         CLI   NCOMBOS,0           LIST=COMBO                                   
         BE    INIT0320             NO                                          
*                                                                               
         MVC   0(7,RF),COMBOSTA    USE FIRST COMBO STATION                      
         XC    QSTA,QSTA                                                        
*                                                                               
INIT0320 OC    QREGION,QREGION                                                  
         BZ    INIT0330                                                         
         SPACE                                                                  
         CLI   RRGWOFSW,C'Y'       IS OFFICE ONE OF THE FLDS REQUESTED          
         BE    INIT0330                                                         
         SPACE                                                                  
         MVI   BYTE,QLREG                                                       
         BAS   RE,FIND             GO FIND FIELD                                
         MVC   0(2,RF),QREGION                                                  
*                                                                               
INIT0330 OC    QOFF,QOFF                                                        
         BZ    INIT0340                                                         
         SPACE                                                                  
         MVI   BYTE,QLOFF                                                       
         BAS   RE,FIND             GO FIND FIELD                                
         MVC   0(2,RF),QOFF                                                     
         SPACE                                                                  
INIT0340 OC    QTEAM,QTEAM                                                      
         BZ    INIT0344                                                         
         SPACE                                                                  
         MVI   BYTE,QLTEM                                                       
         BAS   RE,FIND             GO FIND FIELD                                
         MVC   0(2,RF),QTEAM                                                    
*                                                                               
INIT0344 OC    QADV,QADV                                                        
         BZ    INIT0346                                                         
         SPACE                                                                  
         MVI   BYTE,QLADV                                                       
         BAS   RE,FIND             GO FIND FIELD                                
         MVC   0(L'QADV,RF),QADV                                                
         SPACE                                                                  
INIT0346 OC    QAGY,QAGY                                                        
         BZ    INIT0350                                                         
         SPACE                                                                  
         MVI   BYTE,QLAGY                                                       
         BAS   RE,FIND             GO FIND FIELD                                
         MVC   0(L'QAGY,RF),QAGY                                                
         SPACE                                                                  
INIT0350 OC    QAFF,QAFF                                                        
         BZ    INIT0360                                                         
         SPACE                                                                  
         CLI   RRGWSTSW,C'Y'       IS STATION ONE OF THE FLDS REQUESTED         
         BE    INIT0360                                                         
         SPACE                                                                  
         MVI   BYTE,QLAFF                                                       
         BAS   RE,FIND             GO FIND FIELD                                
         MVC   0(3,RF),QAFF                                                     
*                                                                               
INIT0360 OC    QCLASS,QCLASS                                                    
         BZ    INIT0370                                                         
         SPACE                                                                  
         MVI   BYTE,QLCLS                                                       
         BAS   RE,FIND             GO FIND FIELD                                
         MVC   0(2,RF),QCLASS                                                   
*                                                                               
INIT0370 OC    QCTGY,QCTGY                                                      
         BZ    INIT0380                                                         
         SPACE                                                                  
         MVI   BYTE,QLCAT                                                       
         BAS   RE,FIND             GO FIND FIELD                                
         MVC   0(2,RF),QCTGY                                                    
*                                                                               
INIT0380 CLI   QSTATY,0                                                         
         BE    INIT0390                                                         
         SPACE                                                                  
         MVI   BYTE,QLSTY                                                       
         BAS   RE,FIND             GO FIND FIELD                                
         MVC   0(1,RF),QSTATY                                                   
*                                                                               
INIT0390 OC    QTVB,QTVB                                                        
         BZ    INIT0400                                                         
         SPACE                                                                  
         CLI   RRGWSTSW,C'Y'       IS STATION ONE OF THE FLDS REQUESTED         
         BE    INIT0400                                                         
         SPACE                                                                  
         MVI   BYTE,QLTVB                                                       
         BAS   RE,FIND             GO FIND FIELD                                
         MVC   0(2,RF),QTVB                                                     
*                                                                               
INIT0400 OC    QOWNER,QOWNER                                                    
         BZ    INIT0410                                                         
         SPACE                                                                  
         CLI   RRGWSTSW,C'Y'       IS STATION ONE OF THE FLDS REQUESTED         
         BE    INIT0410                                                         
         SPACE                                                                  
         MVI   BYTE,QLOWN                                                       
         BAS   RE,FIND             GO FIND FIELD                                
         MVC   0(3,RF),QOWNER                                                   
*                                                                               
INIT0410 OC    QCONTY,QCONTY                                                    
         BZ    INIT0420                                                         
         SPACE                                                                  
         OC    QCONTY,BLANKS                                                    
         SPACE                                                                  
         MVI   BYTE,QLCON                                                       
         BAS   RE,FIND             GO FIND FIELD                                
         MVC   0(L'QCONTY,RF),QCONTY                                            
         SPACE                                                                  
INIT0420 CLI   QRANK,0                                                          
         BE    INIT0430                                                         
         SPACE                                                                  
         CLI   RRGWSTSW,C'Y'       IS STATION ONE OF THE FLDS REQUESTED         
         BE    INIT0430                                                         
         SPACE                                                                  
         MVI   BYTE,QLRNK                                                       
         BAS   RE,FIND             GO FIND FIELD                                
         MVC   0(1,RF),QRANK                                                    
*                                                                               
INIT0430 OC    QMKT,QMKT                                                        
         BZ    INIT0434                                                         
         SPACE                                                                  
         MVI   BYTE,QLMKT                                                       
         BAS   RE,FIND             GO FIND FIELD                                
         MVC   0(4,RF),QMKT                                                     
*                                                                               
INIT0434 OC    QDCT,QDCT                                                        
         BZ    INIT0440                                                         
         SPACE                                                                  
         MVI   BYTE,QLDCT                                                       
         BAS   RE,FIND             GO FIND FIELD                                
         MVC   0(L'QDCT,RF),QDCT                                                
         SPACE                                                                  
INIT0440 DS   0H                                                                
         L     R7,AIO1                                                          
         MVC   KEYSAVE,KEY                                                      
         SPACE                                                                  
         GOTO1 RRGWDMGR,DMCB,=C'DMRDHI',=C'RRGNEW',KEY,(R7)                     
         SPACE                                                                  
         MVC   KEY,0(R7)                                                        
         MVI   TRCTYPE,01                                                       
         GOTO1 =A(TRACE),RR=RELO                                                
         CLC   ROKEY(ROKHD2ST-ROKEY),KEY                                        
         BNE   DATATYER                                                         
         SPACE                                                                  
         MVC   SVKEY(3),KEY+5                                                   
         MVI   RRGWYRSW,0                                                       
         SPACE                                                                  
         CLI   RRGWCNTL,C'V'       VALIDATE ONLY?                               
         BNE   *+14                                                             
         OC    RRGWSTDT,RRGWSTDT   ANY START DATE?                              
         BZ    INIT0504                                                         
*                                                                               
         CLC   RRGWSTDT,ROKHD2EN   START VS FILE END                            
         BH    VKPERERR            STARTS AFTER FILE ENDS = ERROR               
*                                                                               
         MVC   RRGWASDT,RRGWSTDT                                                
         MVC   RRGWAEDT,RRGWEDDT                                                
*                                                                               
         CLC   RRGWEDDT,ROKHD2ST       END VS FILE START                        
         BNL   INIT0504                                                         
*                                                                               
         MVC   DUB(2),RRGWSTDT                                                  
         MVI   DUB+2,15                                                         
         GOTO1 RRGWDATC,DMCB,(3,DUB),(0,WORK)                                   
         GOTO1 RRGWADAY,(R1),WORK,WORK+6,F'365'                                 
         GOTO1 RRGWDATC,(R1),(0,WORK+6),(3,FULL)                                
*                                  WITHIN FILE PERIOD                           
         CLC   FULL(2),ROKHD2EN     START VS FILE END                           
         BH    VKPERERR            STARTS AFTER FILE ENDS = ERROR               
         MVC   FULL+1(1),RRGWEDDT+1                                             
         CLC   FULL(2),ROKHD2ST     END VS FILE START                           
         BL    VKPERERR            ENDS BEFORE FILE STARTS = ERROR              
         SPACE                                                                  
         MVI   RRGWYRSW,C'P'       SET TO PRIOR YEAR                            
         MVC   RRGWASDT(1),FULL                                                 
         MVC   RRGWAEDT(1),FULL                                                 
         SPACE                                                                  
INIT0504 CLI   RRGWCNTL,C'V'       VALIDATE ONLY?                               
         BE    EXIT                                                             
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R7,KEY                                                           
         MVC   ROKREP,RRGWREP                                                   
         MVC   ROKDTLRQ,RRGWDOLT   ALL/CONFIRMED/DIRECT/UNCONFIRMED             
         MVC   ROKDTLTY(ROKDTLYM-ROKDTLTY),SVKEY                                
         SPACE                                                                  
         CLI   NCOMBOS,0           TEST COMBINED STATIONS                       
         BE    INIT0530             NO                                          
         SPACE                                                                  
         LA    RE,ROKDTLTY         YES-FIND DISPLACEMENT INTO KEY OF            
         LA    RF,ROKDTLVL             STATION VALUE                            
         LA    R0,L'ROKDTLTY                                                    
*                                                                               
INIT0520 CLI   0(RE),2                                                          
         BE    INIT0524                                                         
         LA    RE,1(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,INIT0520                                                      
*                                  WITHIN FILE PERIOD                           
         MVI   RRGWERR,RRGWER0A    REQUESTED COMBO STATIONS NOT ON FILE         
         B     ERREXIT                                                          
*                                  WITHIN FILE PERIOD                           
INIT0524 SR    RF,R7                                                            
         ST    RF,STADISP                                                       
         LA    R4,COMBOSTA         BRANCH THROUGH STATIONS UNTIL AN             
         ZIC   R5,NCOMBOS          RRGON RECORD IS FOUND                        
*                                                                               
INIT0530 DS    0H                                                               
         MVC   KEY+6(24),ELEM+8                                                 
         L     R7,AIO1                                                          
         MVC   KEYSAVE,KEY                                                      
         SPACE                                                                  
         GOTO1 RRGWDMGR,DMCB,=C'DMRDHI',=C'RRGNEW',KEY,(R7)                     
*                                  READ FIRST RRGON DETAIL RECORD               
         MVC   KEY,0(R7)                                                        
         SPACE                                                                  
         MVI   TRCTYPE,02                                                       
         GOTO1 =A(TRACE),RR=RELO                                                
         SPACE                                                                  
         CLC   ROKDTLTY,SVKEY      SEE IF RIGHT DATA TYPES                      
         BNE   LREC0260                                                         
*        BNE   VKRECERR                                                         
         SPACE                                                                  
         CLC   KEY(ROKDTLVL-ROKEY),ROKEY                                        
         BE    INIT0600                                                         
*                                                                               
         CLI   NCOMBOS,0           NOT FOUND - TEST COMBINED STATIONS           
         BE    LREC0260                                                         
*        BE    VKRECERR             NO-RECORD NOT FOUND                         
         SPACE                                                                  
         LA    R4,7(R4)            YES-TRY NEXT COMBINED STATION                
         BCT   R5,*+8                                                           
         B     LREC0260                                                         
*        B     VKRECERR                                                         
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   ROKREP,RRGWREP                                                   
         MVC   ROKDTLTY(ROKDTLYM-ROKDTLTY),SVKEY                                
         L     R1,STADISP                                                       
         LA    R1,KEY(R1)                                                       
         MVC   0(7,R1),0(R4)                                                    
         B     INIT0530                                                         
         SPACE                                                                  
VKRECERR MVI   RRGWERR,RRGWER0B    REQUESTED DATA TYPES NOT ON FILE             
         B     ERREXIT                                                          
*                                  WITHIN FILE PERIOD                           
         SPACE                                                                  
INITCLCB CLC   0(0,R1),BLANKS                                                   
INITCLCD CLC   0(0,R1),0(R6)                                                    
         SPACE                                                                  
INIT0600 DS   0H                                                                
         CLI   NCOMBOS,0           TEST COMBO STATION LIST                      
         BE    INIT0604                                                         
         STC   R5,NCOMBOS          YES-SAVE N'ACTIVE STATIONS                   
         ST    R4,ACOMBOS                   AND A(FIRST ACTIVE ONE)             
*                                                                               
INIT0604 DS   0H                                                                
         CLC   RRGWSTDT(1),RRGWEDDT TEST - PERIOD START/END YEARS SAME          
         BNE   INIT0630                                                         
         MVC   FULL(2),RRGWSTDT    YES - SET UP WEEKS PER MONTH                 
         MVI   FULL+2,15                                                        
         SPACE                                                                  
INIT0610 GOTO1 RRGWDATC,DMCB,(3,FULL),(0,WORK)                                  
         MVC   FULL(2),WORK        SAVE CURRENT YEAR IN FULL                    
         SPACE                                                                  
         CLI   FULL,X'F9'          THIS 2000 OR MORE                            
         BNH   *+8                                                              
         NI    FULL,X'FF'-X'02'                                                 
         SPACE                                                                  
         LA    R4,RRGWCWK          WEEKS/MONTH CURRENT YEAR                     
         SR    R0,R0                                                            
         SR    R6,R6                                                            
         ZAP   HALF,=P'1'          START FROM JANUARY                           
*                                                                               
INIT0620 OI    HALF+1,X'0F'                                                     
         UNPK  WORK+2(2),HALF                                                   
         GOTO1 =V(GETBROAD),DMCB,WORK,WORK+6,0,0,RR=RELO                        
         IC    R0,0(R1)            WEEKS PER BROADCAST MONTH                    
         STC   R0,0(R4)                                                         
         LA    R4,1(R4)                                                         
         AP    HALF,=P'1'          NEXT MONTH                                   
         CP    HALF,=P'12'         TEST DONE WHOLE YEAR                         
         BNH   INIT0620                                                         
         SPACE                                                                  
         CLI   WORK,X'F9'          THIS 2000 OR MORE                            
         BNH   *+8                                                              
         NI    WORK,X'FF'-X'02'                                                 
         SPACE                                                                  
         CLC   WORK(2),FULL        THIS PREV YEAR                               
         BNE   INIT0630                                                         
         PACK  DUB,WORK(2)         NOW DO PRIOR YEAR                            
         SP    DUB,=P'1'                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(2),DUB                                                      
         LA    R4,RRGWCWKY         WEEKS/MONTH PRIOR YEAR                       
         SR    R6,R6                                                            
         ZAP   HALF,=P'1'                                                       
         B     INIT0620                                                         
*                                                                               
INIT0630 DS   0H                                                                
         XC    KEY,KEY             SET FOR INIT                                 
         EJECT                                                                  
* READ ALL RECORDS REQUESTED                                                    
*                                                                               
LREC0010 DS   0H                                                                
         OC    KEY(L'ROKEY),KEY    TEST KEY IS ZERO                             
         BZ    LREC0020            YES-FIRST TIME THROUGH                       
         LA    R7,KEY              NO-PASSING RECS TO WRITER                    
         USING RORECD,R7                                                        
         MVI   ROKDTLYM+2,X'FF'    NEXT MONTH                                   
         SPACE                                                                  
LREC0014 MVC   KEYSAVE,KEY                                                      
         SPACE                                                                  
         GOTO1 RRGWDMGR,DMCB,=C'DMRDHI',=C'RRGNEW',KEY,AIO1                     
         SPACE                                                                  
         L     R1,AIO1                                                          
         MVC   KEY,0(R1)                                                        
         SPACE                                                                  
         MVI   TRCTYPE,03                                                       
         GOTO1 =A(TRACE),RR=RELO                                                
         SPACE                                                                  
         CLC   KEY(ROKDTLVL-ROKEY),KEYSAVE  TEST CORRECT RECORD TYPE            
         BNE   LREC0260                                                         
         CLC   RRGWLKEY,KEY+ROKDTLTY-ROKEY                                      
         BNE   LREC0260                                                         
         CLC   KEY(ROKDTLVL-ROKEY),KEYSAVE  TEST CORRECT RECORD TYPE            
         BNE   LREC0260                                                         
         SPACE                                                                  
         CLC   KEY(30),KEYSAVE     CK FOR CHANGE IN KEY                         
         BNE   LREC0110             SEE IF THIS ONE IS USABLE                   
         SPACE                                                                  
         CLC   ROKDTLYM,RRGWAEDT   CHECK REACHED END MONTH                      
         BNH   LT050                                                            
         LA    R7,KEY              GET NEXT RECORD PAST THIS KEY COMBO          
         MVI   ROKDTLYM,X'FF'                                                   
         SPACE                                                                  
         B     LREC0014                                                         
         SPACE                                                                  
* CLEAR SAVED CODE TABLE *                                                      
         SPACE                                                                  
LREC0020 DS   0H                                                                
         L     R1,=A(LCOMBOS)                                                   
         A     R1,RELO                                                          
         USING LCOMBOSD,R1                                                      
         SPACE                                                                  
LREC0030 CLI   0(R1),X'FF'         FIND THE KEY COMBINATION                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   LALTKEY,SVKEY       ENTRY EQUAL TO FILTERS ENTERED               
         BE    LREC0060                                                         
*                                                                               
LREC0050 DS   0H                                                                
         LA    R1,LCNEXT                                                        
         B     LREC0030                                                         
*                                                                               
LREC0060 DS   0H                                                                
         MVC   RRGWLKEY,SVKEY      SAVE LIST KEY                                
         DROP  R1                                                               
         SPACE                                                                  
         LA    R7,KEY              SET FIRST PART OF KEY                        
         USING RORECD,R7                                                        
         MVC   ROKREP,RRGWREP                                                   
         MVC   ROKDTLRQ,RRGWDOLT   ALL/CONFIRMED/DIRECT/UNCONFIRMED             
         MVC   ROKDTLTY,RRGWLKEY                                                
         MVC   ROKDTLVL(24),ELEM+8                                              
         SPACE                                                                  
LREC0100 DS   0H                                                                
         MVC   KEYSAVE,KEY                                                      
         SPACE                                                                  
         GOTO1 RRGWDMGR,DMCB,=C'DMRDHI',=C'RRGNEW',KEY,AIO1                     
         SPACE                                                                  
         L     R1,AIO1                                                          
         MVC   KEY,0(R1)                                                        
         SPACE                                                                  
         MVI   TRCTYPE,04                                                       
         GOTO1 =A(TRACE),RR=RELO                                                
*                                                                               
LREC0110 CLC   KEY(ROKDTLVL-ROKEY),KEYSAVE  TEST CORRECT RECORD TYPE            
         BNE   LREC0260                                                         
         CLC   RRGWLKEY,KEY+ROKDTLTY-ROKEY                                      
         BNE   LREC0260                                                         
         SPACE                                                                  
         L     R7,AIO              ADDRESS THE RECORD                           
*                                                                               
         LA    R0,3                CHECK RECORD AGAINST REQUEST FILTERS         
         LA    R2,ROKDTLTY                                                      
         LA    R3,ROKDTLVL                                                      
         LA    R4,RODATA-ROKDTLVL-1                                             
         LA    R5,RODATA-ROKDTLVL-9                                             
         LA    R6,KEY+ROKDTLVL-ROKEY                                            
*                                                                               
LREC0130 CLI   0(R2),0                                                          
         BE    LREC0210                                                         
         CLC   0(8,R3),BLANKS      YES-TEST SIGNIFICANT VALUE                   
         BNH   LREC0190            NO-SKIP                                      
         L     RF,=A(FILTAB)                                                    
         A     RF,RELO                                                          
*                                                                               
LREC0140 CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R2),0(RF)                                                    
         BE    LREC0142                                                         
         LA    RF,L'FILTAB(RF)                                                  
         B     LREC0140                                                         
         SPACE                                                                  
LREC0142 LH    R1,2(RF)                                                         
         LA    R1,SYSD(R1)                                                      
         ZIC   RE,1(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,LREX1            TEST FILTER IS SET (COMP TO SPACES)          
         BNH   LREC0160             NO                                          
         SPACE                                                                  
         EX    RE,LREX2            YES-TEST RECORD VALUE AGAINST FILTER         
         BE    LREC0160            EQUAL-OK                                     
         BH    LREC0150            HIGH                                         
         XC    0(8,R6),0(R6)       LOW-SKIP TO FILTER VALUE                     
         EX    RE,LREX3                                                         
         SPACE                                                                  
         EX    R5,LREX4            NEXT FIELD TO NULLS                          
         SPACE                                                                  
         B     LREC0100                                                         
*                                                                               
LREC0150 DS   0H                                                                
         EX    R4,LREX5            NO-SKIP                                      
         XC    ROKDTLYM-ROKEY+KEY,ROKDTLYM-ROKEY+KEY                            
         SPACE                                                                  
         B     LREC0100                                                         
         SPACE                                                                  
LREC0160 CLI   0(R2),QLOFF         YES-TEST ON OFFICE NOW                       
         BNE   LREC0170                                                         
         OC    0(2,R3),0(R3)       YES-TEST NULL OFFICE                         
         BZ    LREC0190            YES-SKIP                                     
         OC    QREGION,QREGION     TEST REGION FILTER                           
         BZ    LREC0200                                                         
         BAS   RE,CHKRGN           YES-CHECK OFFICE IN REGION                   
         BNE   LREC0190            NO-SKIP                                      
         B     LREC0200                                                         
*                                                                               
LREC0170 DS   0H                                                                
         CLI   0(R2),QLSTA         YES-TEST ON STATION NOW                      
         BNE   LREC0200                                                         
*                                                                               
         CLI   NCOMBOS,0                                                        
         BE    LREC0174                                                         
         LA    RE,COMBOSTA          ADDRESS OF COMBO STATION TABLE              
         ZIC   RF,NCOMBOS           N'STATIONS                                  
LREC0172 CLC   0(7,R3),0(RE)       THIS ONE OF THE ONES WANTED                  
         BE    LREC0176             YES                                         
         LA    RE,7(,RE)                                                        
         BCT   RF,LREC0172                                                      
         SPACE                                                                  
         MVC   KEYSAVE,KEY                                                      
         SPACE                                                                  
         GOTO1 RRGWDMGR,DMCB,=C'DMRSEQ',=C'RRGNEW',KEY,AIO1                     
         SPACE                                                                  
         L     R1,AIO1                                                          
         MVC   KEY,0(R1)                                                        
         SPACE                                                                  
         MVI   TRCTYPE,X'11'                                                    
         GOTO1 =A(TRACE),RR=RELO                                                
         B     LREC0110            NO-SKIP                                      
*                                                                               
LREC0174 DS    0H                                                               
         OC    QGROUP,QGROUP       THIS AN ACTIVE FILTER                        
         BZ    LREC0176                                                         
         SPACE                                                                  
         CLC   =C'PV',RRGWREP      THIS PETRY                                   
         BE    *+12                 YES                                         
         CLI   QGROUP+1,C' '       YES-TEST SUBGROUP FILTER                     
         NOP   LREC0176                                                         
*        BNH   LREC0176                                                         
         SPACE                                                                  
         BAS   RE,GETSTA                                                        
         BNE   ERREXIT                                                          
         SPACE                                                                  
         BAS   RE,CHKGRP           YES-CHECK STATION IN GROUP                   
         BNE   LREC0190            NO-SKIP                                      
         SPACE                                                                  
LREC0176 OC    QAFF,QAFF           TEST AFFILIATE FILTER                        
         BZ    *+12                                                             
         BAS   RE,AFFCHK           YES-CHECK STATION IN AFFILIATE               
         BNE   LREC0190                                                         
         SPACE                                                                  
         OC    QTVB,QTVB           TEST TVB FILTER                              
         BZ    *+12                                                             
         BAS   RE,TVBCHK           YES-CHECK STATION IN TVB REGION              
         BNE   LREC0190                                                         
         SPACE                                                                  
         CLI   QRANK,0             TEST MARKET RANK FILTER                      
         BE    *+12                                                             
         BAS   RE,RNKCHK           YES-CHECK STATION IN MKT RANK                
         BNE   LREC0190                                                         
         SPACE                                                                  
         OC    QOWNER,QOWNER       TEST OWNER FILTER                            
         BZ    *+12                                                             
         BAS   RE,OWNCHK           YES-CHECK STATION IN OWNERSHIP               
         BNE   LREC0190                                                         
         SPACE                                                                  
* GET ALL DATA FROM STATION MASTER                                              
         SPACE                                                                  
LREC0178 DS    0H                                                               
         BAS   RE,GETSTA           CK STA MASTER AVAIL                          
         BNE   ERREXIT                                                          
         SPACE                                                                  
         L     R4,AIO2                                                          
         SPACE                                                                  
         OC    RRGWSTAD,RRGWSTAD   WRITER PROVIDE AN ADDRESS?                   
         BZ    *+8                  NO                                          
         L     R4,RRGWSTAD         USE IT                                       
         USING RSTAD,R4                                                         
         B     LREC0200                                                         
*                                                                               
LREC0180 DS   0H                                                                
         CLI   0(R2),QLSTY         YES-TEST ON STATION TYPE NOW                 
         BNE   LREC0200                                                         
         CLI   0(R3),C'4'          YES-IGNORE 'ALL' STATION TYPES               
         BNE   LREC0200                                                         
*                                                                               
LREC0190 EX    R5,LREX6            SKIP - FORCE NEXT ROW VALUE                  
         SPACE                                                                  
         B     LREC0100                                                         
         SPACE 2                                                                
*                                                                               
LREX1    CLC   0(0,R1),BLANKS      EXECUTED INSTRUCTIONS                        
LREX2    CLC   0(0,R3),0(R1)                                                    
LREX3    MVC   0(0,R6),0(R1)                                                    
LREX4    XC    8(0,R6),8(R6)                                                    
LREX5    MVC   0(0,R6),XFF                                                      
LREX6    MVC   8(0,R6),XFF                                                      
*                                                                               
LREC0200 LA    R2,1(R2)            NEXT ROW TYPE                                
         LA    R3,8(R3)                                                         
         LR    R4,R5                                                            
         SH    R5,=H'8'                                                         
         LA    R6,8(R6)                                                         
         BCT   R0,LREC0130                                                      
*                                                                               
LREC0210 ZIC   R1,LSTDISP                                                       
         LA    R1,ROREC(R1)        R1=A(LIST LINE VALUE)                        
         MVC   LSTVAL,0(R1)        SAVE THE VALUE                               
         CLI   LSTTYPE,QLSTY       TEST LISTING STATION TYPES                   
         BNE   LREC0220                                                         
         SPACE                                                                  
         BAS   RE,DSTY             DISPLAY STATION TYPE                         
         SPACE                                                                  
*        ROUTINE TO PASS RECORDS TO NEW REP WRITER                              
         SPACE                                                                  
LREC0220 DS   0H                                                                
         MVC   SVKEY,KEY           SAVE KEY OF FIRST MONTH RECORD               
         SPACE                                                                  
         L     R7,AIO                                                           
         USING RORECD,R7                                                        
         LA    R6,RODPER           PERIOD COLS                                  
*                                                                               
LT010    CLC   ROKDTLYM(1),RRGWASDT CHECK THE YEAR                              
         BNL   LT020                                                            
         SPACE                                                                  
         MVC   KEYSAVE,KEY                                                      
         SPACE                                                                  
         GOTO1 RRGWDMGR,DMCB,=C'DMRSEQ',=C'RRGNEW',KEY,AIO1                     
         SPACE                                                                  
         L     R1,AIO1                                                          
         MVC   KEY,0(R1)                                                        
         SPACE                                                                  
         MVI   TRCTYPE,X'12'                                                    
         GOTO1 =A(TRACE),RR=RELO                                                
*        EX    R4,LTCOMPK                                                       
         CLC   KEY(30),KEYSAVE     CK FOR CHANGE IN KEY                         
         BNE   LT900                END OF DATA                                 
         B     LT010                                                            
*                                                                               
LT020    CLC   ROKDTLYM(1),RRGWASDT                                             
         BH    LT800               YEAR HIGH - NO DATA                          
         CLI   ROKDTLYM+1,0        CHECK FOR PRIOR MONTH                        
         BNE   LT040                                                            
*                                                                               
LT030    MVC   KEYSAVE,KEY                                                      
         SPACE                                                                  
         GOTO1 RRGWDMGR,DMCB,=C'DMRSEQ',=C'RRGNEW',KEY,AIO1                     
         SPACE                                                                  
         L     R1,AIO1                                                          
         MVC   KEY,0(R1)                                                        
         SPACE                                                                  
         MVI   TRCTYPE,X'13'                                                    
         GOTO1 =A(TRACE),RR=RELO                                                
*        EX    R4,LTCOMPK                                                       
         SPACE                                                                  
         CLC   KEY(30),KEYSAVE     CK FOR CHANGE IN KEY                         
         BNE   LT900                END OF DATA                                 
         B     LT020                                                            
*                                                                               
LT040    CLC   ROKDTLYM,RRGWASDT   CHECK MON AGAINST START MON                  
         BL    LT030               LOW - GET NEXT                               
         CLC   ROKDTLYM,RRGWAEDT   CHECK MON AGAINS END MON                     
         BH    LT800               HIGH - NO DATA                               
*                                                                               
LT050    L     R7,AIO                                                           
         MVC   RRGWRADR(4),AIO1       REC ADDR                                  
         SPACE                                                                  
         MVC   RRGWYRMO(2),ROKDTLYM   YEAR/MONTH                                
         CLI   RRGWYRSW,C'P'        THIS FOR PRIOR YEAR                         
         BNE   LT054                                                            
         ZIC   RE,RRGWYRMO                                                      
         BCTR  RE,0                                                             
         STC   RE,RRGWYRMO                                                      
         SPACE                                                                  
* DOLLAR AMTS                                                                   
         SPACE                                                                  
LT054    MVC   RRGWPRBL(4),RODPPBLG                                             
         MVC   RRGWCRBL(4),RODPCBLG                                             
         MVC   RRGWPFIN(4),RODPPFIN                                             
         MVC   RRGWCBUD(4),RODPCBUD                                             
         MVC   RRGWCBK(4),RODPPFIN                                              
         MVC   RRGWPBK(4),RODPPFIN                                              
         SPACE                                                                  
         CLI   RRGWYRSW,C'P'        THIS FOR PRIOR YEAR                         
         BNE   LT060                                                            
         SPACE                                                                  
         MVC   RRGWPRBL(4),RODP2BLG                                             
         MVC   RRGWCRBL(4),RODPPBLG                                             
         MVC   RRGWPFIN(4),RODP2FIN                                             
         MVC   RRGWCBUD(4),RODPPBUD                                             
         MVC   RRGWCBK(4),RODCWKBT                                              
         MVC   RRGWPBK(4),RODCWKBL                                              
         SPACE                                                                  
LT060    DS    0H                                                               
         OC    RRGWDFLD,RRGWDFLD   ANY DATA TO SEND?                            
         BZ    LT030                NO, NO DOLLARS, GET NEXT                    
         SPACE                                                                  
* COMPUTE PERCENTS                                                              
         SPACE                                                                  
         L     R4,RRGWPRBL                                                      
         L     R3,RRGWCRBL                                                      
         BAS   RE,PCTCOMP          GO GET PERCENT TO FINAL                      
         BAS   RE,PCTADJ           ADJUST FOR 4/5 WK PACING                     
         LTR   R5,R5               TEMP                                         
         BZ    *+6                                                              
         DC    X'0700'                                                          
         STH   R5,RRGWCPAC                                                      
         SPACE                                                                  
         L     R4,RRGWPFIN                                                      
         L     R3,RRGWCRBL                                                      
         BAS   RE,PCTCOMP          GO GET PERCENT TO FINAL                      
         STH   R5,RRGWPCTF                                                      
         SPACE                                                                  
         L     R4,RRGWCBUD                                                      
         L     R3,RRGWCRBL                                                      
         BAS   RE,PCTCOMP          GO GET PERCENT TO BUDGET                     
         STH   R5,RRGWPCTB                                                      
         SPACE                                                                  
         LA    R2,8                                                             
         LA    R3,RRGWDT1                                                       
         LA    R4,RRGWDAT1                                                      
LT070    LA    R0,8                                                             
         LA    RE,ROKDTLTY                                                      
         LA    RF,ROKDTLVL                                                      
         SPACE                                                                  
LT080    CLC   0(1,R3),0(RE)                                                    
         BE    LT100                                                            
         LA    RE,1(,RE)                                                        
         LA    RF,8(,RF)                                                        
         BCT   R0,LT080                                                         
         SPACE                                                                  
         CLI   0(R3),01            IS THIS BOGUS REP ENTRY                      
         BE    LT140                YES BYPASS                                  
         SPACE                                                                  
         CLI   0(R3),QLGRP                                                      
         BNE   LT082                                                            
         MVC   0(2,R4),QGROUP                                                   
         B     LT140                                                            
         SPACE                                                                  
LT082    CLI   0(R3),QLGRGRP                                                    
         BNE   LT090                                                            
         MVC   0(1,R4),QGROUP                                                   
         OI    1(R4),X'40'                                                      
         B     LT140                                                            
         SPACE                                                                  
LT090    CLI   0(R3),QLAFF                                                      
         BNE   LT094                                                            
         SPACE                                                                  
         MVC   1(L'QAFF,R3),SVAFF                                               
         B     LT140                                                            
         SPACE                                                                  
LT094    CLI   0(R3),QLRNK                                                      
         BNE   LT096                                                            
         SPACE                                                                  
         MVC   1(L'QRANK,R3),SVRNK                                              
         B     LT140                                                            
         SPACE                                                                  
LT096    CLI   0(R3),QLTVB                                                      
         BNE   LT098                                                            
         SPACE                                                                  
         MVC   1(L'QTVB,R3),SVTVB                                               
         B     LT140                                                            
         SPACE                                                                  
LT098    CLI   0(R3),QLOWN                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   1(L'QOWNER,R3),SVOWN                                             
         B     LT140                                                            
         SPACE                                                                  
LT100    CLI   0(R3),QLSTA                                                      
         BNE   LT120                                                            
         MVC   0(4,R4),0(RF)                                                    
         SPACE                                                                  
         LA    R1,5(,RF)                                                        
         CLI   3(R4),C'-'                                                       
         BNE   LT106                                                            
         BCTR  R1,0                                                             
         MVI   3(R4),C' '                                                       
         SPACE                                                                  
LT106    MVC   4(1,R4),0(R1)                                                    
         CLI   4(R4),C'T'                                                       
         BNE   *+8                                                              
         MVI   4(R4),C' '                                                       
         B     LT140                                                            
         SPACE                                                                  
LT120    MVC   0(8,R4),0(RF)                                                    
         SPACE                                                                  
LT140    LA    R3,9(,R3)                                                        
         LA    R4,8(,R4)                                                        
         CLI   0(R3),0                                                          
         BE    LT400                                                            
         BCT   R2,LT070                                                         
LT400    DS   0H                                                                
         B     EXIT                                                             
*                                                                               
*                                                                               
LT800    LA    R7,KEY              GET NEXT RECORD PAST THIS KEY COMBO          
         MVI   ROKDTLYM,X'FF'                                                   
         B     LREC0014                                                         
         SPACE                                                                  
*                                                                               
         SPACE                                                                  
LT900    DS   0H                                                                
         CLC   KEY(ROKDTLVL-ROKEY),KEYSAVE  TEST CORRECT RECORD TYPE            
         BE    LREC0100                      SAME, DO READ HIGH                 
         SPACE                                                                  
* AT END OF DATA REQUESTED, CLEAR 'TO' FIELDS, SET END                          
         SPACE                                                                  
LREC0260 DS   0H                                                                
         XC    RRGWDAT1(RRGWPCTB-RRGWDAT1),RRGWDAT1                             
         XC    RRGWYRMO,RRGWYRMO                                                
         XC    RRGWDFLD,RRGWDFLD                                                
         MVI   RRGWSTAT,X'FF'      SET END OF LIST                              
         MVI   RRGWERR,0                                                        
         XC    KEY,KEY                                                          
         XC    KEYSAVE,KEYSAVE                                                  
         B     EXIT                                                             
         EJECT                                                                  
CHKRGN   LR    R0,RE                                                            
         BAS   RE,SETREPFL         REPFILE VALUES                               
         XC    KEY,KEY                                                          
         MVI   KEY,4                                                            
         MVC   KEY+23(2),RRGWREP                                                
         MVC   KEY+25(2),0(R3)                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 RRGWDMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY                  
         GOTO1 RRGWDMGR,DMCB,=C'GETREC',=C'REPFIL',KEY+28,AIO,DMWORK            
         L     R1,AIO                                                           
         USING ROFFD,R1                                                         
         BAS   RE,RSETRRGO         BACK TO RRGON FILE VALUES                    
         CLC   ROFFREG,QREGION     CHECK THE OFFICE REGION                      
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R1                                                               
         SPACE 2                                                                
* CHANGE STATION TYPE FROM 1/2/3 TO READABLE                                    
         SPACE                                                                  
DSTY     MVC   LSTVAL,BLANKS       YES-CONVERT STATION TYPE CODE                
         MVC   LSTVAL(3),=C'NEW'                                                
         CLI   0(R1),C'2'                                                       
         BER   RE                                                               
         MVC   LSTVAL(3),=C'OLD'                                                
         CLI   0(R1),C'3'                                                       
         BER   RE                                                               
         MVC   LSTVAL(4),=C'COMP'                                               
         CLI   0(R1),C'1'                                                       
         BER   RE                                                               
         DC    H'0'                                                             
         EJECT                                                                  
* CHECK FILTERS BY READING THE STATION REC & COMPARING TO IT *                  
         SPACE                                                                  
CHKGRP   NTR1  ,                                                                
         L     R4,AIO2                                                          
         SPACE                                                                  
         OC    RRGWSTAD,RRGWSTAD   WRITER PROVIDE AN ADDRESS?                   
         BZ    *+8                  NO                                          
         L     R4,RRGWSTAD         USE IT                                       
         USING RSTAD,R4                                                         
         SPACE                                                                  
         CLI   QGROUP+1,C' '                                                    
         NOP   CHKGRP10            TEST - TEMP? 2/12/98                         
         SPACE                                                                  
         CLC   RSTAGRUP,QGROUP     CHECK THE STATION GROUP                      
         XIT1                                                                   
         SPACE                                                                  
CHKGRP10 CLC   RSTAGRUP(1),QGROUP  CHECK THE STATION GROUP                      
         XIT1                                                                   
         SPACE                                                                  
         DROP  R4                                                               
         SPACE                                                                  
AFFCHK   MVI   DUB+2,1             SET COMPARISON FLAG                          
         B     ALLCHK                                                           
         SPACE                                                                  
TVBCHK   MVI   DUB+2,2             SET COMPARISON FLAG                          
         B     ALLCHK                                                           
         SPACE                                                                  
OWNCHK   MVI   DUB+2,3             SET COMPARISON FLAG                          
         B     ALLCHK                                                           
         SPACE                                                                  
RNKCHK   MVI   DUB+2,4             SET COMPARISON FLAG                          
         SPACE                                                                  
ALLCHK   NTR1  ,                                                                
         SPACE                                                                  
         GOTO1 =A(GESTATS),RR=RELO                                              
         XIT1                                                                   
         EJECT                                                                  
* FNDSTA - FIRST FINDS STATION FIELD, THEN USES IT TO GET STATION REC           
         SPACE                                                                  
FNDSTA   NTR1                                                                   
         LA    R0,8                                                             
         LA    R3,RRGWDT1                                                       
FNDSTA10 CLI   0(R3),QLSTA                                                      
         BE    FNDSTA20                                                         
         LA    R3,9(,R3)                                                        
         BCT   R0,FNDSTA10                                                      
         DC    H'0'                                                             
FNDSTA20 BAS   RE,GETSTA                                                        
         SPACE                                                                  
         L     R6,AIO2                                                          
         SPACE                                                                  
         OC    RRGWSTAD,RRGWSTAD   WRITER PROVIDE AN ADDRESS?                   
         BZ    *+8                  NO                                          
         L     R6,RRGWSTAD         USE IT                                       
         SPACE                                                                  
         USING RSTAD,R6                                                         
*                                                                               
**  SAVE ALL VALUES                                                             
*                                                                               
         MVC   RRGWAFF,RSTAAFFL                                                 
         MVC   RRGWTVB,RSTATVB                                                  
         MVC   RRGWOWN,RSTAOWN                                                  
         MVC   RRGWRNK,RSTARANK                                                 
         XIT1                                                                   
         DROP  R6                                                               
         SPACE 2                                                                
* GET STATION MASTER FOR FILTERING OR GETTING DATA                              
         SPACE                                                                  
GETSTA   NTR1                      GET STATION RECORD                           
         BAS   RE,SETSTA                                                        
         SPACE                                                                  
         L     R1,AIO2                                                          
         SPACE                                                                  
         OC    RRGWSTAD,RRGWSTAD   WRITER PROVIDE AN ADDRESS?                   
         BZ    *+8                  NO                                          
         L     R1,RRGWSTAD         USE IT                                       
         SPACE                                                                  
         CLI   0(R1),2             TEST RECORD ALREADY IN CORE                  
         BNE   *+14                                                             
         CLC   STATION,22(R1)                                                   
         BE    GETSTAX             YES                                          
         BAS   RE,SETREPFL         REPFILE VALUES                               
         MVI   BYTE,0                                                           
*                                                                               
GETSTA1  XC    KEY,KEY             SET STATION RECORD PASSIVE KEY               
         LA    R4,KEY                                                           
         USING RSTAKEY,R4                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,RRGWREP                                                 
         MVC   RSTAKSTA,STATION                                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 RRGWDMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY                  
         SPACE                                                                  
GETSTA2  CLC   KEY(RST2KEND-RST2KEY),KEYSAVE                                    
         BE    GETSTA6                                                          
*        CLI   BYTE,0              TEST SECOND SEARCH                           
*        BNE   *+14                                                             
*        CLC   RRGWREP,=C'SJ'      OR AGENCY IS SJR (SJR MUST GET               
*        BNE   *+12                                  SJR RECORDS)               
*                                                                               
         MVI   RRGWERR,RRGWER0E    STATION MASTER NOT ON FILE                   
         LTR   RB,RB                                                            
         B     GETSTAX                                                          
*                                                                               
*        MVI   BYTE,1              NO-START SECOND SEARCH TO FIND               
*        B     GETSTA1                FIRST POINTER                             
*                                                                               
*ETSTA4  CLC   RST2KREP,RRGWREP    AGENCIES MATCH - FINE                        
*        BE    GETSTA6                                                          
*        CLI   BYTE,0              TEST FIRST SEARCH                            
*        BE    *+14                YES-TRY TO FIND AGENCY MATCH                 
*        CLC   RST2KREP,=C'SJ'     NO-SKIP SJR RECORDS                          
*        BNE   GETSTA6                                                          
         SPACE                                                                  
*        MVC   KEYSAVE,KEY                                                      
         SPACE                                                                  
*        GOTO1 RRGWDMGR,DMCB,=C'DMRSEQ',=C'REPDIR',KEYSAVE,KEYSAVE              
         SPACE                                                                  
*        L     R1,AIO1                                                          
*        MVC   KEY,0(R1)                                                        
         SPACE                                                                  
*        B     GETSTA2                                                          
         DROP  R4                                                               
*                                                                               
GETSTA6  DS   0H                                                                
         SPACE                                                                  
         L     R6,AIO2                                                          
         OC    RRGWSTAD,RRGWSTAD   WRITER PROVIDE AN ADDRESS?                   
         BZ    *+8                  NO                                          
         L     R6,RRGWSTAD         USE IT                                       
         GOTO1 RRGWDMGR,DMCB,=C'GETREC',=C'REPFIL',KEY+28,(R6),DMWORK           
         SPACE                                                                  
         USING RSTAREC,R6                                                       
         MVC   SVAFF,RSTAAFFL                                                   
         MVC   SVTVB,RSTATVB                                                    
         MVC   SVOWN,RSTAOWN                                                    
         MVC   SVGRP,RSTAGRUP                                                   
         MVC   SVRNK,RSTARANK                                                   
         SPACE                                                                  
         BAS   RE,RSETRRGO                                                      
         SPACE                                                                  
         L     R1,AIO1                                                          
         MVC   KEY,0(R1)                                                        
         SPACE                                                                  
GETSTAX  XIT1                                                                   
         DROP  R6                                                               
         SPACE                                                                  
SETSTA   MVC   STATION(4),0(R3)                                                 
         LA    RF,5(R3)                                                         
         CLI   STATION+3,C'-'                                                   
         BNE   *+10                                                             
         MVI   STATION+3,C' '                                                   
         BCTR  RF,0                                                             
         MVI   STATION+4,C' '                                                   
         CLI   0(RF),C'T'                                                       
         BE    *+10                                                             
         MVC   STATION+4(1),0(RF)                                               
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*        ROUTINE TO CALCULATE A PERCENTAGE                                      
*        REG 4 AS A PERCENT OF R3 - RESULT IN R5                                
*        CONDITION CODE SET TO NOT EQUAL IF PCT IS GT 999                       
*                                                                               
PCTCOMP  DS    0H                                                               
         SR    R5,R5                                                            
         LTR   R3,R3                                                            
         BZR   RE                                                               
         ST    R4,DUB              SAVE R4                                      
         CVD   R3,PARAS                                                         
         MP    PARAS(8),=P'10'                                                  
         CVD   R4,PARAS+8                                                       
         CP    PARAS(8),PARAS+8(8)                                              
         BNH   PC010               PCT GT 999                                   
         LA    R5,2000                                                          
         MR    R4,R4                                                            
         DR    R4,R3                                                            
         LTR   R5,R5                                                            
         BM    *+8                                                              
         A     R5,=F'1'                                                         
         SRA   R5,1                                                             
         L     R4,DUB              RESTORE R4                                   
         CR    RB,RB                                                            
         BR    RE                                                               
*                                                                               
PC010    LTR   RB,RB               CC NE                                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*        ROUTINE TO ADJUST A PERCENTAGE ACCORDING TO NUMBER OF                  
*        WEEKS IN PRIOR AND CURRENT MONTHS                                      
*        R5 CONTAINS THE PCT                                                    
*        CONDITION CODE SET TO NOT EQUAL IF PCT IS GT 999                       
*                                                                               
         DS    0F                                                               
PCTADJ   NTR1                                                                   
         CLC   RRGWASDT(1),RRGWAEDT TEST FOR SAME START/END YEARS               
         BE    *+6                    YES                                       
         DC    H'0'                NO - FORGET IT                               
         LA    RE,RRGWCWKY                                                      
         LA    RF,RRGWCWK                                                       
         SR    R4,R4                                                            
         ZIC   R1,RRGWYRMO+1       GET THE MONTH                                
         BCTR  R1,0                                                             
         ZIC   R6,0(R1,RE)         GET THE PRIOR WEEKS                          
         STC   R6,RRGWPRWK                                                      
         AR    R6,R6               X 2                                          
         MR    R4,R6               MULTIPLY BY PRIOR WEEKS                      
         ZIC   R6,0(R1,RF)         GET THE CURRENT WEEKS                        
         STC   R6,RRGWCRWK                                                      
         DR    R4,R6               DIVIDE BY CURRENT WEEKS                      
         A     R5,=F'1'                                                         
         SRA   R5,1                DIVIDE BACK BY 2                             
*                                                                               
         XIT1  REGS=(R5)           PASS BACK PCT IN R5                          
         EJECT                                                                  
*                                                                               
*        ROUTINE TO SET UP REP FILE VALUES                                      
*                                                                               
SETREPFL DS    0H                                                               
         MVC   FILENMSV,FILENAME   SAVE RRGON I/O VALUES                        
         MVC   SVAIO,AIO                                                        
         MVC   SVKEY2,KEY                                                       
         MVC   LKEYSV,LKEY                                                      
         MVC   USEIOSV,USEIO                                                    
         XC    FILENAME,FILENAME   SWITCH TO REP I/O VALUES                     
         MVC   AIO,AIO2                                                         
         MVC   LKEY,=H'27'                                                      
         MVI   USEIO,C'N'                                                       
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
*        ROUTINE TO RESET RRGON FILE VALUES                                     
*                                                                               
RSETRRGO DS    0H                                                               
         MVC   FILENAME,FILENMSV   SWITCH BACK TO RRGON I/O VALUES              
         MVC   AIO,SVAIO                                                        
         MVC   KEY,SVKEY2                                                       
         MVC   LKEY,LKEYSV                                                      
         MVC   USEIO,USEIOSV                                                    
         BR    RE                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
*                                                                               
         EJECT                                                                  
ROUND    DS    0H                                                               
*                                                                               
*        ROUTINE TO ROUND UNITS TO THOUSANDS                                    
*        INPUT:  R5 = UNITS                                                     
*        OUTPUT: R5 = ROUNDED TO THOUSANDS                                      
*                CC = 0 IF ROUNDED IS ZERO                                      
*                                                                               
         LR    R0,R5                                                            
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,=F'1000'                                                      
         LTR   R1,R1                                                            
         BM    *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         LTR   R5,R1                                                            
         BR    RE                                                               
         EJECT                                                                  
* PRINT RRGW DSECT IF OFFLINE                                                   
         SPACE                                                                  
PRTRRGW  NTR1                                                                   
         SPACE                                                                  
         CLI   RRGWCNTL,C'V'       VALIDATE ONLY?                               
         BE    PRTWX                                                            
         SPACE                                                                  
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         SPACE                                                                  
PRTW010  LA    R0,RRGWDEND-RRGWRI(,RA)                                          
         LA    R2,RRGWRI                                                        
         SPACE                                                                  
PRTW020  LA    R3,P                                                             
         LA    R4,P+80                                                          
         SPACE                                                                  
         LA    R5,2                                                             
PRTW030  LA    R6,4                                                             
         SPACE                                                                  
         MVC   0(16,R4),0(R2)                                                   
         SPACE                                                                  
PRTW040  GOTO1 RRGWHEXO,DMCB,(R2),(R3),4                                        
         LA    R2,4(,R2)                                                        
         LA    R3,9(,R3)                                                        
         BCT   R6,PRTW040                                                       
         LA    R3,1(,R3)                                                        
         LA    R4,17(,R4)                                                       
         BCT   R5,PRTW030                                                       
         SPACE                                                                  
         GOTO1 RRGWPRNT,DMCB,P-1,=C'BL01'  PRINT LINE                           
         SPACE                                                                  
         CR    R0,R2                                                            
         BH    PRTW020                                                          
PRTWX    DS    0H                                                               
         XIT1                                                                   
         DROP  R7                                                               
         SPACE                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
XFF      DC    XL24'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'           
*                                                                               
BLANKS   DC    CL80' '                                                          
*                                                                               
*                                                                               
         EJECT                                                                  
* TABLE OF REQUEST FILTERS                                                      
*                                                                               
*        DEFINITION                                                             
*   BYTE 0 - REQUEST TYPE - CHECKED AGAINST FIRST 3 BYTES OF KEY                
*        1 - LENGTH OF FILTER FIELD                                             
*        2 - DISPLACEMENT ADDRESS OF FILTER FIELD                               
*                                                                               
         DS    0F                                                               
FILTAB   DS    0XL4                                                             
         DC    AL1(QLSTA),AL1(L'QSTA),AL2(QSTA-SYSD)                            
         DC    AL1(QLREG),AL1(L'QREGION),AL2(QREGION-SYSD)                      
         DC    AL1(QLOFF),AL1(L'QOFF),AL2(QOFF-SYSD)                            
         DC    AL1(QLTEM),AL1(L'QTEAM),AL2(QTEAM-SYSD)                          
         DC    AL1(QLSAL),AL1(L'QSAL),AL2(QSAL-SYSD)                            
         DC    AL1(QLGRP),AL1(L'QGROUP),AL2(QGROUP-SYSD)                        
         DC    AL1(QLADV),AL1(L'QADV),AL2(QADV-SYSD)                            
         DC    AL1(QLAGY),AL1(L'QAGY),AL2(QAGY-SYSD)                            
         DC    AL1(QLAFF),AL1(L'QAFF),AL2(QAFF-SYSD)                            
         DC    AL1(QLCLS),AL1(L'QCLASS),AL2(QCLASS-SYSD)                        
         DC    AL1(QLCAT),AL1(L'QCTGY),AL2(QCTGY-SYSD)                          
         DC    AL1(QLSTY),AL1(L'QSTATY),AL2(QSTATY-SYSD)                        
         DC    AL1(QLTVB),AL1(L'QTVB),AL2(QTVB-SYSD)                            
         DC    AL1(QLOWN),AL1(L'QOWNER),AL2(QOWNER-SYSD)                        
         DC    AL1(QLGRGRP),AL1(1),AL2(QGROUP-SYSD)                             
         DC    AL1(QLRNK),AL1(L'QRANK),AL2(QRANK-SYSD)                          
         DC    AL1(QLCON),AL1(L'QCONTY),AL2(QCONTY-SYSD)                        
         DC    AL1(QLMKT),AL1(L'QMKT),AL2(QMKT-SYSD)                            
         DC    AL1(QLDCT),AL1(L'QDCT),AL2(QDCT-SYSD)                            
         DC    AL1(0)                                                           
         EJECT                                                                  
       ++INCLUDE RENRGLTBL                                                      
         EJECT                                                                  
* ROUTINE TO CHECK IF -CM COULD BE COMBO STATION                                
* IF SO, BUILD LIST OF COMBINED STATIONS                                        
*                                                                               
         SPACE                                                                  
         DS    0F                                                               
GETCOMBO NMOD1 0,**GTCO**                                                       
         L     RC,SVRC             GET A(GEND) GENCON WORK AREA                 
         USING GEND,RC                                                          
         LA    R1,COMBREPS         CHECK THE REP                                
*                                                                               
GETCOMB2 CLI   0(R1),0                                                          
         BE    GETCOMBX                                                         
         CLC   RRGWREP,0(R1)                                                    
         BE    *+12                                                             
         LA    R1,2(R1)                                                         
         B     GETCOMB2                                                         
         MVC   KEY,SVSTAKEY        READ STATION RECORD                          
         MVC   KEYSAVE,KEY                                                      
         GOTO1 RRGWDMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY                  
         GOTO1 RRGWDMGR,DMCB,=C'GETREC',=C'REPFIL',KEY+28,AIO,DMWORK            
         L     R4,AIO                                                           
         USING RSTAD,R4                                                         
         LA    R3,RSTAELEM         LOOK FOR COMBINED STATION ELEMENTS           
         SR    RE,RE                                                            
         LA    R1,COMBOSTA                                                      
         SR    R5,R5                                                            
         XC    COMBOSTA,COMBOSTA                                                
*                                                                               
GETCOMB4 CLI   0(R3),0                                                          
         BE    GETCOMB8                                                         
         CLI   0(R3),X'0A'                                                      
         BNE   GETCOMB6                                                         
         USING RSTACSEL,R3                                                      
         CLI   RSTACPRF,C'-'       STATION MINUS'D OUT?                         
         BE    GETCOMB6            YES - SKIP IT                                
         OC    COMBOSTA,COMBOSTA   FIRST STATION IS REQUESTED STATION           
         BNZ   *+18                                                             
         MVC   COMBOSTA,QSTA                                                    
         LA    R1,7(R1)                                                         
         LA    R5,1(R5)                                                         
         MVC   0(4,R1),RSTACS      FOUND-SAVE THE STATION                       
         LA    RF,4(R1)                                                         
         CLI   3(R1),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         MVI   6(R1),0                                                          
         MVC   0(3,RF),=C'-TV'                                                  
         CLI   RSTACS+4,C'T'                                                    
         BE    *+14                                                             
         MVC   1(1,RF),RSTACS+4                                                 
         MVI   2(RF),C'M'                                                       
         LA    R1,7(R1)                                                         
         LA    R5,1(R5)                                                         
*                                                                               
GETCOMB6 IC    RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     GETCOMB4                                                         
*                                                                               
GETCOMB8 STC   R5,NCOMBOS          SAVE N'COMBO STATIONS                        
         CLI   NCOMBOS,CSTAMAX                                                  
         BNH   *+6                                                              
         DC    H'0'                BOMB IF TOO MANY                             
         SPACE                                                                  
* GET QSORT ADDRESS                                                             
         SPACE                                                                  
         MVC   DMCB+4(4),=X'D9000A50'  QSORT IS T00A50                          
         GOTO1 RRGWCALL,DMCB                                                    
         ICM   RF,15,DMCB                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),DMCB,COMBOSTA,(R5),7,7,0                                    
*                                                                               
GETCOMBX XIT1                                                                   
         DROP  R3,R4,RB,RC                                                      
         SPACE                                                                  
COMBREPS DC    CL2'IR'             TABLE OF REPS THAT COULD HAVE                
         DC    CL2'TO'             COMBO STATIONS                               
         DC    CL2'D4'             NEW DARNY                                    
         DC    CL2'I1'                                                          
         DC    CL2'HN'                                                          
         DC    CL2'MG'                                                          
         DC    CL2'DI'                                                          
         DC    CL2'GP'                                                          
         DC    CL2'I2'                                                          
         DC    CL2'I8'                                                          
         DC    CL2'I9'                                                          
         DC    CL2'CN'                                                          
         DC    CL2'CM'                                                          
         DC    CL2'IF'             NEW INFINITY                                 
         DC    CL2'AQ'             NEW ALLIED RADIO PARTNERS                    
         DC    CL2'S1'             NEW SHAMROCK                                 
         DC    CL2'CM'             NEW CONCERT MUSIC                            
         DC    CL2'SJ'                                                          
* KATZ REPS ADDED                                                               
         DC    CL2'BF'                                                          
         DC    CL2'CR'                                                          
         DC    CL2'EA'                                                          
         DC    CL2'KF'                                                          
         DC    CL2'KU'                                                          
         DC    CL2'K4'                                                          
         DC    CL2'RS'                                                          
         DC    CL2'S3'                                                          
*                                                                               
         DC    X'00'                                                            
         LTORG                                                                  
**                                                                              
*   NEED TO SAVE VALUE IN FIRST PARAM AS POINTER TO COMPARE                     
*                                                                               
GESTATS  NMOD1 0,**GEST**                                                       
         L     RC,SVRC             GET A(GEND) GENCON WORK AREA                 
         USING GEND,RC                                                          
*                                  ONLY CLEAR 1ST 2 CHARS OF DUB                
*                                     FLAG IS IN 3RD POSITION                   
         XC    DUB(2),DUB          SET DUB = ZERO:  EQUAL                       
         MVC   FILENMSV,FILENAME   SAVE RRGON I/O VALUES                        
         MVC   SVAIO,AIO                                                        
         MVC   SVKEY2,KEY                                                       
         MVC   LKEYSV,LKEY                                                      
         MVC   USEIOSV,USEIO                                                    
         XC    FILENAME,FILENAME   SWITCH TO REP I/O VALUES                     
         MVC   AIO,AIO2                                                         
         MVC   LKEY,=H'27'                                                      
         MVI   USEIO,C'N'                                                       
         MVI   BYTE,0                                                           
*                                                                               
         SPACE                                                                  
         MVC   STATION(4),0(R3)                                                 
         LA    RF,5(R3)                                                         
         CLI   STATION+3,C'-'                                                   
         BNE   *+10                                                             
         MVI   STATION+3,C' '                                                   
         BCTR  RF,0                                                             
         MVI   STATION+4,C' '                                                   
         CLI   0(RF),C'T'                                                       
         BE    *+10                                                             
         MVC   STATION+4(1),0(RF)                                               
         SPACE                                                                  
         L     R1,AIO2                                                          
         SPACE                                                                  
         OC    RRGWSTAD,RRGWSTAD   WRITER PROVIDE AN ADDRESS?                   
         BZ    *+8                  NO                                          
         L     R1,RRGWSTAD         USE IT                                       
         SPACE                                                                  
         CLI   0(R1),2             TEST RECORD ALREADY IN CORE                  
         BNE   GEST0040                                                         
         CLC   STATION,22(R1)                                                   
         BE    GEST0160            YES                                          
*                                                                               
*   IF ALREADY IN CORE, BRANCH TO COMPARISON ROUTINE RATHER                     
*      THAN EXIT....                                                            
*                                                                               
GEST0040 DS   0H                                                                
         XC    KEY,KEY             SET STATION RECORD PASSIVE KEY               
         LA    R4,KEY                                                           
         USING RST2KEY,R4                                                       
         MVI   RST2KTYP,X'82'                                                   
         MVC   RST2KSTA,STATION                                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 RRGWDMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY                  
*                                                                               
GEST0060 CLC   KEY(RST2KEND-RST2KEY),KEYSAVE                                    
         BE    GEST0120                                                         
         CLI   BYTE,0              TEST SECOND SEARCH                           
         BNE   GEST0080                                                         
         CLC   RRGWREP,=C'SJ'      OR AGENCY IS SJR (SJR MUST GET               
         BNE   GEST0100                              SJR RECORDS)               
*                                                                               
GEST0080 DS   0H                                                                
         MVI   RRGWERR,RRGWER0E    STATION MASTER NOT ON FILE                   
         MVI   RRGWSTAT,01         SET ERROR                                    
         L     RD,BASERD                                                        
         XIT1                                                                   
*                                                                               
GEST0100 DS   0H                                                                
         MVI   BYTE,1              NO-START SECOND SEARCH TO FIND               
         B     GEST0040               FIRST POINTER                             
*                                                                               
GEST0120 DS   0H                                                                
         CLC   RST2KREP,RRGWREP    AGENCIES MATCH - FINE                        
         BE    GEST0150                                                         
         CLI   BYTE,0              TEST FIRST SEARCH                            
         BE    GEST0140            YES-TRY TO FIND AGENCY MATCH                 
         CLC   RST2KREP,=C'SJ'     NO-SKIP SJR RECORDS                          
         BNE   GEST0150                                                         
GEST0140 DS   0H                                                                
         L     R6,AIO2                                                          
         SPACE                                                                  
         OC    RRGWSTAD,RRGWSTAD   WRITER PROVIDE AN ADDRESS?                   
         BZ    *+8                  NO                                          
         L     R6,RRGWSTAD         USE IT                                       
         SPACE                                                                  
         GOTO1 RRGWDMGR,DMCB,=C'GETREC',=C'REPFIL',KEY+28,(R6),DMWORK           
         SPACE                                                                  
         B     GEST0060                                                         
*                                                                               
GEST0150 DS   0H                                                                
         GOTO1 RRGWDMGR,DMCB,=C'DMRSEQ',=C'REPDIR',KEYSAVE,KEYSAVE              
*                                                                               
         DROP  R4                                                               
*                                                                               
GEST0160 L     R5,AIO2                                                          
         SPACE                                                                  
         OC    RRGWSTAD,RRGWSTAD   WRITER PROVIDE AN ADDRESS?                   
         BZ    *+8                  NO                                          
         L     R5,RRGWSTAD         USE IT                                       
         SPACE                                                                  
         USING RSTAD,R5                                                         
*                                                                               
**  DO COMPARISON BASED ON VALUE PASSED IN...                                   
*                                                                               
         CLI   DUB+2,1             COMPARE AFFILIATE?                           
         BNE   GEST0162            NO                                           
         CLC   RSTAAFFL,QAFF       CHECK THE STATION'S AFFILIATE                
         BE    GEST0200            YES -                                        
         B     GEST0172            NO                                           
GEST0162 DS   0H                                                                
         CLI   DUB+2,2             COMPARE TVB REGION?                          
         BNE   GEST0164            NO                                           
         CLC   RSTATVB,QTVB        CHECK THE STATION'S TVB REGION               
         BE    GEST0200            YES -                                        
         B     GEST0172            NO                                           
GEST0164 DS   0H                                                                
         CLI   DUB+2,3             COMPARE OWNER?                               
         BNE   GEST0166            NO                                           
         CLC   RSTAOWN,QOWNER      SAME OWNER?                                  
         BE    GEST0200            YES -                                        
         B     GEST0172            NO                                           
         SPACE                                                                  
*                                  MUST BE RANK COMPARE!!                       
GEST0166 DS   0H                                                                
         CLC   RSTARANK,QRANK      CHECK THE STATION'S MARKET RANK              
         BE    GEST0200            YES -                                        
         DROP  R5                                                               
GEST0172 DS   0H                                                                
         CLC   =C'IR',RRGWREP      CROSS-COMPANY AGENCY?                        
         BNE   GEST0180            NO  - EXIT                                   
         GOTO1 RRGWDMGR,DMCB,=C'DMRSEQ',=C'REPDIR',KEYSAVE,KEYSAVE              
*                                  GET NEXT RECORD IN LINE                      
         CLC   KEY(RST2KEND-RST2KEY),KEYSAVE                                    
*                                  SAME STATION FOUND?                          
         BNE   GEST0180            NO  - TREAT AS NOT FOUND                     
         OC    KEY+RST2KEND-RST2KEY(3),KEY+RST2KEND-RST2KEY                     
*                                  IS THERE A LEAVE DATE?                       
         BZ    GEST0150            NO  - GO BACK AND COMPARE                    
*                                  YES - TREAT AS 'NOT FOUND'                   
GEST0180 DS   0H                                                                
         MVI   DUB,1               SET DUB NOT = ZERO:  NOT EQUAL               
GEST0200 DS   0H                                                                
         MVC   FILENAME,FILENMSV   SWITCH BACK TO RRGON I/O VALUES              
         MVC   AIO,SVAIO                                                        
         MVC   KEY,SVKEY2                                                       
         MVC   LKEY,LKEYSV                                                      
         MVC   USEIO,USEIOSV                                                    
         CLC   DUB(1),DUB+1        SET FINAL CONDITION CODE                     
         XIT1                                                                   
         DROP  RB,RC                                                            
         LTORG                                                                  
         EJECT                                                                  
* TRACE I/O                                                                     
         SPACE                                                                  
TRACE    DS   0H                                                                
         CLI   RRGWCNTL,C'V'       VALIDATE ONLY?                               
         BR    RE                                                               
TRAC     NMOD1 0,**TRAC**                                                       
         L     RC,SVRC             GET A(GEND) GENCON WORK AREA                 
         USING GEND,RC                                                          
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         MVI   P+1,C'S'                                                         
         CLI   TRCTYPE,16                                                       
         BH    *+8                                                              
         MVI   P+1,C'H'                                                         
         SPACE                                                                  
         LA    R0,TRCTABCT                                                      
         LA    R1,TRCTABL                                                       
TRC010   CLC   TRCTYPE,0(R1)                                                    
         BE    TRC020                                                           
         LA    R1,11(,R1)                                                       
         BCT   R0,TRC010                                                        
         DC    H'0'                                                             
         SPACE                                                                  
TRC020   MVC   P+3(2),1(R1)                                                     
         MVC   P+6(8),3(R1)                                                     
         SPACE                                                                  
         MVC   P+15(3),=C'KEY'                                                  
         MVC   P+23(3),KEY                                                      
         GOTO1 RRGWHEXO,DMCB,KEY+3,P+27,3,0,0                                   
         MVC   P+34(24),KEY+6                                                   
         GOTO1 RRGWHEXO,DMCB,KEY+30,P+59,2,0,0                                  
         SPACE                                                                  
         L     R4,AIO                                                           
         ICM   R0,15,RODPPBLG-ROKEY(R4)                                         
         EDIT  (R0),(10,P+64),0,COMMAS=YES,MINUS=YES                            
         SPACE                                                                  
         ICM   R0,15,RODPCBLG-ROKEY(R4)                                         
         EDIT  (R0),(10,P+75),0,COMMAS=YES,MINUS=YES                            
         SPACE                                                                  
         ICM   R0,15,RODPPFIN-ROKEY(R4)                                         
         EDIT  (R0),(10,P+86),0,COMMAS=YES,MINUS=YES                            
         SPACE                                                                  
         GOTO1 RRGWPRNT,DMCB,P-1,=C'BL01'  PRINT LINE                           
         SPACE                                                                  
         MVC   P+15(7),=C'KEYSAVE'                                              
         MVC   P+23(3),KEYSAVE                                                  
         GOTO1 RRGWHEXO,DMCB,KEYSAVE+3,P+27,3,0,0                               
         MVC   P+34(24),KEYSAVE+6                                               
         GOTO1 (RF),(R1),KEYSAVE+30,P+59,2,0,0                                  
         GOTO1 RRGWPRNT,(R1),P-1,=C'BL01'  PRINT LINE                           
TRACEX   XIT1                                                                   
         SPACE                                                                  
TRCTABL  DC    X'01',CL10'01INIT0440'                                           
         DC    X'02',CL10'02INIT0530'                                           
         DC    X'03',CL10'03LREC0014'                                           
         DC    X'04',CL10'04LREC0100'                                           
         DC    X'05',CL10'05AFT FSET'                                           
         DC    X'06',CL10'06LT200'                                              
         DC    X'07',CL10'07LT800'                                              
         DC    X'08',CL10'08LMON0030'                                           
         DC    X'09',CL10'09CSETM300'                                           
         DC    X'0A',CL10'0ADR020'                                              
         DC    X'0B',CL10'0BCSETD240'                                           
         DC    X'0C',CL10'0CLMON0254'                                           
         DC    X'11',CL10'11LREC0172'                                           
         DC    X'12',CL10'12LT010'                                              
         DC    X'13',CL10'13LT030'                                              
         DC    X'14',CL10'14LT050'                                              
         DC    X'15',CL10'15LMON0046'                                           
         DC    X'16',CL10'16LMON0160'                                           
         DC    X'17',CL10'17LMON0210'                                           
         DC    X'18',CL10'18XPLAN'                                              
         DC    X'19',CL10'19XPLAN'                                              
         DC    X'1A',CL10'1AXPLAN'                                              
         DC    X'1B',CL10'1BXPLAN'                                              
         DC    X'1C',CL10'1CXPLAN'                                              
TRCTABCT EQU   (*-TRCTABL)/9                                                    
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
* DSECT FOR LCOMBOS - LIST OF VALID FILTERS & KEY COMBINATIONS TABLE            
         SPACE                                                                  
LCOMBOSD DSECT                                                                  
LFTRFLDS DS    XL3                                                              
LSTTYP   DS    XL1                                                              
LFLAG    DS    XL1                                                              
LALTKEY  DS    XL3                                                              
LCNEXT   EQU   *                                                                
         SPACE 3                                                                
       ++INCLUDE RENWRNRG                                                       
         SPACE 3                                                                
RORECD   DSECT                                                                  
*                                                                               
       ++INCLUDE REGENRRGOC                                                     
         EJECT                                                                  
ROFFD    DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENOFF                                                       
         PRINT ON                                                               
         SPACE                                                                  
RSTAD    DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENSTA                                                       
         PRINT ON                                                               
RBUDD    DSECT                                                                  
       ++INCLUDE REGENBUD                                                       
         SPACE                                                                  
* DDTSARD                                                                       
         PRINT OFF                                                              
         SPACE                                                                  
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE FAUTL                                                          
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE RENWRNRGWK                                                     
         PRINT ON                                                               
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
*                                                                               
         ORG   APPLORG                                                          
         DS    0F                                                               
ACOMBOS  DS    A                                                                
STADISP  DS    F                                                                
SVREGS   DS    2F                                                               
SVR5R6   DS    2F                                                               
SVREGE   DS    F                                                                
RELO     DS    A                                                                
COMBOSTA DS    (CSTAMAX)CL7                                                     
CSTAMAX  EQU   8                                                                
NCOMBOS  DS    X                                                                
LISTEND  DS    C                                                                
LSTTYPE  DS    X                   TYPE DATA BEING LISTED (ADV/AGY/STA)         
LSTTYPLN DS    X                   LEN OF DATA LISTED                           
LSTKEY   DS    XL(L'ROKDTLTY)                                                   
LSTKEYEX DS    XL1                                                              
LSTDISP  DS    XL1                                                              
         SPACE                                                                  
SVTYPES  DS    CL5                 POSSIBLE TYPES ON THIS REPS FILE             
         SPACE                                                                  
TYPEKEY  DS    CL1                 NULL= ALL DOLLARS                            
*                                  C = CONFIRMED                                
*                                  U = UNCONFIRMED                              
*                                  D = DIRECT CONFIRMED                         
*                                  R = DIRECT                                   
LSTVAL   DS    CL8                                                              
         SPACE                                                                  
SVGRP    DS    CL2                                                              
SVAFF    DS    CL3                                                              
SVOWN    DS    CL3                                                              
SVTVB    DS    CL2                                                              
SVRNK    DS    CL1                                                              
SVREG    DS    CL1                                                              
STATION  DS    CL5                                                              
COMPBUDG DS    CL1                 COMPANY BUDGET FLAG                          
*                                  N  =  DON'T TRY TO RETRIEVE IT               
SETKEY   DS    XL3                 KEY OF FILTERS AND POSSIBLE L=               
*                                  WITH X'80' SET ON IF SETS                    
TRCTYPE  DS    XL1                                                              
FOUNDSET DS    CL1                                                              
FULLSTER DS    CL1                 Y IF FULL STEREO, N IF EMULATOR              
SETKEYAD DS    A                                                                
         SPACE                                                                  
         DS    CL37                SPARE                                        
         DS    CL1                                                              
P        DS    CL132                                                            
ENDSYSD  EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032RENWR10   10/24/11'                                      
         END                                                                    
