*          DATA SET ACREPXQT02 AT LEVEL 136 AS OF 10/17/00                      
*          DATA SET ACREPXQT02 AT LEVEL 142 AS OF 26/09/00                      
*PHASE ACQT02A                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE ACRECTYP                                                               
*INCLUDE HELEN                                                                  
         TITLE '- CURRENTLY IN USE BY FP TO SOLVE CURRENCY SIGNS'               
ACQT02   TITLE '- REMOVE BALANCE ELEMENTS FROM HIGH LVL ACCOUNTS'               
ACQT02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACQT**,R9,R8                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACCSTD,RC                                                        
                                                                                
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,LEDGFRST                                                    
         BE    PRLED                                                            
         CLI   MODE,PROCTRNS                                                    
         BE    PRTRNS                                                           
         CLI   MODE,PROCLEVC                                                    
         BE    PRLEVC                                                           
         CLI   MODE,PROCLEVB                                                    
         BE    PRLEVB                                                           
         CLI   MODE,PROCLEVA                                                    
         BE    PRLEVA                                                           
         CLI   MODE,REQLAST                                                     
         BE    RUNL                                                             
                                                                                
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* RUNFIRST MODE                                                       *         
***********************************************************************         
                                                                                
REQF     MVI   RCSUBPRG,0                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS LEDGER RECORD                                               *         
***********************************************************************         
                                                                                
PRLED    L     R2,ADLEDGER                                                      
         USING ACTRECD,R2                                                       
         MVC   P+2(L'ACTKCULA),ACTKCULA                                         
         UNPK  P+1(L'ACTKCPY),P+2(L'ACTKCPY)                                    
         MVZ   P+1(2),=C'00'       EG X'24' IS NOW C'24'                        
         CLI   P+1,X'F9'           BUT WHAT IF NOT NUMERIC?                     
         BNH   PRLED0                                                           
         GOTO1 CNVCO,P             CNVCO DEALS WITH A-F                         
PRLED0   CLI   P+2,X'F9'                                                        
         BNH   PRLED2                                                           
         GOTO1 CNVCO,P+1                                                        
PRLED2   GOTO1 ACREPORT                                                         
         SR    R0,R0                                                            
         LR    R3,R2                                                            
         AH    R3,DATADISP                                                      
         USING ACLELD,R3                                                        
PRLED4   CLI   ACLEL,ACLELQ                                                     
         BE    PRLED6                                                           
         CLI   ACLEL,0                                                          
         BE    XIT                                                              
         IC    R0,ACLLN                                                         
         AR    R3,R0                                                            
         B     PRLED4                                                           
*                                                                               
PRLED6   MVI   LEVMAX,C' '                                                      
         CLI   ACLVLEN,X'0C'                                                    
         BE    PRLEDX                                                           
         MVI   LEVMAX,C'A'                                                      
         MVC   LENMAX,ACLVLEN+0*L'ACLVALS                                       
         CLI   ACLVLEN+L'ACLVALS,X'0C'                                          
         BNL   PRLEDX                                                           
         MVI   LEVMAX,C'B'                                                      
         MVC   LENMAX,ACLVLEN+1*L'ACLVALS                                       
         CLI   ACLVLEN+2*L'ACLVALS,X'0C'                                        
         BNL   PRLEDX                                                           
         MVI   LEVMAX,C'C'                                                      
         MVC   LENMAX,ACLVLEN+2*L'ACLVALS                                       
PRLEDX   MVC   P+1(L'LEVMAX),LEVMAX                                             
         MVC   P+2(40),=C' IS THE LEVEL BEFORE A LOW-LEVEL ACCOUNT'             
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
LENMAX   DC    X'0C'                                                            
LEVMAX   DC    C' '                                                             
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCOUNT                                                     *         
***********************************************************************         
                                                                                
PRLEVA   CLI   LEVMAX,C'A'                                                      
         BL    XIT                                                              
         B     PRACC                                                            
                                                                                
PRLEVB   CLI   LEVMAX,C'B'                                                      
         BL    XIT                                                              
         MVI   P+62,C'B'           REPORT LEVEL                                 
         L     R2,ADHEIRB                                                       
         B     PRAC01                                                           
                                                                                
PRLEVC   CLI   LEVMAX,C'C'                                                      
         BL    XIT                                                              
         MVI   P+62,C'C'           REPORT LEVEL                                 
         L     R2,ADHEIRC                                                       
         B     PRAC01                                                           
PRACC    L     R2,ADHEIRA                                                       
         USING ACTRECD,R2                                                       
         MVI   P+62,C'A'           REPORT LEVEL                                 
PRAC01   LR    R3,R2                                                            
         AP    RECTOT,=P'1'                                                     
         AH    R3,DATADISP                                                      
         SR    R0,R0                                                            
         XC    RECFLAG,RECFLAG                                                  
         USING ABLELD,R3                                                        
PRAC02   CLI   ABLEL,0                                                          
         BE    PRAC10                                                           
         CLI   ABLEL,ABLELQ                                                     
         BE    PRAC06                                                           
         CLI   ABLEL,APOELQ                                                     
         BE    PRAC06                                                           
PRAC04   IC    R0,ABLLN                                                         
         AR    R3,R0                                                            
         B     PRAC02                                                           
*                                                                               
PRAC06   CLI   ABLEL,ABLELQ                                                     
         BNE   PRAC09                                                           
         ZAP   BALFWD,ABLFRWD                                                   
         ZAP   BALDR,ABLDR                                                      
         ZAP   BALCR,ABLCR                                                      
PRAC08   MVI   ABLEL,X'FF'                                                      
         MVI   RECFLAG,1                                                        
         B     PRAC04                                                           
         USING APOELD,R3                                                        
PRAC09   CP    APODR,=PL6'0'                                                    
         BE    *+10                                                             
         MVC   P+16(33),=C'PEELED OFF ELEMENT VALUE PRESENT**'                  
         CP    APOCR,=PL6'0'                                                    
         BE    *+10                                                             
         MVC   P+16(33),=C'PEELED OFF ELEMENT VALUE PRESENT**'                  
         B     PRAC08                                                           
         DROP  R3                                                               
*                                                                               
PRAC10   OC    RECFLAG,RECFLAG                                                  
         BZ    XIT                                                              
         AP    RECCHG,=P'1'                                                     
         GOTO1 VHELLO,DMCB,(C'D',ACCFIL),(X'FF',ACTRECD),0,0                    
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P+1(L'ACTKACT),ACTKACT                                           
* CHECK P+1 PLUS DISPLACEMENT HELD IN LENMAX IS A SPACE                         
         SR    R7,R7                                                            
         IC    R7,LENMAX                                                        
         LA    R6,P                                                             
         LA    R7,1(R6,R7)         P+1 +LENMAX-VALUE => R7                      
         CLI   0(R7),C' '          THIS CHAR MUST BE BLANK                      
         BE    PRAC11                                                           
         MVC   P+10(33),=C'**********ERROR*************ERROR'                   
         B     PRACX               ERROR RECORDS LEFT UNCHANGED                 
PRAC11   EDIT  BALFWD,(16,P+13),MINUS=YES,ZERO=NONBLANK                         
         EDIT  BALDR,(16,P+29),MINUS=YES,ZERO=NONBLANK                          
         EDIT  BALCR,(16,P+45),MINUS=YES,ZERO=NONBLANK                          
         CLC   P+13(16*3),SPACES   ZEROS PRINT NOTHING                          
         BNE   PRTRN               ANYTHING NON-ZERO & DONT WRITE BACK          
         B     PRAC13                                                           
PRAC12   MVC   P+1(L'ACTKACT),ACTKULA  INDENT WHEN TRANSACTION EXIST            
PRAC13   CLI   RCWRITE,C'N'                                                     
         BE    PRACX                                                            
         MVI   MODE,WRITLEVA                                                    
         CLI   P+62,C'B'                                                        
         BL    PRACX                                                            
         MVI   MODE,WRITLEVB                                                    
         BE    PRACX                                                            
         MVI   MODE,WRITLEVC                                                    
         B     PRACX                                                            
                                                                                
PRACX    GOTO1 ACREPORT                                                         
         B     XIT                                                              
                                                                                
         EJECT                                                                  
                                                                                
LOW      USING ACTRECD,R4                                                       
PRTRN    LA    R4,DKEY                                                          
         MVC   LOW.ACTKEY,ACTKEY   FIND LOW LEVEL ACCOUNT                       
         MVI   LOW.ACTKACT+L'ACTKACT-1,X'41'                                    
         MVC   P+64(L'ACTKACT),ACTKACT    PRINT CURRENT HIGH LEVEL              
         BAS   RE,HIGH                                                          
         LA    R4,DIR              POINT AT RECORD RETURED VIA DMGR             
         SR    R7,R7                                                            
         IC    R7,LENMAX                                                        
         BCTR  R7,0                                                             
         EX    R7,*+4                                                           
         CLC   DKEY(0),LOW.ACTKEY  CHECK LOW LEVEL IS SAME AS HIGH LVL          
         BE    PRTRN02                                                          
         MVC   P+74(18),=C'NO LOW LEVEL FOUND'  NO PRINT ERROR MESSAGE          
         B     PRACX                                                            
PRTRN02  MVC   P+74(2),=C'TO'      YES IT'S OK                                  
         MVC   P62SAVE,P+62                                                     
         MVC   P+78(L'ACTKACT),LOW.ACTKACT   SAVE LOW LEVEL FOR TRANS           
         MVC   LWLVLAC,LOW.ACTKACT                                              
         GOTO1 ACREPORT            PRINT DETAILS                                
         DROP  LOW                                                              
         USING TRNRECD,R4                                                       
PRTRN04  LA    R4,DKEY                                                          
         MVC   TRNKEY,ACTKEY                                                    
         BAS   RE,READ             REESTABLISH OLD HIGH LEVEL ACCOUNT           
         MVC   P+62(1),P62SAVE     NEDED ONLY WITH SPEEDY(BELOW) IN             
SPEEDY   B     PRAC13              FP *OUT TO MOVE NONZERO BALANCES             
PRTRN06  LA    R4,DKEY                                                          
         MVC   TRNKEY,DIR                                                       
         BAS   RE,SEQ              DO SEQUENTIAL READ TO GET TRANS              
         MVC   P+62(1),P62SAVE     ENSURE RESTORED BEFORE RETURNING             
         CLC   TRNKCULA,DIR        IS ACCOUNT CODE THE SAME                     
         BNE   PRAC12              NO - THEN WE'VE FINISHED                     
         LA    R4,DIR                                                           
         CLI   TRNKDATE,C' '       IS IT A TRANSACTION                          
         BNH   PRTRN06             NO GET NEXT RECORD                           
         L     R4,AIO2                                                          
         BAS   RE,GET              GET RECORDS FROM ACCMST                      
         CLI   TRNRFST,TRNELQ      IS THIS A TRANSACTION                        
         MVI   P+12,C'O'           OLD IS THE DEFAULT NOR R=RECENT              
         BNE   PRTRN06             NO GET NEXT RECORD                           
         USING TRNELD,R3                                                        
         LA    R3,TRNRFST                                                       
         CLI   TRNMOS,C'0'         1990 OR 2000                                 
         BNE   *+8                                                              
         CLI   TRNKDATE,X'A0'      ACCEPT Y2K                                   
         BNE   *+8                                                              
         MVI   P+12,C'R'                                                        
         CLI   TRNMOS,C'9'         1999                                         
         BNE   PRTRN08                                                          
         CLI   TRNMOS+1,C'D'       OCT TO DEC 1999                              
         BH    *+8                                                              
         MVI   P+12,C'R'                                                        
         CLI   TRNMOS+1,C'8'       AUG TO SEPTEMBER                             
         BL    *+8                                                              
         MVI   P+12,C'R'                                                        
PRTRN08  CLI   P+12,C'R'                                                        
                                                                                
         BNE   PRTRN10                                                          
         MVC   P+1(L'TRNKACT),TRNKACT                                           
         MVC   P+16(L'TRNMOS),TRNMOS                                            
         MVC   P+26(L'TRNBREF),TRNBREF                                          
         B     PRTRN12                                                          
PRTRN10  MVC   P+13(33),=C'LD TRANSACTION MOVED TO NEW LEVEL'                   
         L     R4,AIO2             CODE TO PUT WRITE AND THEN ADD               
         OI    TRNRSTA,TRNSDELT                                                 
         BAS   RE,PUT              DELETE WRONG LEVEL TRANSACTION               
         LA    R4,DIR                                                           
         OI    TRNKSTA,TRNSDELT                                                 
         BAS   RE,WRT              KEEP DIRECTORY IN STEP WITH PUT              
         MVC   TRNKACT,LWLVLAC     CHANGE KEY OF CURRENT TRANSACTION            
         LA    R7,0                                                             
         IC    R7,TRNKSBR                                                       
PRTRN11  BAS   RE,READ                                                          
         TM    8(R1),X'10'         TEST RECORD NOT FOUND                        
         BO    PRTRN11A            NOT FOUND MEANS CAN ADD                      
         LA    R7,1(R7)                                                         
         STC   R7,TRNKSBR                                                       
         B     PRTRN11                                                          
PRTRN11A L     R4,AIO2                                                          
         STC   R7,TRNKSBR                                                       
         MVC   TRNKACT,LWLVLAC     CHANGE KEY OF CURRENT TRANSACTION            
         NI    TRNRSTA,255-TRNSDELT REMOVE DELETION INDICATOR                   
         BAS   RE,ADD              AND WRITE IT BACK                            
                                                                                
PRTRN12  GOTO1 ACREPORT                                                         
         B     PRTRN06                                                          
         EJECT                                                                  
***********************************************************************         
* PROCESS SR TRANSACTION                                              *         
***********************************************************************         
                                                                                
                                                                                
PRTRNS  CLI    QOPT1,C'L'          QOPT1=L GIVES CURRENCY CHECK(LONG)           
        BL     XIT                                                              
        L      R3,ADTRANS                                                       
        LR     R2,R3                                                            
        SH     R2,DATADISP         SET R2 TO POINT TO RECORD                    
                                                                                
        SR     R0,R0                                                            
        USING  TRNELD,R3                                                        
PRTRNS2 CLI    TRNEL,0                                                          
        BE     PRTRNS8                                                          
        CLI    TRNEL,TRNELQ                                                     
        BE     PRTRNS4                                                          
        CLI    TRNEL,AFCELQ                                                     
        BE     PRTRNS6                                                          
PRTRNS3 IC     R0,TRNLN                                                         
        AR     R3,R0                                                            
        B      PRTRNS2                                                          
PRTRNS4 MVC    P+1(10),=C'ELEMENT 44'                                           
        CP     =PL6'0',TRNAMNT                                                  
        BNH    *+8                                                              
        MVI    P+11,C'-'                                                        
        B      PRTRNS3                                                          
        USING  AFCELD,R3                                                        
PRTRNS6 CP     =PL6'0',AFCAMNT                                                  
        BE     PRTRNSX             NOT INTERESTED IF AFCAMNT IS ZERO            
        BL     *+8                                                              
        MVI    P+14,C'-'           HIGH SO MARK AFCAMNT NEGATIVE                
        MVC    P+15(10),=C'NONZERO 7A'                                          
        B      PRTRNS3                                                          
PRTRNS8 CLC    P+15(10),=C'NONZERO 7A'                                          
        BNE    PRTRNSX                                                          
        CLC    P+11(1),P+14                                                     
        BE     PRTRNSX                                                          
        CLI    QOPT1,C'L'          QOPT1=L GIVES CURRENCY CHECK(LONG)           
        BE     PRTRNS9                                                          
        GOTO1  ACREPORT                                                         
        B      PRTRNSX             QOPT=S BYPASSES HEX DISPLAY                  
PRTRNS9 BAS    RE,PRTTRN                                                        
PRTRNSX MVC    P,SPACES                                                         
        B      XIT                                                              
        EJECT                                                                   
***********************************************************************         
* RUNLAST                                                             *         
***********************************************************************         
                                                                                
                                                                                
RUNL     MVC   P+1(27),=C'Number of Records Changed= '                          
         MVI   P+62,C' '                                                        
         EDIT  RECCHG,(16,P+28),MINUS=YES                                       
         GOTO1 ACREPORT                                                         
         MVC   P+1(27),=C'Number of HILEVEL records= '                          
         EDIT  RECTOT,(16,P+28),MINUS=YES                                       
         GOTO1 ACREPORT                                                         
         ZAP   RECCHG,=P'0'                                                     
         ZAP   RECTOT,=P'0'                                                     
RUNLX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SUBROUTINES          1) CONVERT COMPANIES WHICH ARE NON NUMERIC     *         
***********************************************************************         
                                                                                
CNVCO    UNPK  1(1,R1),1(1,R1)                                                  
         NI    1(R1),X'7F'         TURN OFF FIRST BIT                           
         SP    1(1,R1),=P'1'       SUBTRACT PACKED 1                            
         UNPK  1(1,R1),1(1,R1)                                                  
         BR    RE                                                               
***********************************************************************         
*                      2) PRINT ELEMENTS 44 AND 7A                    *         
***********************************************************************         
                                                                                
PRTTRN   NTR1  ,                                                                
         MVC   P,SPACES                                                         
         LR    R4,R2                                                            
         LH    R0,DATADISP                                                      
         GOTO1 HEXPRT,DMCB,KLIT,(R0),TRNRECD                                    
         LR    R3,R2                                                            
         AH    R3,DATADISP                                                      
         USING TRNELD,R3                                                        
         SR    R0,R0                                                            
PRTT02   IC    R0,TRNLN                                                         
         CLI   TRNEL,TRNELQ                                                     
         BE    *+8                                                              
         CLI   TRNEL,AFCELQ                                                     
         BNE   PRTT04                                                           
         GOTO1 HEXPRT,DMCB,ELIT,(R0),(R3)                                       
PRTT04   AR    R3,R0                                                            
         CLI   TRNEL,0                                                          
         BNE   PRTT02                                                           
         MVI   P,C'-'                                                           
         MVC   P+1(L'P-1),P                                                     
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT CHARACTER AND HEX FORMAT                           *         
*                                                                     *         
* P1     -     A(8 CHARACTER NAME)                                    *         
* P2     -     WIDTH OF THE DATA                                      *         
* P3     -     A(DATA)                                                *         
***********************************************************************         
                                                                                
HEXPRT   NTR1  ,                   PRINTING ROUTINE                             
         LM    R2,R4,0(R1)                                                      
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         MVC   P+1(L'KLIT),0(R2)                                                
*                                                                               
HEXPRT02 LR    R2,R3                                                            
         CH    R2,=Y(MAXWIDTH)                                                  
         BNH   *+8                                                              
         LH    R2,=Y(MAXWIDTH)     R2=WIDTH OF PRINTING THIS LINE               
         BCTR  R2,0                                                             
         EX    R2,*+4                                                           
         MVC   P+20(0),0(R4)       MOVE DATA TO PRINT LINE                      
         EX    R2,*+4                                                           
         TR    P+20(0),CHRTAB      TRANSLATE TO PRINTABLE                       
         GOTO1 ACREPORT                                                         
         LA    R2,1(R2)                                                         
         GOTO1 VHEXOUT,DMCB,(R4),HEXWORK,(R2),SEP                               
         BCTR  R2,0                                                             
         EX    R2,*+4                                                           
         MVC   P+20(0),HEXWORK     MOVE ZONES HALF TO PLINE                     
         GOTO1 ACREPORT                                                         
         LA    RF,HEXWORK+1(R2)                                                 
         EX    R2,*+4                                                           
         MVC   P+20(0),0(RF)       MOVE NUMERIC HALF TO PLINE                   
         GOTO1 ACREPORT                                                         
         LA    R2,1(R2)                                                         
         AR    R4,R2               POINT TO NEXT INPUT CHUNK                    
         SR    R3,R2               DECREMENT DATA WIDTH                         
         BP    HEXPRT02                                                         
HEXPRTX  B     EXIT                                                             
                                                                                
HEXWORK  DS    CL(MAXWIDTH*2)                                                   
MAXWIDTH EQU   60                                                               
KLIT     DC    CL8'KEY'                                                         
ELIT     DC    CL8'ELEMENT'                                                     
***********************************************************************         
* GENERAL EXIT                                                        *         
***********************************************************************         
         SPACE 1                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
         SPACE 1                                                                
HIGH     LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
READ     LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
SEQ      LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
WRT      LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,DIR,DIR                                
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
GET      LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,(R4),DMWORK2                       
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
ADD      LR    R0,RE                                                            
         XC    DA,DA                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,DA,(R4),DMWORK2                       
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
PUT      LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DA,(R4),DMWORK2                       
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
CHRTAB   DC    (12*16)C'.'             00-BF    TRANSLATE TABLE FOR             
         ORG   CHRTAB+X'40'                     CHARACTER DISPLAY               
         DC    C' '                                                             
         ORG                                                                    
         DC    CL16'.ABCDEFGHI......'  C0-CF                                    
         DC    CL16'.JKLMNOPQR......'  D0-DF                                    
         DC    CL16'..STUVWXYZ......'  E0-EF                                    
         DC    CL16'0123456789......'  F0-FF                                    
         SPACE 2                                                                
         LTORG                                                                  
VHEXOUT  DC    V(HEXOUT)                                                        
VCASHVAL DC    V(CASHVAL)                                                       
SEP      DC    C'SEP'                                                           
VHELLO   DC    V(HELLO)                                                         
AIO1     DC    A(IO1)                                                           
AIO2     DC    A(IO2)                                                           
ACCOUNT  DC    C'ACCOUNT'                                                       
ACCDIR   DC    C'ACCDIR '                                                       
ACCFIL   DC    C'ACCFIL '                                                       
ACCMST   DC    C'ACCMST '                                                       
GETREC   DC    C'GETREC '                                                       
PUTREC   DC    C'PUTREC '                                                       
ADDREC   DC    C'ADDREC '                                                       
*                                                                               
BALFWD   DC    PL8'0'                                                           
BALDR    DC    PL8'0'                                                           
BALCR    DC    PL8'0'                                                           
RECCHG   DC    PL8'0'                                                           
RECTOT   DC    PL8'0'                                                           
DMWORK2  DC    XL96'00'                                                         
IO1      DC    (MAXRLNQ)X'00'                                                   
MAXRLNQ  EQU   1990                INSTEAD OF 2000 TO BE SAFE                   
*                                                                               
         DS    0D                                                               
IO2      DC    (MAXRLNQ)X'00'                                                   
*                                                                               
ACCSTD   DSECT                                                                  
         DS    0F                                                               
DKEY     DS    CL(L'ACCKEY)                                                     
DACC     EQU   DKEY+3                                                           
DIR      DS    CL64                                                             
DIRKACT  EQU   DIR+3                                                            
DIRKDATE EQU   DIR+32                                                           
DA       DS    F                                                                
RECFLAG  DS    XL1                                                              
P62SAVE  DS    C                                                                
LWLVLAC  DS    CL(L'ACTKACT)                                                    
         SPACE 2                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACRCVRECD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRCVRECD                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'136ACREPXQT0210/17/00'                                      
         END                                                                    
