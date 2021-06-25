*          DATA SET APGHM2AGAA AT LEVEL 012 AS OF 05/01/02                      
*PHASE ACHM2AGA,+0                                                              
         TITLE 'BELL ATLANTIC RECONCILLIATION'                                  
ACHM2AGA CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8,RR=R5                                              
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         ST    R5,HKRELO                                                        
                                                                                
*---------------------------------------------------------------------*         
*        FIRST TIME IN HOOK FOR THIS REQUEST - SET UP CLIENT LIST     *         
*        AND BUILD TABLE OF ANALYSIS POINTERS AND NAMES               *         
*                                                                     *         
*        FIRST BUILD CLIENT LIST FROM LIST CODE PASSED IN QSELECT     *         
*---------------------------------------------------------------------*         
HOOKSORT DS    0H                                                               
                                                                                
         CLI   HOOKSW,C'Y'            HAVE I BEEN HERE BEFORE                   
         BE    HK100                                                            
         MVI   HOOKSW,C'Y'            SET BEEN HERE BEFORE SWITCH               
                                                                                
*        LA    R2,CLITAB              CLIENT TABLE                              
*        MVI   0(R2),X'FF'            MARK END OF TABLE                         
*                                                                               
*        CLC   QSELECT,SPACES         WAS LIST RECORD ENTERED AT REQ            
*        BE    HK030                                                            
*        CLI   QSELECT,C'-'                                                     
*        BE    HK010                                                            
*        CLI   QSELECT,C'+'                                                     
*        BNE   HK030                                                            
*                                                                               
*        USING LSTRECD,R3                                                       
*K010    L     R3,AHOOKIO             READ LIST RECORD                          
*        MVC   0(42,R3),SPACES                                                  
*        MVI   LSTKTYP,LSTKTYPQ                                                 
*        MVC   LSTKCPY,QCOMPANY                                                 
*        MVC   LSTKLST,QSELECT+1                                                
*        GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',AHOOKIO,AHOOKIO                  
*        TM    DMCB,X'10'                                                       
*        BO    HK030                                                            
*                                                                               
*        L     R4,AHOOKIO             FIND CLIENT LIST ELEMENT                  
*        AH    R4,DATADISP                                                      
*K015    CLI   0(R4),0                                                          
*        BE    HK030                                                            
*        CLI   0(R4),LIDELQ                                                     
*        BE    HK020                                                            
*K017    ZIC   R0,1(R4)                                                         
*        AR    R4,R0                                                            
*        B     HK015                                                            
*                                                                               
*        USING LIDELD,R4                                                        
*K020    CLC   LIDDLEDG,=C'SJ'        WILL BE SET UP ON SJ                      
*        BNE   HK017                                                            
*        LA    R2,CLITAB              FILL TABLE FROM CLIENT LIST               
*        LA    R6,LIDDACCS                                                      
*        ZIC   R5,LIDLN                                                         
*        SH    R5,=Y(LIDDACCS-LIDEL)                                            
*K025    MVC   0(3,R2),0(R6)                                                    
*        LA    R2,3(R2)                                                         
*        LA    R6,3(R6)                                                         
*        BCTR  R5,0                                                             
*        BCTR  R5,0                                                             
*        BCT   R5,HK025                                                         
*        MVI   0(R2),X'FF'                                                      
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        SET UP TABLE OF ANALYSIS CODES AND NAMES                     *         
*---------------------------------------------------------------------*         
HK030    DS    0H                                                               
         USING ANLTABD,R2                                                       
         LA    R2,ANLTAB              MARK INITIAL END OF TABLE                 
         MVI   0(R2),X'FF'                                                      
                                                                                
         USING ACTRECD,R3                                                       
         LA    R3,MYKEY               BUILD 14 KEY AND READ FIRST               
         MVC   0(42,R3),SPACES        ACCOUNT                                   
         MVC   ACTKCPY,QCOMPANY                                                 
         MVC   ACTKUNT(2),=C'14'                                                
         MVI   ACTKACT,X'41'                                                    
                                                                                
HK032    GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',MYKEY,AHOOKIO                    
         L     R3,AHOOKIO                                                       
         CLC   ACTKEY(3),MYKEY        SAME C/U/L                                
         BNE   HK100                  IF NOT HAVE READ ALL 14'S                 
         MVC   MYKEY,ACTKEY           SAVE KEY OF REC JUST READ                 
                                                                                
         L     R4,AHOOKIO                                                       
         AH    R4,DATADISP                                                      
HK035    CLI   0(R4),0                GET NAME ELEMENT                          
         BE    HK050                                                            
         CLI   0(R4),NAMELQ                                                     
         BE    HK040                                                            
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     HK035                                                            
                                                                                
         USING NAMELD,R4                                                        
HK040    ZIC   R5,NAMLN               SET LENGTH OF NAME TEXT                   
         SH    R5,=Y(NAMEREC-NAMEL)                                             
         BCTR  R5,0                                                             
HK045    MVC   ANLCODE,ACTKACT        SAVE 14 ACCOUNT CODE AND                  
         MVC   ANLNAME,SPACES         NAME IN TABLE                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   ANLNAME(0),NAMEREC                                               
         LA    R2,ANLENTL(R2)         BUMP PAST THIS TABLE ENTRY                
         MVI   0(R2),X'FF'            MARK NEW END OF TABLE                     
                                                                                
HK050    MVI   MYKEY+4,X'FF'          SET KEY FOR NEXT READHI                   
         B     HK032                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        PROCESS SORT RECORD - ALTER RECORD PRIOR TO SORT             *         
*---------------------------------------------------------------------*         
HK100    DS    0H                                                               
         CLI   HOOKNUM,1              MODE=PUTHOOK                              
         BNE   HK200                                                            
                                                                                
         USING HOOK1D,R7                                                        
         L     R7,HOOKAREC            ADDR OF SORT RECORD                       
                                                                                
         CLI   QOPT6,C'1'             DUMP RECORD BEFORE (MODE 1)               
         BNE   HK110                                                            
         LA    R0,H1LEN                                                         
         GOTO1 PRNTBL,DMCB,(5,=C'REC1B'),(R7),C'DUMP',(R0),=C'2D',     X        
               (C'P',PRINT)                                                     
                                                                                
HK110    DS    0H                                                               
         USING REPTABD,RE                                                       
         LA    RE,REPTAB              FIND REPORT NUMBER IN TABLE TO            
HK115    CLI   RPTCODE,X'FF'          GET DISPLACEMENTS                         
         BE    HK190                                                            
         CLC   H1REPNO,RPTCODE        MATCH ON REPORT NUMBER                    
         BE    HK130                                                            
         LA    RE,REPTENT(RE)                                                   
         B     HK115                                                            
                                                                                
HK130    DS    0H                                                               
         SR    R3,R3                                                            
         ICM   R3,3,CGCODE            DISP INTO SORT REC TO CLIENT CODE         
         AR    R3,R7                  ADD TO ADDRESS OF SORT REC                
         SR    R4,R4                                                            
         ICM   R4,3,CGNAME            DISP INTO SORT REC TO CLIENT NAME         
         AR    R4,R7                  ADD TO ADDRESS OF SORT REC                
                                                                                
         USING ANLTABD,R2             SET COST GROUP NAME FROM TABLE            
         LA    R2,ANLTAB                                                        
HK135    CLI   ANLCODE,X'FF'                                                    
         BE    HK190                                                            
         CLC   ANLCODE,0(R3)          R3=COST GROUP CODE                        
         BE    HK140                                                            
         LA    R2,ANLENTL(R2)                                                   
         B     HK135                                                            
HK140    MVC   0(36,R4),ANLNAME                                                 
                                                                                
HK190    CLI   QOPT7,C'1'             DUMP RECORD AFTER (MODE 1)                
         BNE   XIT                                                              
         LA    R0,H1LEN                                                         
         GOTO1 PRNTBL,DMCB,(5,=C'REC1A'),(R7),C'DUMP',(R0),=C'2D',     X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        PROCESS SORT RECORD                                          *         
*---------------------------------------------------------------------*         
HK200    DS    0H                                                               
         CLI   HOOKNUM,2              MODE=SORTHOOK                             
         BNE   HK300                                                            
                                                                                
         USING HOOK2D,R7                                                        
         L     R7,HOOKAREC            ADDR OF SORT RECORD                       
                                                                                
         CLI   QOPT6,C'2'             DUMP RECORD BEFORE (MODE 2)               
         BNE   HK210                                                            
         LA    R0,H2LEN                                                         
         GOTO1 PRNTBL,DMCB,(5,=C'REC2B'),(R7),C'DUMP',(R0),=C'2D',     X        
               (C'P',PRINT)                                                     
                                                                                
HK210    DS    0H                                                               
         USING REPTABD,RE                                                       
         LA    RE,REPTAB              FIND REPORT NUMBER IN TABLE TO            
HK215    CLI   RPTCODE,X'FF'          GET DISPLACEMENTS                         
         BE    HK240                                                            
         CLC   H2REPNO,RPTCODE        MATCH ON REPORT NUMBER                    
         BE    HK220                                                            
         LA    RE,REPTENT(RE)                                                   
         B     HK215                                                            
                                                                                
HK220    DS    0H                                                               
*        CLC   QSELECT,SPACES         IF NO LIST RECORD SPECIFIED THEN          
*        BE    HK230                  NO CLIENT FILTERING                       
                                                                                
         SR    R3,R3                                                            
         ICM   R3,3,CLIDISP           DISP INTO SORT REC TO CLIENT CODE         
         AR    R3,R7                  ADD TO ADDRESS OF SORT REC                
         LA    R2,CLITAB              IS THIS CLIENT IN TABLE                   
HK225    CLI   0(R2),X'FF'                                                      
         BE    XITNO                                                            
         CLC   0(3,R2),0(R3)                                                    
         BE    HK230                                                            
         LA    R2,3(R2)                                                         
         B     HK225                                                            
                                                                                
HK230    DS    0H                                                               
         SR    R3,R3                                                            
         ICM   R3,3,PERNAME           DISP IN SORT REC TO PERSON NAME           
         AR    R3,R7                  ADD TO ADDRESS OF SORT REC                
         MVC   35(1,R3),CURRFLT1      KEEP FILTER 1 IN LAST POSITION            
**TEST                                                                          
         L     RE,ADLVCNAM                                                      
         MVC   33(2,R3),2(RE)                                                   
**TEST                                                                          
         CLI   H2REPNO,1              TO COLLAPSE WHEN SORTING - SO             
         BE    HK237                  I AM CLEARING OUT THE ACCOUNT             
         CLI   H2REPNO,3               LEVEL CODES BUT IDENTICAL                
         BE    HK237                   NAMES WILL COLLAPSE)                     
         CLI   H2REPNO,5              TO COLLAPSE WHEN SORTING - SO             
         BE    HK237                  I AM CLEARING OUT THE ACCOUNT             
         CLI   H2REPNO,7               LEVEL CODES BUT IDENTICAL                
         BE    HK237                   NAMES WILL COLLAPSE)                     
         CLI   H2REPNO,2              CODES IN ORDER TO DO THIS                 
         BE    HK238                  (ACCOUNTS WITH DIFFERENT LOW              
         CLI   H2REPNO,4                                                        
         BE    HK238                                                            
         CLI   H2REPNO,6              CODES IN ORDER TO DO THIS                 
         BE    HK238                  (ACCOUNTS WITH DIFFERENT LOW              
         CLI   H2REPNO,8                                                        
         BE    HK238                                                            
         B     HK240                                                            
HK237    MVC   H2CODE2,SPACES                                                   
HK238    MVC   H2CODE1,SPACES                                                   
                                                                                
HK240    DS    0H                                                               
         CLI   CURRFLT1,C'F'                                                    
         BNE   HK250                                                            
         ZAP   DIVIDEND,H2COL5                                                  
         PACK  DIVISOR,QEND+2(2)                                                
         DP    DIVIDEND,DIVISOR                                                 
         ZAP   WORK(11),QUOTENT                                                 
         MP    WORK(11),=P'75002'                                               
         SRP   WORK(11),61,5                                                    
         ZAP   H2COL7,WORK+3(8)                                                 
         B     HK280                                                            
                                                                                
HK250    DS    0H                                                               
         CLI   CURRFLT1,C'P'                                                    
         BNE   HK260                                                            
         ZAP   WORK(10),H2COL5                                                  
         MP    WORK(10),=P'625'                                                 
         SRP   WORK(10),62,5                                                    
         ZAP   H2COL7,WORK+2(8)                                                 
         B     HK280                                                            
                                                                                
HK260    DS    0H                                                               
         CLI   CURRFLT1,C'D'                                                    
         BE    HK262                                                            
         CLI   CURRFLT1,C' '                                                    
         BNE   HK270                                                            
HK262    CP    H2COL6,=P'0'                                                     
         BNE   HK265                                                            
         ZAP   H2COL7,=P'0'                                                     
         B     HK280                                                            
HK265    ZAP   DIVIDEND,H2COL5                                                  
         MP    DIVIDEND,=P'1000000'                                             
         ZAP   DIVISOR,H2COL6                                                   
         DP    DIVIDEND,DIVISOR                                                 
         ZAP   H2COL7,QUOTENT                                                   
         SRP   H2COL7,0,5                                                       
         B     HK280                                                            
                                                                                
HK270    DS    0H                                                               
         CLI   CURRFLT1,C'T'                                                    
         BNE   HK290                                                            
         CP    H2COL6,=P'0'                                                     
         BNE   HK272                                                            
         ZAP   H2COL7,=P'0'                                                     
         B     HK280                                                            
HK272    ZAP   DIVIDEND,H2COL5                                                  
         MP    DIVIDEND,=P'1000000'                                             
         ZAP   DIVISOR,H2COL6                                                   
         DP    DIVIDEND,DIVISOR                                                 
         ZAP   WORK(11),QUOTENT                                                 
         PACK  HALF,QEND+2(2)                                                   
         MP    WORK(11),HALF                                                    
         ZAP   DIVIDEND,WORK(11)                                                
         PACK  MNTHWRKD,QEND+2(2)                                               
         BAS   RE,CALCMNTH                                                      
         ZAP   DIVISOR,MNTHWRKD                                                 
         DP    DIVIDEND,DIVISOR                                                 
         ZAP   H2COL7,QUOTENT                                                   
         SRP   H2COL7,0,5                                                       
                                                                                
HK280    DS    0H                                                               
**       ZAP   WORK(16),H2COL8                                                  
**       MP    WORK(16),H2COL7                                                  
**       SRP   WORK(16),58,5                                                    
**       ZAP   H2COL8,WORK+8(8)                                                 
**                                                                              
**       ZAP   H2COL12,H2COL7                                                   
**       ZAP   H2COL14,H2COL8                                                   
                                                                                
HK290    CLI   QOPT7,C'2'             DUMP RECORD AFTER (MODE 2)                
         BNE   XIT                                                              
         LA    R0,H2LEN                                                         
         GOTO1 PRNTBL,DMCB,(5,=C'REC2A'),(R7),C'DUMP',(R0),=C'2D',     X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        PROCESS SORT RECORD                                          *         
*---------------------------------------------------------------------*         
HK300    DS    0H                                                               
         CLI   HOOKNUM,3              MODE = SORTOUT                            
         BNE   XIT                                                              
                                                                                
         USING HOOK3D,R7                                                        
         L     R7,HOOKAREC            ADDR OF SORT RECORD                       
                                                                                
         CLI   QOPT6,C'3'             DUMP RECORD BEFORE (MODE 3)               
         BNE   HK305                                                            
         LA    R0,H3LEN                                                         
         GOTO1 PRNTBL,DMCB,(5,=C'REC3B'),(R7),C'DUMP',(R0),=C'2D',     X        
               (C'P',PRINT)                                                     
                                                                                
HK305    DS    0H                                                               
                                                                                
         USING REPTABD,RE                                                       
         LA    RE,REPTAB              FIND THIS REPORT NUMBER TO GET            
HK320    CLI   RPTCODE,X'FF'          DISPLACEMENTS                             
         BE    HK380                                                            
         CLC   H3REPNO,RPTCODE        MATCH ON REPORT NUMBER                    
         BE    HK325                                                            
         LA    RE,REPTENT(RE)                                                   
         B     HK320                                                            
                                                                                
HK325    SR    R3,R3                                                            
         ICM   R3,3,PERNAME           DISP TO PERSON NAME                       
         AR    R3,R7                  ADD TO ADDRESS OF SORT REC                
                                                                                
         MVC   BYTE,35(R3)            SWAP FILTER AND PERSON NAME               
         CLI   BYTE,C' '              SO FILTER WILL PRECEDE PERSON             
         BNE   *+8                    NAME ON REPORT                            
         MVI   BYTE,X'41'             IF BLANK WILL KEEP ALIGNMENT              
         MVC   HALF,33(R3)                                                      
         MVC   WORK(31),0(R3)                                                   
         MVC   0(36,R3),SPACES                                                  
         MVC   0(1,R3),BYTE           SHIFT FILTER                              
         MVC   2(31,R3),WORK          SHIFT NAME                                
         MVC   34(2,R3),HALF          SHIFT NAME                                
         LA    R4,2(R3)                                                         
         LA    R5,34                                                            
HK370    CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         OI    0(R4),X'01'                                                      
         LA    R4,1(R4)                                                         
         BCT   R5,HK370                                                         
                                                                                
HK380    DS    0H                                                               
         OI    H3COL8+7,X'08'                                                   
         CP    H3COL8,=P'0'                                                     
         BNE   HK385                                                            
         ZAP   H3COL8,=P'0'                                                     
         ZAP   H3COL12,=P'0'                                                    
         ZAP   H3COL14,=P'0'                                                    
         B     HK390                                                            
                                                                                
HK385    ZAP   WORK(16),H3COL8                                                  
         MP    WORK(16),H3COL7                                                  
         SRP   WORK(16),58,5                                                    
         ZAP   H3COL8,WORK+8(8)                                                 
                                                                                
         ZAP   H3COL12,H3COL7                                                   
         ZAP   H3COL14,H3COL8                                                   
*MN                                                                             
         DC    H'0'                                                             
*MN                                                                             
                                                                                
HK390    CLI   QOPT7,C'3'             DUMP RECORD AFTER (MODE 3)                
         BNE   XIT                                                              
         LA    R0,H3LEN                                                         
         GOTO1 PRNTBL,DMCB,(5,=C'REC3A'),(R7),C'DUMP',(R0),=C'2D',     X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        IF PERSON HAS F1=T - MEANS THEY'RE TERMINATED SO I NEED                
*        TO CALC NUMBER OF MONTHS WORKED WITHIN THIS REQUEST PERIOD             
*-------------------------------------------------------------------*           
CALCMNTH NTR1                                                                   
                                                                                
         MVC   WORK(6),QSTART           INITIALIZE DATES WORKED TO              
         CLC   WORK+4(2),SPACES         REPORT START AND END                    
         BNE   *+10                                                             
         MVC   WORK+4(2),=C'01'                                                 
                                                                                
         MVC   WORK+6(6),QEND                                                   
         CLC   WORK+10(2),SPACES                                                
         BNE   CALC20                                                           
         MVC   WORK+10(2),=C'01'        GET LAST DAY OF MONTH                   
         GOTO1 DATCON,DMCB,(X'30',WORK+6),(0,WORK+6),(1,0)                      
                                                                                
CALC20   CLC   CURRHIRE(4),WORK         IS HIRE DATE > REPORT START             
         BNH   CALC25                                                           
         MVC   WORK(6),CURRHIRE         THEN USE HIRE DATE                      
CALC25   CLI   CURRTERM,X'FF'                                                   
         BE    CALCXIT                                                          
         CLC   CURRTERM(6),WORK+6       IS TERM DATE < REPORT END               
         BH    CALCXIT                                                          
         MVC   WORK+6(6),CURRTERM       THEN USE TERM DATE                      
                                                                                
CALC30   DS    0H                                                               
         GOTO1 PERVERT,DMCB,WORK,WORK+6      GET # OF MONTHS WORKED             
         SR    R3,R3                                                            
         LA    R2,DMCB+14                                                       
         ICM   R3,3,0(R2)                                                       
         CVD   R3,DUB                                                           
         ZAP   MNTHWRKD,DUB             STORE PACKED FOR CALCULATION            
                                                                                
CALCXIT  B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PROGRAM EXITS                                                          
*--------------------------------------------------------------------*          
XIT      SR    RC,RC                    KEEP RECORD                             
XITNO    LTR   RC,RC                    DROP RECORD                             
         XIT1                                                                   
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        DEFINE CONSTANTS                                                       
*--------------------------------------------------------------------*          
AHOOKIO  DC    A(HOOKIO)                                                        
HKRELO   DS    F                                                                
HOOKSW   DC    C'N'                     BEEN HERE BEFORE?                       
MYKEY    DC    CL42' '                  SAVE KEY FOR COMPARE                    
MNTHWRKD DC    PL2'0'                   # MONTHS WORKED                         
                                                                                
         DS    0D                                                               
DIVISOR  DS    D                        DIVISION WORK AREA                      
DIVIDEND DS    0PL16                                                            
QUOTENT  DS    D                                                                
REMAIND  DS    D                                                                
                                                                                
*----------------------------------------------------------------*              
*        TABLE DSECT                                                            
*        AL1 - REPORT NUMBER                                                    
*        AL2 - DISP TO CLIENT CODE                                              
*        AL2 - DISP TO PERSON NAME                                              
*        AL2 - DISP TO COST GROUP CODE                                          
*        AL2 - DISP TO COST GROUP NAME                                          
*        AL2 - DISP TO 1C LEVEL 3                                               
*        AL2 - DISP TO 1C LEVEL 4 (OR ZERO IF N/A)                              
*----------------------------------------------------------------*              
REPTAB   DS    0F                                                               
         DC    AL1(1),AL2(H2CODE1-HOOK2D),AL2(H2NSEQ4-HOOK2D)                   
         DC    AL2(H1CODE3-HOOK1D),AL2(H1NAME3-HOOK1D)                          
         DC    AL2(H2CODE1-HOOK2D),AL2(H2CODE2-HOOK2D)                          
REPTENT  EQU   *-REPTAB                                                         
                                                                                
         DC    AL1(2),AL2(H2CODE1-HOOK2D),AL2(H2NSEQ3-HOOK2D)                   
         DC    AL2(H1CODE2-HOOK1D),AL2(H1NAME2-HOOK1D)                          
         DC    AL2(H2CODE1-HOOK2D),AL2(0)                                       
                                                                                
         DC    AL1(3),AL2(H2CODE1-HOOK2D),AL2(H2NSEQ4-HOOK2D)                   
         DC    AL2(H1CODE3-HOOK1D),AL2(H1NAME3-HOOK1D)                          
         DC    AL2(H2CODE1-HOOK2D),AL2(H2CODE2-HOOK2D)                          
                                                                                
         DC    AL1(4),AL2(H2CODE1-HOOK2D),AL2(H2NSEQ3-HOOK2D)                   
         DC    AL2(H1CODE2-HOOK1D),AL2(H1NAME2-HOOK1D)                          
         DC    AL2(H2CODE1-HOOK2D),AL2(0)                                       
                                                                                
         DC    AL1(5),AL2(H2CODE1-HOOK2D),AL2(H2NSEQ4-HOOK2D)                   
         DC    AL2(H1CODE3-HOOK1D),AL2(H1NAME3-HOOK1D)                          
         DC    AL2(H2CODE1-HOOK2D),AL2(H2CODE2-HOOK2D)                          
                                                                                
         DC    AL1(6),AL2(H2CODE1-HOOK2D),AL2(H2NSEQ3-HOOK2D)                   
         DC    AL2(H1CODE2-HOOK1D),AL2(H1NAME2-HOOK1D)                          
         DC    AL2(H2CODE1-HOOK2D),AL2(0)                                       
                                                                                
         DC    AL1(7),AL2(H2CODE1-HOOK2D),AL2(H2NSEQ4-HOOK2D)                   
         DC    AL2(H1CODE3-HOOK1D),AL2(H1NAME3-HOOK1D)                          
         DC    AL2(H2CODE1-HOOK2D),AL2(H2CODE2-HOOK2D)                          
                                                                                
         DC    AL1(8),AL2(H2CODE1-HOOK2D),AL2(H2NSEQ3-HOOK2D)                   
         DC    AL2(H1CODE2-HOOK1D),AL2(H1NAME2-HOOK1D)                          
         DC    AL2(H2CODE1-HOOK2D),AL2(0)                                       
                                                                                
         DC    X'FF'                                                            
                                                                                
FLTRTAB  DS    0C                                                               
         DC    CL1'F',AL3(0)                                                    
FLTRLEN  EQU   *-FLTRTAB                                                        
         DC    CL1'P',AL3(0)                                                    
         DC    CL1'D',AL3(0)                                                    
         DC    CL1'*',AL3(0)                                                    
         DC    CL1' ',AL3(0)                                                    
         DC    X'FF'                                                            
                                                                                
CLITAB   DS    0C                    REPORT ONLY SELECT CLIENTS                 
         DC    CL3'BAS'                                                         
         DC    CL3'BAM'                                                         
         DC    CL3'BNM'                                                         
         DC    CL3'BSM'                                                         
         DC    CL3'BME'                                                         
         DC    CL3'BNH'                                                         
         DC    CL3'BRI'                                                         
         DC    CL3'BVT'                                                         
         DC    CL3'BWM'                                                         
         DC    CL3'BNY'                                                         
         DC    CL3'BMB'                                                         
         DC    CL3'BAD'                                                         
         DC    CL3'BAW'                                                         
         DC    CL3'BAI'                                                         
         DC    CL3'BAV'                                                         
         DC    CL3'BVD'                                                         
         DC    X'FF'                                                            
                                                                                
ANLTAB   DS    0C                    14 CODES AND NAMES                         
         DC    CL1' ',CL36'NON-ANALYZED'                                        
ANLENTL  EQU   *-ANLTAB                                                         
         DC    50CL1' ',50CL36' '                                               
         DC    X'FF'                                                            
                                                                                
         LTORG                                                                  
                                                                                
         DS    0F                                                               
HOOKIO   DS    CL2000                                                           
         EJECT                                                                  
                                                                                
*-------------------------------------------------------------------*           
*        TABLE DSECTS                                                           
*-------------------------------------------------------------------*           
ANLTABD  DSECT                                                                  
ANLCODE  DS    CL1                                                              
ANLNAME  DS    CL36                                                             
                                                                                
REPTABD  DSECT                                                                  
RPTCODE  DS    AL1              REPORT NUMBER                                   
CLIDISP  DS    AL2              DISP TO CLIENT CODE                             
PERNAME  DS    AL2              DISP TO PERSON NAME                             
CGCODE   DS    AL2              DISP TO COST GROUP CODE                         
CGNAME   DS    AL2              DISP TO COST GROUP NAME                         
LEV3CLI  DS    AL2              DISP TO 1C LEVEL 3                              
LEV4DIV  DS    AL2              DISP TO 1C LEVEL 4 (OR ZERO IF N/A)             
                                                                                
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        MODE PUTHOOK RECORD (1)                                                
*-------------------------------------------------------------------*           
HOOK1D   DSECT                                                                  
H1HEADER DS    0XL18                                                            
H1REPNO  DS    XL1                                                              
H1REPCPY DS    XL1                                                              
         DS    XL16                                                             
H1ROW1   DS    XL2                                                              
H1CODE1  DS    XL14                                                             
H1ROW2   DS    XL2                                                              
H1CODE2  DS    XL14                                                             
H1ROW3   DS    XL2                                                              
H1CODE3  DS    XL14                                                             
H1ROW4   DS    XL2                                                              
H1CODE4  DS    XL14                                                             
*                                                                               
HZEROS   DS    CL96                                                             
*                                                                               
H1COL1   DS    PL8                                                              
H1COL2   DS    PL8                                                              
H1COL3   DS    PL8                                                              
H1COL4   DS    PL8                                                              
H1COL5   DS    PL8                                                              
H1COL6   DS    PL8                                                              
H1COL7   DS    PL8                                                              
H1COL8   DS    PL8                                                              
H1COL9   DS    PL8                                                              
H1COL10  DS    PL8                                                              
H1COL11  DS    PL8                                                              
H1COL12  DS    PL8                                                              
H1COL13  DS    PL8                                                              
H1COL14  DS    PL8                                                              
*                                                                               
HZEROS2  DS    XL144                                                            
*                                                                               
H1NAME1  DS    CL36                                                             
H1NAME2  DS    CL36                                                             
H1NAME3  DS    CL36                                                             
H1NAME4  DS    CL36                                                             
*                                                                               
H1LEN    EQU   *-HOOK1D                                                         
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        MODE SORTHOOK RECORD (2)                                               
*-------------------------------------------------------------------*           
HOOK2D   DSECT                                                                  
H2ROW1   DS    XL2                                                              
H2NSEQ1  DS    XL36                                                             
H2CODE1  DS    XL14                                                             
H2ROW2   DS    XL2                                                              
H2NSEQ2  DS    XL36                                                             
H2CODE2  DS    XL14                                                             
H2ROW3   DS    XL2                                                              
H2NSEQ3  DS    XL36                                                             
H2CODE3  DS    XL14                                                             
H2ROW4   DS    XL2                                                              
H2NSEQ4  DS    XL36                                                             
H2CODE4  DS    XL14                                                             
H2REPNO  DS    XL1                                                              
H2REPCP  DS    XL1                                                              
H2TYPE   DS    XL2                                                              
*                                                                               
H2NAME1  DS    CL36                                                             
H2NAME2  DS    CL36                                                             
H2NAME3  DS    CL36                                                             
H2NAME4  DS    CL36                                                             
*                                                                               
H2COL1   DS    PL8                                                              
H2COL2   DS    PL8                                                              
H2COL3   DS    PL8                                                              
H2COL4   DS    PL8                                                              
H2COL5   DS    PL8                                                              
H2COL6   DS    PL8                                                              
H2COL7   DS    PL8                                                              
H2COL8   DS    PL8                                                              
H2COL9   DS    PL8                                                              
H2COL10  DS    PL8                                                              
H2COL11  DS    PL8                                                              
H2COL12  DS    PL8                                                              
H2COL13  DS    PL8                                                              
H2COL14  DS    PL8                                                              
H2LEN    EQU   *-HOOK2D                                                         
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        MODE SORTOUT (3) RECORD                                                
*-------------------------------------------------------------------*           
HOOK3D   DSECT                                                                  
H3ROW1   DS    XL2                                                              
H3NSEQ1  DS    XL36                                                             
H3CODE1  DS    XL14                                                             
H3ROW2   DS    XL2                                                              
H3NSEQ2  DS    XL36                                                             
H3CODE2  DS    XL14                                                             
H3ROW3   DS    XL2                                                              
H3NSEQ3  DS    XL36                                                             
H3CODE3  DS    XL14                                                             
H3ROW4   DS    XL2                                                              
H3NSEQ4  DS    XL36                                                             
H3CODE4  DS    XL14                                                             
H3REPNO  DS    XL1                                                              
H3REPCP  DS    XL1                                                              
H3TYPE   DS    XL2                                                              
*                                                                               
H3NAME1  DS    CL36                                                             
H3NAME2  DS    CL36                                                             
H3NAME3  DS    CL36                                                             
H3NAME4  DS    CL36                                                             
*                                                                               
H3COL1   DS    PL8                                                              
H3COL2   DS    PL8                                                              
H3COL3   DS    PL8                                                              
H3COL4   DS    PL8                                                              
H3COL5   DS    PL8                                                              
H3COL6   DS    PL8                                                              
H3COL7   DS    PL8                                                              
H3COL8   DS    PL8                                                              
H3COL9   DS    PL8                                                              
H3COL10  DS    PL8                                                              
H3COL11  DS    PL8                                                              
H3COL12  DS    PL8                                                              
H3COL13  DS    PL8                                                              
H3COL14  DS    PL8                                                              
H3LEN    EQU   *-HOOK3D                                                         
         EJECT                                                                  
                                                                                
                                                                                
*        ACAPGGEND                                                              
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACAPGGEND                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012APGHM2AGAA05/01/02'                                      
         END                                                                    
