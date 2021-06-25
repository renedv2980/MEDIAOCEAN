*          DATA SET APGHFINEA  AT LEVEL 065 AS OF 08/11/99                      
*PHASE ACHFINEA,+0                                                              
         TITLE 'BATES CLIENT WARNER LAMBERT'                                    
ACHFINEA CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8                                                    
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
*---------------------------------------------------------------------*         
*        MODE PUTHOOK=1 - BEFORE SORT                                 *         
*---------------------------------------------------------------------*         
         CLI   HOOKNUM,1                                                        
         BNE   HK200                                                            
                                                                                
         USING HOOK1D,R5                                                        
         CLI   QOPT6,C'1'          DUMP RECORD BEFORE CHANGES                   
         BNE   HK010                                                            
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1B'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
                                                                                
HK010    DS    0H                                                               
         CLI   QOPT3,C'X'                                                       
         BNE   HK011                                                            
         CLC   CURRCON+1(2),=C'16'                                              
         BNE   HK011                                                            
         CLC   CURRCON+5(6),=C'999003'                                          
         BE    XITNO                                                            
                                                                                
HK011    DS    0H                                                               
         USING REPTABD,R2          TABLE OF REPORTS USING 'REPORTING            
         LA    R2,REPTAB           OFFICE' FEATURE                              
HK012    CLI   TABREPNO,X'FF'                                                   
         BE    HK020                                                            
         CLC   TABREPNO,R1REPNO                                                 
         BE    HK013                                                            
         LA    R2,RTABLEN(R2)                                                   
         B     HK012                                                            
                                                                                
HK013    CLC   CURRCON+1(2),=C'11'       IF CONTRA UL=11,12 GET OFFICE          
         BE    HK020                     FROM ACCOUNT                           
         CLC   CURRCON+1(2),=C'12'                                              
         BE    HK020                                                            
         MVC   R1CODE1(2),CURRCON+3      IF CUL=16 GET OFFICE FROM              
         CLC   CURRCON+1(2),=C'16'       CA POSITION 1                          
         BE    *+10                      ELSE CUL=13,14,15 AND OFFICE           
         MVC   R1CODE1(2),CURRCON+4      IS FROM CA POSITION 2                  
                                                                                
*        ONLY BUILD FOLLOWING TABLES ONCE EACH TIME REPORT RUNS                 
                                                                                
HK020    CLI   HOOKSW,C'Y'               IF YES, I'VE BEEN HERE BEFORE          
         BE    HK070                     SKIP                                   
         MVI   HOOKSW,C'Y'               ELSE SET SWITCH FOR NEXT PASS          
                                                                                
         LA    R2,FILTOFF                OFFICE FILTER TABLE                    
         MVI   0(R2),X'FF'               MARK INITIAL END OF TABLE              
         CLC   QSELECT,SPACES            ANY OFFICE FILTERING REQUESTED         
         BE    HK040                     COULD BE LIST OF SINGLE OFFICE         
         MVC   0(2,R2),QSELECT           MOVE IT TO TABLE                       
         MVI   2(R2),X'FF'               NEW EOT                                
                                                                                
         USING OFFRECD,R3                                                       
         L     R3,AHOOKIO                READ OFFICE/OFFICE LIST REC            
         MVC   0(42,R3),SPACES                                                  
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,QCOMPANY                                                 
         MVC   OFFKOFF,QSELECT                                                  
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',AHOOKIO,AHOOKIO                  
         TM    DMCB,X'10'                                                       
         BO    HK040                                                            
                                                                                
         L     R3,AHOOKIO                                                       
         TM    OFFRSTAT,OFFSLIST         IS THIS AN OFFICE LIST                 
         BZ    HK040                     IF NOT TABLE IS ALREADY SET            
                                                                                
         L     R3,AHOOKIO                ELSE FIND OFFICE LIST ELEMENT          
         AH    R3,DATADISP                                                      
HK025    CLI   0(R3),0                                                          
         BE    HK040                                                            
         CLI   0(R3),OFLELQ                                                     
         BE    HK030                                                            
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     HK025                                                            
                                                                                
         USING OFLELD,R3                                                        
HK030    LA    R2,FILTOFF                BUILD TABLE OF OFFICES TO              
         LA    R4,OFLNTRY                FILTER AGAINST                         
         ZIC   R5,OFLLN                                                         
         SH    R5,=H'3'                                                         
HK035    MVC   0(2,R2),0(R4)                                                    
         LA    R2,2(R2)                                                         
         LA    R4,2(R4)                                                         
         BCTR  R5,0                                                             
         BCT   R5,HK035                                                         
         MVI   0(R2),X'FF'                                                      
                                                                                
*        BUILD A TABLE OF ALL OFFICES/NAMES ON FILE TO ASSIGN NAMES             
*        TO REPORTING OFFICE                                                    
                                                                                
HK040    DS    0H                                                               
*MN                                                                             
         B     HK070                                                            
*MN                                                                             
         USING OFFTABD,R2                                                       
         LA    R2,OFFTAB                 TABLE OF OFFICES AND NAMES             
         MVI   0(R2),X'FF'               MARK INITIAL EOT                       
                                                                                
         USING OFFRECD,R3                                                       
         L     R3,AHOOKIO                SET KEY TO BEGIN READING AT            
         MVC   0(42,R3),SPACES           FIRST OFFICE REC FOR THIS ID           
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,QCOMPANY                                                 
         MVC   COMMAND,DMRDHI                                                   
HK043    GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',AHOOKIO,AHOOKIO                 
         MVC   COMMAND,DMRSEQ            SEQENTIALS AFTER THIS                  
         L     R3,AHOOKIO                                                       
         CLC   OFFKCPY,QCOMPANY          HAS COMPANY CHANGED - IF YES           
         BNE   HK070                     I'M THROUGH                            
         TM    OFFRSTAT,OFFSLIST         IS THIS AN OFFICE LIST RECORD          
         BO    HK043                     IF YES SKIP AND READ NEXT              
                                                                                
         L     R4,AHOOKIO                GET NAME ELEMENT                       
         AH    R4,DATADISP                                                      
HK050    CLI   0(R4),0                                                          
         BE    HK043                                                            
         CLI   0(R4),NAMELQ                                                     
         BE    HK055                                                            
         ZIC   R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     HK050                                                            
                                                                                
         USING NAMELD,R4                                                        
HK055    DS    0H                                                               
         MVC   OFFCDE,OFFKOFF            ADD CODE AND NAME TO TABLE             
         ZIC   R5,NAMLN                                                         
         SH    R5,=H'3'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   OFFNME(0),NAMEREC                                                
         LA    R2,OFFTABL(R2)                                                   
         MVI   0(R2),X'FF'                                                      
         B     HK043                                                            
                                                                                
*        FILTER THIS RECORD AGAINST QSELECT TABLE                               
*        EITHER AN OFFICE OR AN OFFICE LIST                                     
                                                                                
HK070    CLC   QSELECT,SPACES            IF NOTHING ENTERED SKIP                
         BE    HK099                     FILTERING                              
                                                                                
         L     R5,HOOKAREC               GET OFFICE TO FILTER AGAINST           
         MVC   THISOFF,R1CODE1           SHOULD BE IN ROW 1                     
         CLI   R1REPNO,8                 EXCEPT FOR COMPANY SUMMARY             
         BH    HK099                     REPORTS (8 AND 16)                     
                                                                                
HK071    MVC   THISOFF,ACTACC+3                                                 
         CLI   R1REPNO,8                                                        
         BH    HK080                                                            
         CLC   CURRCON+1(2),=C'11'                                              
         BE    HK080                                                            
         CLC   CURRCON+1(2),=C'12'                                              
         BE    HK080                                                            
         MVC   THISOFF,CURRCON+3                                                
         CLC   CURRCON+1(2),=C'16'                                              
         BE    *+10                                                             
         MVC   THISOFF,CURRCON+4                                                
                                                                                
HK080    LA    R2,FILTOFF                CHECK THIS OFFC AGAINST TABLE          
HK085    CLI   0(R2),X'FF'               NOT IN TABLE - DROP RECORD             
         BE    XITNO                                                            
         CLC   0(2,R2),THISOFF                                                  
         BE    HK099                                                            
         LA    R2,2(R2)                                                         
         B     HK085                                                            
                                                                                
HK099    DS    0H                                                               
         CLI   QOPT7,C'1'                DUMP AFTER RECORD                      
         BNE   XIT                                                              
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1A'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        HOOK MODE SORTHOOK=2                                         *         
*        DO NOT NEED TO CHECK REPORT TABLE - SPEC ONLY COMES HERE     *         
*        FROM REPORTS ACTUALLY DOING REPORTING OFFICE FEATURE         *         
*---------------------------------------------------------------------*         
         USING HOOK2D,R5                                                        
HK200    CLI   HOOKNUM,2                                                        
         BNE   XIT                                                              
                                                                                
         CLI   QOPT6,C'2'                DUMP BEFORE RECORD                     
         BNE   HK205                                                            
         LA    R0,R2LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD2B'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
                                                                                
HK205    CLC   R2CODE1(2),SPACES         IF THESE EXIST - DROP THEM             
         BE    XITNO                                                            
         CLI   R2CODE1,0                                                        
         BE    XITNO                                                            
                                                                                
         MVC   R2NAME1,SPACES            CLEAR NAME AND SET IT FROM             
         USING OFFTABD,R2                OFFICE NAMES IN TABLE                  
         LA    R2,OFFTAB                                                        
HK210    CLI   0(R2),X'FF'                                                      
         BE    HK230                                                            
         CLC   OFFCDE,R2CODE1                                                   
         BE    HK220                                                            
         LA    R2,OFFTABL(R2)                                                   
         B     HK210                                                            
HK220    MVC   R2NAME1,OFFNME                                                   
                                                                                
HK230    CLI   QOPT7,C'2'                DUMP AFTER RECORD                      
         BNE   XIT                                                              
         LA    R0,R2LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD2A'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
                                                                                
XIT      SR    RC,RC                                                            
XITNO    LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------*              
*        LITERAL POOL                                                           
*----------------------------------------------------------------*              
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------*              
*        DEFINE CONSTANTS                                                       
*----------------------------------------------------------------*              
HOOKSW   DC    C'N'                IS THIS FIRST PASS THROUGH HOOK              
THISOFF  DC    CL2' '              OFFICE TO FILTER AGAINST TABLE               
COMMAND  DC    CL8' '              DATAMANAGER COMMAND                          
AHOOKIO  DC    A(HOOKIO)           IO AREA                                      
                                                                                
REPTAB   DS    0C                  THESE REPORTS USE REPORTING                  
         DC    AL1(1)              OFFICE FEATURE                               
         DC    AL1(2)                                                           
         DC    AL1(3)                                                           
         DC    AL1(4)                                                           
         DC    AL1(5)                                                           
         DC    AL1(6)                                                           
         DC    AL1(7)                                                           
         DC    X'FF'                                                            
                                                                                
FILTOFF  DC    255CL2'  '          ONLY INCLUDE THESE OFFICES                   
         DC    X'FF'                                                            
                                                                                
OFFTAB   DS    0C                                                               
         DC    80CL2'  ',80CL36' '     ALL OFFICE CODES AND NAMES               
         DC    X'FF'                                                            
*                                                                               
         DS    0F                                                               
HOOKIO   DS    CL2000              IO AREA                                      
         EJECT                                                                  
                                                                                
*----------------------------------------------------------------*              
*        WORKING STORAGE                                                        
*----------------------------------------------------------------*              
REPTABD  DSECT                                                                  
TABREPNO DS    XL1          REPORT NUMBER                                       
RTABLEN  EQU   *-REPTABD                                                        
                                                                                
OFFTABD  DSECT                                                                  
OFFCDE   DS    CL2                                                              
OFFNME   DS    CL36                                                             
OFFTABL  EQU   *-OFFTABD                                                        
         EJECT                                                                  
*----------------------------------------------------------------*              
*        RECORD DSECT FOR HOOK MODE 1                                           
*----------------------------------------------------------------*              
HOOK1D   DSECT                                                                  
R1REPNO  DS    XL1                                                              
         DS    XL17                                                             
R1ROW1   DS    XL2                                                              
R1CODE1  DS    XL14                                                             
R1ROW2   DS    XL2                                                              
R1CODE2  DS    XL14                                                             
R1ROW3   DS    XL2                                                              
R1CODE3  DS    XL14                                                             
R1ROW4   DS    XL2                                                              
R1CODE4  DS    XL14                                                             
R1ROW5   DS    XL2                                                              
R1CODE5  DS    XL14                                                             
R1ROW6   DS    XL2                                                              
R1CODE6  DS    XL14                                                             
R1ROW7   DS    XL2                                                              
R1CODE7  DS    XL14                                                             
R1ROW8   DS    XL2                                                              
R1CODE8  DS    XL14                                                             
         DS    XL32                                                             
*                                                                               
R1COL1   DS    PL8                                                              
R1COL2   DS    PL8                                                              
R1COL3   DS    PL8                                                              
R1COL4   DS    PL8                                                              
R1COL5   DS    PL8                                                              
R1COL6   DS    PL8                                                              
R1COL7   DS    PL8                                                              
R1COL8   DS    PL8                                                              
R1COL9   DS    PL8                                                              
*                                                                               
R1NAME1  DS    CL36                                                             
R1NAME2  DS    CL36                                                             
R1NAME3  DS    CL36                                                             
R1NAME4  DS    CL36                                                             
R1NAME5  DS    CL36                                                             
R1NAME6  DS    CL36                                                             
R1NAME7  DS    CL36                                                             
R1NAME8  DS    CL36                                                             
*                                                                               
R1LEN    EQU   *-HOOK1D                                                         
         EJECT                                                                  
*----------------------------------------------------------------*              
*        RECORD DSECT FOR HOOK MODE 2                                           
*----------------------------------------------------------------*              
HOOK2D   DSECT                                                                  
R2ROW1   DS    XL2                                                              
R2CODE1  DS    XL14                                                             
R2ROW2   DS    XL2                                                              
R2CODE2  DS    XL14                                                             
R2ROW3   DS    XL2                                                              
R2CODE3  DS    XL14                                                             
R2ROW4   DS    XL2                                                              
R2CODE4  DS    XL14                                                             
R2ROW5   DS    XL2                                                              
R2CODE5  DS    XL14                                                             
R2ROW6   DS    XL2                                                              
R2CODE6  DS    XL14                                                             
R2ROW7   DS    XL2                                                              
R2CODE7  DS    XL14                                                             
R2REPNO  DS    XL1                                                              
R2REPCP  DS    XL1                                                              
R2TYPE   DS    XL2                                                              
*                                                                               
R2NAME1  DS    CL36                                                             
R2NAME2  DS    CL36                                                             
R2NAME3  DS    CL36                                                             
R2NAME4  DS    CL36                                                             
R2NAME5  DS    CL36                                                             
R2NAME6  DS    CL36                                                             
R2NAME7  DS    CL36                                                             
*                                                                               
R2COL1   DS    PL8                                                              
R2COL2   DS    PL8                                                              
R2COL3   DS    PL8                                                              
R2COL4   DS    PL8                                                              
R2COL5   DS    PL8                                                              
R2COL6   DS    PL8                                                              
R2COL7   DS    PL8                                                              
R2COL8   DS    PL8                                                              
R2COL9   DS    PL8                                                              
R2LEN    EQU   *-HOOK2D                                                         
         EJECT                                                                  
                                                                                
*        ACAPGGEND                                                              
         PRINT OFF                                                              
       ++INCLUDE ACAPGGEND                                                      
         PRINT ON                                                               
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065APGHFINEA 08/11/99'                                      
         END                                                                    
