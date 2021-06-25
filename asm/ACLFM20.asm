*          DATA SET ACLFM20    AT LEVEL 011 AS OF 05/01/02                      
*PHASE T60320A,+0                                                               
*INCLUDE SCANNER                                                                
*INCLUDE UNSCAN                                                                 
*********************************************************************           
*                                                                   *           
*  MODIFICATION HISTORY                                             *           
*  --------------------                                             *           
*                                                                   *           
*   11-30-94  JSHA   CHANGED ACHRLEVA TO COMBINE WITH ACHRLEVB IF   *           
*                    ACHRLEVA IS EITHER 1 OR 2 INSTEAD OF JUST 1    *           
*                    CHANGE APPEARS IN AC40 CHANGED CLI STATMENT    *           
*                    FROM 1 TO 2 AND THE FOLLOWING BNE TO BH        *           
*                                                                   *           
*********************************************************************           
         EJECT                                                                  
T60320   CSECT                                                                  
         TITLE 'PROGRAM TO ESTABLISH FEE ACCOUNTING RULES'                      
         PRINT NOGEN                                                            
         NMOD1 100,**LFM20*,R9,RR=R5                                            
         LR    R8,RC                                                            
         USING FEED,R8                                                          
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         ST    R5,PRELOC                                                        
         L     R5,=V(SCANNER)                                                   
         A     R5,PRELOC                                                        
         ST    R5,SCANNER                                                       
         L     R5,=V(UNSCAN)                                                    
         A     R5,PRELOC                                                        
         ST    R5,UNSCAN                                                        
         SPACE 1                                                                
*******************           BUILDKEY          ***********************         
         SPACE 1                                                                
         MVI   ERROR,X'FF'                                                      
         MVI   ANYKEY,C'N'                                                      
         CLI   MODE,BUILDKEY                                                    
         BNE   AC110                                                            
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'1F'                                                  
         GOTO1 READ                                                             
         BAS   RE,SAVEM            SAVE HIERARCHY ELEMENT (X'16')               
         LA    R7,SAVEHIER                                                      
         USING ACHEIRD,R7                                                       
         OC    FALCLI,SPACES                                                    
         LA    R2,FALCLIH                                                       
         CLI   5(R2),0                                                          
         BNE   AC10                                                             
         BAS   RE,INTFLD1          CLEAR NAME FIELDS                            
         MVI   ERROR,MISSING                                                    
         B     XIT                                                              
         SPACE 1                                                                
** R6 POINTS TO INSERTION POINT IN KEY **                                       
         SPACE 1                                                                
AC10     LA    R6,KEY+3                                                         
         ZIC   R5,5(R2)                                                         
         BCTR  R5,0                                                             
         EX    R5,MVCDATA                                                       
         SPACE 1                                                                
         TM    4(R2),X'20'         FIELD VALIDATED PREVIOUSLY                   
         BO    AC20                                                             
         BAS   RE,INTFLD1                                                       
         MVI   ERROR,INTOOLNG                                                   
         CLC   5(1,R2),ACHRLEVA                                                 
         BH    XIT                                                              
         GOTO1 READ                                                             
         BAS   RE,GETNAME          PUT OUT CLIENT NAME                          
         MVC   SAVENAME,SPACES                                                  
         SPACE 1                                                                
AC20     OI    FALCLIH+4,X'20'     CLIENT VALIDATED                             
         LA    R2,FALDIVH                                                       
         TM    4(R2),X'20'         FIELD VALIDATED PREVIOUSLY                   
         BO    *+8                                                              
         BAS   RE,INTFLD2                                                       
         OC    FALDIV,SPACES                                                    
         CLI   5(R2),0             IS THERE A DIVISION                          
         BNE   AC30                                                             
         ZIC   R4,ACHRLEVA         NO. FILL WITH *'S                            
         AR    R6,R4               POINT R6 TO DIVISION FIELD START             
         LA    R1,KEY+15                                                        
         SR    R1,R6               L'FIELD TO FILL WITH *'S                     
         EX    R1,MVCSTAR                                                       
         B     AC40                                                             
         SPACE                                                                  
AC30     ZIC   R4,ACHRLEVA                                                      
         ZIC   R5,ACHRLEVB                                                      
         SR    R5,R4               ACTUAL LENGTH OF LVL B                       
         MVI   ERROR,INTOOLNG                                                   
         CLM   R5,1,5(R2)                                                       
         BL    XIT                                                              
         AR    R6,R4               POINT TO INSERTION POINT FOR LEVB            
         ZIC   R5,5(R2)                                                         
         BCTR  R5,0                                                             
         EX    R5,MVCDATA                                                       
         SPACE 1                                                                
AC35     GOTO1 READ                                                             
         BAS   RE,GETNAME                                                       
         MVC   SAVENAME,SPACES                                                  
         SPACE 1                                                                
*******************************   BUILD CONTRA ACCOUNT PART OF KEY ****         
         SPACE 1                                                                
* R6 POINTS TO INSERTION POINT IN STARKEY                                       
         SPACE 1                                                                
AC40     OI    FALDIVH+4,X'20'     DIVISION IS VALID                            
         MVC   HOLDKEY,SPACES                                                   
         MVC   HOLDKEY(15),KEY     SAVE KEY SO FAR                              
         MVC   KEY,SPACES                                                       
         MVC   STARKEY,SPACES      CAN'T READ 1R LEDGER WITH *'S IN KEY         
         LA    R6,STARKEY                                                       
         MVC   0(1,R6),COMPANY                                                  
         MVC   1(2,R6),=C'1R'                                                   
         MVC   KEY,STARKEY         START OFF EQUAL                              
         GOTO1 READ                ON LEDGER 1R IF LEVA IS 1 OR 2 THEN          
         BAS   RE,SAVEM             OFFICE AND DEPARTMENT HAVE BEEN             
         CLI   ACHRLEVA,2           SEPARATED (INTO LEVA AND LEVB)              
         BH    AC45                 AND WILL NOW BE COMBINED INTO               
         MVC   ACHRLEVA,ACHRLEVB    ONE (LEVA). LEVB AND LEVC WILL              
         MVC   ACHRLEVB,ACHRLEVC    BE ADJUSTED ACCORDINGLY. LEVD               
         MVC   ACHRLEVC,ACHRLEVD    REMAINS UNCHANGED.                          
AC45     OC    FALOD,SPACES                                                     
         LA    R2,FALODH                                                        
         TM    4(R2),X'20'                                                      
         BO    *+8                                                              
         BAS   RE,INTFLD3                                                       
         CLI   5(R2),0             IS THERE AN OFFICE/DEPT(OD)                  
         BNE   AC50                                                             
         LA    R6,3(R6)            NO. MOVE IN *'S                              
         LA    R1,STARKEY+15                                                    
         SR    R1,R6                                                            
         EX    R1,MVCSTAR                                                       
         B     AC63                                                             
         SPACE                                                                  
AC50     MVI   ERROR,INTOOLNG                                                   
         CLC   5(1,R2),ACHRLEVA                                                 
         BH    XIT                                                              
         LA    R6,3(R6)            POINT TO LEVA FLD                            
         ZIC   R5,5(R2)            L'INPUT DATA (OFFICE/DEPT)                   
         BCTR  R5,0                                                             
         EX    R5,MVCDATA                                                       
         MVC   KEY,STARKEY         UPDATE KEY                                   
         SPACE 1                                                                
AC55     TM    4(R2),X'20'         FIELD VALIDATED PREVIOUSLY                   
         BO    AC63                                                             
AC60     GOTO1 READ                                                             
         BAS   RE,GETNAME                                                       
         SPACE                                                                  
***** VALIDATE SUB-DEPT*******                                                  
         SPACE 1                                                                
AC63     OI    FALODH+4,X'20'      OFFICE/DEPT. IS VALID                        
         LA    R2,FALSDH                                                        
         OC    FALSD,SPACES                                                     
         TM    4(R2),X'20'                                                      
         BO    *+8                                                              
         BAS   RE,INTFLD4                                                       
         CLI   5(R2),0             ANY INPUT                                    
         BNE   AC65                NO. FILL REST OF STARKEY WITH *'S            
         ZIC   R4,ACHRLEVA                                                      
         AR    R6,R4               POINT TO START OF SD FIELD                   
         LA    R1,STARKEY+15                                                    
         SR    R1,R6               L'FIELD TO FILL WITH *'S                     
         EX    R1,MVCSTAR                                                       
         B     AC83                                                             
         SPACE                                                                  
AC65     CLI   0(R6),C'*'          IF OD INPUT THEN NO *'S                      
         BNE   AC70                                                             
         LA    R2,FALODH           TO POSITION CURSOR                           
         MVI   ERROR,NOHIGHER      HIGHER LEVEL A/C MISSING                     
         B     XIT                                                              
         SPACE                                                                  
AC70     ZIC   R4,ACHRLEVA                                                      
         ZIC   R5,ACHRLEVB                                                      
         SR    R5,R4               ACTUAL LENGTH OF LVL B                       
         MVI   ERROR,INTOOLNG                                                   
         CLM   R5,1,5(R2)                                                       
         BL    XIT                                                              
         AR    R6,R4               POINT TO INSERTION POINT FOR LEVB            
         ZIC   R5,5(R2)            L'INPUT DATA (SD)                            
         BCTR  R5,0                                                             
         EX    R5,MVCDATA                                                       
         MVC   KEY,STARKEY                                                      
         SPACE 1                                                                
AC75     TM    4(R2),X'20'         FIELD VALIDATED PREVIOUSLY                   
         BO    AC83                                                             
AC80     GOTO1 READ                                                             
         BAS   RE,GETNAME                                                       
         SPACE                                                                  
AC83     OI    FALSDH+4,X'20'                                                   
         LA    R2,FALSTAH                                                       
         OC    FALSTA,SPACES                                                    
         TM    4(R2),X'20'                                                      
         BO    *+8                                                              
         BAS   RE,INTFLD5                                                       
         CLI   5(R2),0             ANY INPUT                                    
         BNE   AC85                                                             
         ZIC   R4,ACHRLEVB         NO. FILL WITH *'S                            
         ZIC   R5,ACHRLEVA                                                      
         SR    R4,R5               =L'LEVB                                      
         AR    R6,R4               POINT TO START OF LEVEL C                    
         LA    R1,STARKEY+15                                                    
         SR    R1,R6               L'FIELD TO FILL WITH *'S                     
         EX    R1,MVCSTAR                                                       
         B     AC103                                                            
         SPACE                                                                  
AC85     CLI   0(R6),C'*'          ARE THERE *'S IN THE STARKEY                 
         BNE   AC90                                                             
         LA    R2,FALSDH           TO POSITION CURSOR                           
         MVI   ERROR,NOHIGHER      HIGHER LEVEL A/C MISSING                     
         B     XIT                                                              
         SPACE 1                                                                
AC90     ZIC   R4,ACHRLEVB                                                      
         ZIC   R5,ACHRLEVC                                                      
         SR    R5,R4               ACTUAL LENGTH OF LVL C                       
         MVI   ERROR,INTOOLNG                                                   
         CLM   R5,1,5(R2)                                                       
         BL    XIT                                                              
         ZIC   R5,ACHRLEVA                                                      
         SR    R4,R5                                                            
         AR    R6,R4               POINT TO INSERTION POINT FOR LEVC            
         ZIC   R5,5(R2)            L'INPUT DATA (STAFF NO.)                     
         BCTR  R5,0                                                             
         EX    R5,MVCDATA                                                       
         MVC   KEY,STARKEY         UPDATE KEY                                   
         SPACE 1                                                                
AC95     TM    4(R2),X'20'         FIELD VALIDATED PREVIOUSLY                   
         BO    AC103                                                            
AC100    GOTO1 READ                                                             
         BAS   RE,GETNAME                                                       
         SPACE                                                                  
AC103    OI    FALSTAH+4,X'20'                                                  
         MVC   HOLDKEY+17(15),STARKEY   CONTRA A/C COMPLETED                    
         MVI   ERROR,X'FF'                                                      
         LA    R2,FALCLIH                                                       
         CLI   LOGACT,C'N'                                                      
         BNE   XIT                                                              
         MVC   KEY,HOLDKEY                                                      
         MVC   IO2(42),SPACES                                                   
         GOTO1 HIGH                                                             
         NI    4(R2),X'DF'                                                      
         CLC   KEYSAVE,KEY                                                      
         BNE   XIT                                                              
         MVI   ERROR,RECONFLE                                                   
         B     XIT                                                              
         EJECT                                                                  
**********          DISPLAY RECORD ON SCREEN          ****************          
         SPACE 1                                                                
AC110    DS    0H                                                               
         CLI   MODE,DSPLYREC                                                    
         BNE   AC200               TRY BUILDREC                                 
         TWAXC FALFEE1H                                                         
         LA    R2,FALCLIH                                                       
         MVC   KEY,HOLDKEY                                                      
         GOTO1 READ                                                             
         LA    R2,FALFEE1H                                                      
         MVI   ERROR,X'FF'                                                      
         LA    R7,IO               FIND X'69' ELEMENTS                          
         MVI   ELCODE,X'69'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
AC120    BAS   RE,NEXTEL                                                        
         BE    AC140                                                            
         LA    R2,FALFEE1H         NO MORE X'69' ELEMENTS                       
         B     XIT                                                              
*                   DISPLAY '69' ELEMENT                                        
         SPACE 1                                                                
         USING ACRULD,R7                                                        
AC140    DS    0H                                                               
         LA    R3,ACRULDET                                                      
         ZIC   R5,ACRULEN          L'ELEMENT                                    
         SH    R5,=H'5'            L'ELEM-5=BYTES TO DISPLAY                    
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(R3)       DATA TO FEE RULES FIELD                      
         SPACE 1                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               R2 TO DATE FIELD                             
         OC    ACRULEFF,ACRULEFF                                                
         BZ    AC150               NO EFFECTIVE DATE                            
         MVC   WORK(2),ACRULEFF                                                 
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(6,8(R2))                                   
         SPACE 1                                                                
AC150    ZIC   R0,0(R2)                                                         
         AR    R2,R0               R2 TO NEXT FEE RULES FIELD                   
         B     AC120                                                            
         EJECT                                                                  
***********    ROUTINE TO BUILD A RECORD IN IO2    ********************         
         SPACE 1                                                                
AC200    CLI   LOGACT,C'A'                                                      
         BNE   AC205                                                            
         LA    R2,FALFEE1H                                                      
         CLC   8(6,R2),=C'DELETE'                                               
         BNE   AC205                                                            
         MVC   KEY,HOLDKEY                                                      
         MVC   IO2(42),SPACES                                                   
         MVI   UPDATE,C'Y'                                                      
         GOTO1 READ                                                             
         LA    R6,IO                                                            
         USING ACKEYD,R6                                                        
         OI    ACSTATUS,X'80'      TURN ON DELETE BIT                           
         LA    R4,IO2              MOVE RECORD FROM IO TO IO2 FOR               
         LH    R5,ACLENGTH          PUTREC                                      
         LR    R7,R5                                                            
         MVCL  R4,R6                                                            
         B     ADDRECD                                                          
         DROP  R6                                                               
         SPACE                                                                  
AC205    LA    R4,IO2              CLEAR IO2                                    
         LA    R5,IOLENQ                                                        
         LA    R6,*                                                             
         XR    R7,R7                                                            
         MVCL  R4,R6                                                            
         LA    R7,IO2                                                           
         USING ACKEYD,R7           BUILD A RECORD IN IO2                        
         MVC   IO2(42),SPACES                                                   
         MVC   KEY,HOLDKEY                                                      
         MVC   ACKEYACC(32),KEY                                                 
         XC    ACKEYREF+1(L'ACKEYREF-1+L'ACKEYSBR),ACKEYREF+1                   
         MVC   ACLENGTH,DATADISP                                                
         MVI   ERRFLAG,0                                                        
         SPACE 1                                                                
**********************************  ROUTINE TO BUILD X'43' ELEMENT ****         
         SPACE 1                                                                
BUILD43  LA    R7,ELEMENT                                                       
         USING TRSUBHD,R7                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TRSBEL,X'43'                                                     
         MVC   TRSBACNT,KEY+17     SUB A/C NUMBER                               
         CLI   SAVENAME,X'40'                                                   
         BE    AC210                                                            
         ZIC   R4,SAVENAME                                                      
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   TRSBNAME(0),SAVENAME+1                                           
         LA    R4,18(R4)           L'CODE,L'LEN,L'SUB A/C+1 (EXECUTE)           
         STC   R4,TRSBLEN                                                       
         B     *+12                                                             
         SPACE                                                                  
AC210    MVI   TRSBNAME,X'40'                                                   
         MVI   TRSBLEN,18                                                       
         GOTO1 ADDANEL                                                          
         B     AC300                                                            
         EJECT                                                                  
************************************  READ FEE RULES - *********** ****         
         SPACE 1                                                                
AC300    DS    0H                                                               
         LA    R2,FALFEE1H                                                      
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     XIT                 MUST HAVE AT LEAST ONE RULE                  
         SPACE 1                                                                
AC320    MVI   ERROR,X'FF'                                                      
         ST    R2,FEEHEAD                                                       
         BAS   RE,EDVAL            EDIT RULES AND BUILD ELEMENT                 
         L     R2,FEEHEAD          THE TRT INST. CREAMS R2                      
         CLI   ERROR,X'FF'                                                      
         BNE   XIT                 SOME KIND OF INPUT ERROR                     
         SPACE 1                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               R2 TO DATE FIELD                             
         CLI   5(R2),0                                                          
         BE    AC335               NO DATE                                      
         MVI   ERROR,13            INVALID DATE                                 
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   XIT                 M/D/Y IS INVALID                             
         GOTO1 DATVAL,DMCB,(2,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    XIT                 NOT M/Y                                      
         MVC   WORK+4(2),=C'01'    ALWAYS FIRST OF MONTH                        
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
*        CLI   WORK+6,X'80'        1980                                         
*        BL    XIT                                                              
         LA    R7,ELEMENT                                                       
         USING ACRULD,R7                                                        
         MVC   ACRULEFF,WORK+6        PACKED YY/MM                              
         SPACE 1                                                                
AC335    BAS   RE,DUPCHECK         NOW CHECK FOR  DUPLICATES                    
         CLI   ERROR,X'FF'                                                      
         BE    AC337               NO DUPLICATES                                
         CLI   5(R2),0             ANY INPUT IN DATE FIELD                      
         BNE   XIT                 IF DATE CURSOR IS ON DATE FIELD              
         L     R2,FEEHEAD          ELSE CURSOR IS ON DATA FIELD                 
         B     XIT                                                              
         SPACE 1                                                                
AC337    GOTO1 ADDANEL             LOOKS OK ADD THE ELEMENT                     
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               R2 TO NEXT FEE RULES FIELD                   
         CLI   0(R2),9                                                          
         BE    ADDRECD             TAB FIELD OK TO ADD RECORD                   
         CLI   5(R2),0                                                          
         BE    ADDRECD             NO MORE INPUT, OK TO ADD                     
         B     AC320                                                            
         EJECT                                                                  
**********************************************  VALIDATE FEE RULES ****         
         SPACE 1                                                                
EDVAL    NTR1                                                                   
         LA    R3,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         MVI   ERROR,INVALID                                                    
         CLI   4(R1),1                                                          
         BNE   XIT                 INVALID INPUT FOR SCANNER                    
         LA    R7,TABLE                                                         
         USING RULED,R7                                                         
         MVI   ERROR,INVALID                                                    
         SPACE 1                                                                
AC360    ZIC   R4,KEYLEN           L'KEYWORD FROM TABLE                         
         BCTR  R4,0                                                             
         CLI   0(R7),X'FF'                                                      
         BNE   AC370                                                            
         MVI   ERRFLAG,C'K'        KEYWORD ERROR                                
         GOTO1 ERRORTN,DMCB,(R3),M1L,M2L                                        
         B     XIT                                                              
         SPACE                                                                  
AC370    EX    R4,CLCKEY                                                        
         BNE   AC375                                                            
         CLI   1(R3),0             GOT VALID KEYWORD. CHECK THAT FORM           
         BNE   AC380                IS KEYWORD=VALUE                            
         MVI   ERRFLAG,C'K'                                                     
         GOTO1 ERRORTN,DMCB,(R3),M1L,M12L                                       
         B     XIT                                                              
         SPACE                                                                  
AC375    ZIC   R0,LEN              POINT TO NEXT TABLE ENTRY                    
         AR    R7,R0                                                            
         B     AC360                                                            
CLCKEY   CLC   12(0,R3),KEYWORD                                                 
         SPACE                                                                  
AC380    L     RF,EDITRTN                                                       
         A     RF,PRELOC                                                        
         BR    RF                                                               
         SPACE 1                                                                
***************  GOT A VALID KEYWORD - NOW VALIDATE VALUE PORTION ****          
         SPACE 1                                                                
EDSTART  DS    0H                                                               
         CLI   1(R3),3             MONTHS ALWAYS 3 BYTES                        
         BE    AC390                                                            
         GOTO1 ERRORTN,DMCB,(R3),M1L,M8L                                        
         B     XIT                                                              
         SPACE                                                                  
AC390    LA    R4,MONTBLE                                                       
         CLI   0(R4),X'FF'                                                      
         BNE   AC400                                                            
         GOTO1 ERRORTN,DMCB,(R3),M1L,M3L                                        
         B     XIT                                                              
         SPACE                                                                  
AC400    CLC   0(3,R4),22(R3)                                                   
         BE    BUILD69                                                          
         LA    R4,3(R4)                                                         
         B     AC390+4                                                          
*                                                                               
EDTYPE   DS    0H                                                               
         LA    R5,TYPETBLE                                                      
AC430    CLI   0(R5),X'FF'                                                      
         BNE   AC435                                                            
         GOTO1 ERRORTN,DMCB,(R3),M1L,M9L                                        
         B     XIT                                                              
         SPACE                                                                  
AC435    ZIC   R4,1(R3)                                                         
         BCTR  R4,0                                                             
         EX    R4,CLCTYPE                                                       
         BE    BUILD69                                                          
         LA    R5,4(R5)                                                         
         B     AC430                                                            
CLCTYPE  CLC   22(0,R3),0(R5)                                                   
*                                                                               
EDADJUST DS    0H                                                               
         CLI   1(R3),2                                                          
         BH    AC440                                                            
         CLC   22(2,R3),=C'YE'     ONLY POSSIBILITY ALLOWED                     
         BE    BUILD69                                                          
AC440    GOTO1 ERRORTN,DMCB,(R3),M1L,M10L                                       
         B     XIT                                                              
         SPACE 2                                                                
****************  ROUTINE TO VALIDATE VARIABLE NUMERIC EXPRESSIONS ****         
         SPACE 1                                                                
HILO     DS    0H                                                               
         MVC   MYWORK,SPACES                                                    
         LA    R4,22(R3)           POINT R4 TO ENTRY TO VALIDATE                
         XR    R6,R6               INITIALIZE CNTR                              
         ZIC   R5,1(R3)            L'ENTRY FROM BLOCK                           
AC450    CLI   0(R4),X'4B'         IS THERE A DECIMAL POINT                     
         BE    AC470                                                            
         TRT   0(1,R4),NUMTBLE                                                  
         BNE   NOTNUM                                                           
         LA    R6,1(R6)                                                         
         LA    R4,1(R4)            POINT TO NEXT BYTE TO TEST                   
         BCT   R5,AC450                                                         
         MVI   MYWORK+1,0          DEC PT NOT FOUND = INTEGERS ONLY             
         STC   R6,MYWORK           COUNT (INTEGERS ONLY)                        
         B     AC520               OR                                           
         SPACE                                                                  
AC470    STC   R6,MYWORK           COUNT(INTEGERS) OR 0 (DECIMAL ONLY)          
         ZIC   R5,1(R3)            L'ENTRY                                      
         SR    R5,R6               - L'LEFT = L'RIGHT                           
         BCTR  R5,0                MINUS DECIMAL POINT                          
         STC   5,MYWORK+1          COUNT OF DECIMAL PLACES                      
         LA    R4,1(R4)            POINT PAST DECIMAL POINT                     
AC480    TRT   0(1,R4),NUMTBLE                                                  
         BNE   NOTNUM                                                           
         LA    R4,1(R4)                                                         
         BCT   R5,AC480                                                         
AC490    CLI   MYWORK+1,0          ANY DECIMAL PLACES                           
         BE    AC520                                                            
         CLI   DECPOINT,0          ARE DECIMAL PLACES ALLOWED                   
         BNE   AC500                                                            
         GOTO1 ERRORTN,DMCB,(R3),M1L,M13L                                       
         B     XIT                 (NO DECIMAL PLACES ALLOWED)                  
         SPACE                                                                  
AC500    CLC   MYWORK+1(1),DECPOINT  DOES IT EXCEED MAX DEC PLS ALLOWED         
         BNH   AC520                                                            
         GOTO1 ERRORTN,DMCB,(R3),M1L,M5L                                        
         B     XIT                 (TOO MANY DECIMAL PLACES)                    
         SPACE                                                                  
AC520    CLI   MYWORK,0            IS LEFT SIDE LENGTH ZERO                     
         BE    BUILD69                                                          
         CLC   MYWORK(1),MAXINT    DOES IT EXCEED MAX. INT'S ALLOWED            
         BNH   BUILD69                                                          
         GOTO1 ERRORTN,DMCB,(R3),M1L,M11L                                       
         B     XIT                 (NUMBER TOO LARGE)                           
         SPACE                                                                  
NOTNUM   GOTO1 ERRORTN,DMCB,(R3),M1L,M4L                                        
         B     XIT                 (NOT NUMERIC)                                
         SPACE                                                                  
*******************************************  BUILD A X'69' ELEMENT ****         
         SPACE                                                                  
BUILD69  DS    0H                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R7,ELEMENT                                                       
         USING ACRULD,R7                                                        
         MVI   ACRUEL,X'69'                                                     
         ZIC   R4,0(R3)            L'KEYWORD                                    
         LR    R6,R4               SAVE LENGTH OF KEYWORD IN R6                 
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   ACRULDET(0),12(R3)  MOVE IN KEYWORD                              
         SPACE 1                                                                
         LA    R5,ACRULDET                                                      
         AR    R5,R6               ADD L'KEYWORD TO START PT OF RULEDET         
         MVI   0(R5),C'='                                                       
         ZIC   R4,1(R3)            L'VALUE                                      
         AR    R6,R4               ADD L'VALUE TO R6                            
         LA    R6,5(R6)            ADD L'ACRUEL,L'ACRULEN+1(= SIGN)+2           
         STC   R6,ACRULEN          MOVE IN ELEMENT LENGTH                       
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R5),22(R3)      MOVE IN VALUE                                
         MVI   ERROR,X'FF'         GOOD INPUT                                   
         B     XIT                                                              
         EJECT                                                                  
*************************  ROUTINE TO CHECK FOR DUPLICATE KEYWORDS ****         
         SPACE 1                                                                
DUPCHECK NTR1                                                                   
         LA    R3,BLOCK            POINT R3 TO CURRENT ENTRY                    
         MVI   ELCODE,X'69'                                                     
         LA    R7,IO2                                                           
         BAS   RE,GETEL                                                         
         BNE   NODUP               FIRST TIME NO DUPS                           
         USING ACRULD,R7                                                        
         ZIC   R1,0(R3)            L'CURRENT ENTRY                              
         BCTR  R1,0                                                             
DUPCHK3  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ACRULDET(0),12(R3)                                               
         BNE   DUPCHK5                                                          
         LA    R4,ELEMENT          R4 TO CURRENT ELEMENT                        
         CLC   ACRULEFF,ACRULEFF-ACRULD(R4)                                     
         BE    DUPERR              DIFFERENT DATES ARE ALLOWED                  
         SPACE 1                                                                
DUPCHK5  BAS   RE,NEXTEL           NEXT ELEMENT FROM RECORD                     
         BNE   NODUP                                                            
         B     DUPCHK3                                                          
         SPACE 1                                                                
NODUP    MVI   ERROR,X'FF'                                                      
         B     XIT                                                              
DUPERR   MVI   ERRFLAG,C'K'        KEYWORD ERROR                                
         GOTO1 ERRORTN,DMCB,(R3),M6L,M7L                                        
         B     XIT                                                              
         EJECT                                                                  
********************************************  WRITE RECORD TO FILE ****         
         SPACE 1                                                                
ADDRECD  DS    0H                                                               
         OI    DMINBTS,X'88'       READ DELETED RECORDS                         
         MVC   KEY,IO2                                                          
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'88'                                              
         CLC   KEYSAVE,KEY                                                      
         BNE   ADDIT                                                            
         GOTO1 PUTREC                                                           
         MVI   ERROR,X'FF'                                                      
         B     XIT                                                              
         SPACE                                                                  
ADDIT    GOTO1 ADDREC                                                           
         SPACE                                                                  
***********   NOW ADD RECORD WITH ASTERISKS IF NOT ALREADY PRESENT ****         
         SPACE 1                                                                
         DS    0H                  BUILD NEW KEY                                
         LA    R7,SAVEHIER                                                      
         USING ACHEIRD,R7                                                       
         MVI   ERROR,X'FF'                                                      
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'1F'                                                  
         LA    R6,KEY+3            POINT TO LVL A START POINT                   
         CLI   FALDIVH+5,0         DID THEY ENTER A DIVISION                    
         BNE   XIT                 YES. THEN WE'RE OKAY                         
         ZIC   R4,FALCLIH+5        OTHERWISE MOVE CLIENT TO KEY                 
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),FALCLI                                                   
         SPACE                                                                  
         ZIC   R4,ACHRLEVA         L'LEVEL A (CLIENT)                           
         AR    R6,R4               POINT R6 TO LVL B START POINT                
         LA    R4,KEY+15           POINT R4 TO END OF KEY FIELD                 
         SR    R4,R6               NUMBER OF BYTES TO FILL WITH *'S             
         BCTR  R4,0                                                             
         EX    R4,MVCSTAR                                                       
         SPACE                                                                  
         MVC   IO2(42),SPACES                                                   
         GOTO1 HIGH                                                             
         CLC   KEY,KEYSAVE                                                      
         BE    XIT                 IT EXISTS - NO ERROR HERE                    
         MVC   KEY,KEYSAVE                                                      
         LA    R4,IO2              CLEAR IO2                                    
         LA    R5,IOLENQ                                                        
         LA    R6,*                                                             
         XR    R7,R7                                                            
         MVCL  R4,R6                                                            
         LA    R7,IO2                                                           
         USING ACKEYD,R7                                                        
         MVC   IO2(42),SPACES                                                   
         MVC   ACKEYACC(32),KEY                                                 
         MVC   ACLENGTH,DATADISP                                                
         LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
         USING ACNAMED,R4          BUILD NAME ELEMENT                           
         ZIC   R5,SAVE20+1         ELEMENT LENGTH FOR X'20' ELEMENT             
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   ACNMEL(0),SAVE20    MOVE IN NAME ELEMENT                         
         SPACE                                                                  
         ZIC   R0,ACNMLEN          L'NAME ELEMENT                               
         XR    R5,R5                                                            
         ICM   R5,3,ACLENGTH                                                    
         AR    R5,R0               ADD TO ELEMENT LENGTH SO FAR                 
         STCM  R5,3,ACLENGTH       UPDATE RECORD LENGTH                         
         GOTO1 STATIN              ADD STATUS ELEMENT                           
         GOTO1 BALIN               ADD BALANCE ELEMENT                          
         GOTO1 ADDREC                                                           
         B     XIT                                                              
         SPACE                                                                  
*                                                                               
XIT      XIT1  REGS=(R2)                                                        
*                                                                               
         SPACE                                                                  
******************************   ROUTINE TO SAVE HIERARCHY DETAILS ****         
         SPACE 1                                                                
SAVEM    DS    0H                                                               
         LA    R4,IO                                                            
         AH    R4,DATADISP                                                      
SAVEH2   CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),X'16'                                                      
         BE    SAVEH4                                                           
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     SAVEH2                                                           
         SPACE 1                                                                
SAVEH4   MVC   SAVEHIER,0(R4)                                                   
         BR    RE                                                               
         SPACE                                                                  
***************  ROUTINE TO PUT OUT AND SAVE NAME ELEMENT DETAILS ****          
         SPACE                                                                  
GETNAME  DS    0H                  R2 POINTS TO HDR OF INPUT FIELD              
         ZIC   R3,0(R2)                                                         
         AR    R3,R2               POINT R3 TO NAME FIELD HEADER                
         ZIC   R1,0(R3)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),SPACES                                                   
         LA    R4,IO                                                            
         AH    R4,DATADISP                                                      
GET2     CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),X'20'         FIND NAME ELEMENT                            
         BE    GET3                                                             
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     GET2                                                             
         SPACE                                                                  
GET3     DS    0H                                                               
         USING ACNAMED,R4                                                       
         LA    R5,FALCLIH                                                       
         CR    R2,R5                                                            
         BNE   GET4                                                             
         ZIC   R5,ACNMLEN          SAVE20 ONLY NEEDED AT CLIENT LEVEL           
         BCTR  R5,0                 FOR ASTERISK RECORD                         
         EX    R5,MVC20                                                         
GET4     CLI   ACNMNAME,X'40'      NO NAME. KEEP WHAT WE'VE ALREADY GOT         
         BE    GETOUT                                                           
         CLI   0(R6),C'*'          GOT *'S - NO NEED TO GO ON                   
         BE    GETOUT                                                           
         MVC   SAVENAME,SPACES                                                  
         ZIC   R5,ACNMLEN                                                       
         CH    R5,=H'38'           MAXIMUM ELEMENT LENGTH (38 BYTES)            
         BL    *+8                                                              
         LA    R5,38                                                            
         SH    R5,=H'3'                                                         
         EX    R5,MVCNAME          MOVE NAME TO SCREEN FIELD                    
         EX    R5,MVCSVNM          SAVE NAME AND LEN FOR X'43' ELEMENT          
         STC   R5,SAVENAME         SAVE FOR BUILDING X'43' ELEMENT              
GETOUT   XR    R4,R4               GIVE EM BACK CLEAN                           
         XR    R5,R5                                                            
         BR    RE                                                               
         SPACE                                                                  
*******************************  ROUTINE TO OUTPUT ERROR MESSAGES ****          
         SPACE                                                                  
* R3 POINTS TO INVALID EXPRESSION                                               
* R4 POINTS TO FIRST HALF OF ERROR MESSAGE                                      
* R5 POINTS TO SECOND HALF OF ERROR MESSAGE                                     
         SPACE                                                                  
ERRORTN  NTR1                                                                   
         LM    R3,R5,0(R1)                                                      
         MVC   LOGHEAD,SPACES                                                   
         OI    LOGHEADH+6,X'80'                                                 
         MVI   ERROR,X'FE'         TELL BASE IT'S MY SHOW                       
         LA    R7,LOGHEAD                                                       
         ZIC   R6,0(R4)            L'FIRST HALF OF MSG                          
         BCTR  R6,0                                                             
         EX    R6,MVCMSG1          MOVE IT TO LOGHEAD                           
         LA    R6,1(R6)                                                         
         AR    R7,R6               POINT TO INSERTION POINT FOR ERROR           
         CLI   ERRFLAG,C'K'        WAS THE ERROR IN THE KEYWORD                 
         BNE   ERR1                                                             
         ZIC   R6,0(R3)            L'KEYWORD THAT IS IN ERROR                   
         BCTR  R6,0                                                             
         EX    R6,MVCERKEY                                                      
         B     ERR2                OR                                           
         SPACE                                                                  
ERR1     ZIC   R6,1(R3)            L'VALUE THAT IS IN ERROR                     
         BCTR  R6,0                                                             
         EX    R6,MVCERVAL         STICK IT IN                                  
ERR2     LA    R6,1(R6)                                                         
         AR    R7,R6               POINT TO INSERTION POINT FOR 2ND 1/2         
         ZIC   R6,0(R5)            L'SECOND HALF OF MSG                         
         BCTR  R6,0                                                             
         EX    R6,MVCMSG2          MOVE IT TO LOGHEAD                           
         B     XIT                                                              
         SPACE 1                                                                
MVC20    MVC   SAVE20(0),0(R4)                                                  
MVCNAME  MVC   8(0,R3),ACNMNAME                                                 
MVCSVNM  MVC   SAVENAME+1(0),ACNMNAME                                           
MVCMSG1  MVC   0(0,R7),1(R4)                                                    
MVCMSG2  MVC   0(0,R7),1(R5)                                                    
MVCERVAL MVC   0(0,R7),22(R3)                                                   
MVCERKEY MVC   0(0,R7),12(R3)                                                   
MVCSTAR  MVC   0(0,R6),FILLCHAR                                                 
MVCDATA  MVC   0(0,R6),8(R2)                                                    
         EJECT                                                                  
*              INITIALIZE NAME FIELDS AND FIELD HEADERS                         
*                                                                               
INTFLD1  MVC   FALCNAM,SPACES                                                   
         OI    FALCNAMH+6,X'80'                                                 
         NI    FALDIVH+4,X'DF'                                                  
INTFLD2  MVC   FALDNAM,SPACES                                                   
         OI    FALDNAMH+6,X'80'                                                 
         NI    FALODH+4,X'DF'                                                   
INTFLD3  MVC   FALONAM,SPACES                                                   
         OI    FALONAMH+6,X'80'                                                 
         NI    FALSDH+4,X'DF'                                                   
INTFLD4  MVC   FALSNAM,SPACES                                                   
         OI    FALSNAMH+6,X'80'                                                 
         NI    FALSTAH+4,X'DF'                                                  
INTFLD5  MVC   FALSTNO,SPACES                                                   
         OI    FALSTNOH+6,X'80'                                                 
         MVI   ANYKEY,C'Y'                                                      
         BR    RE                                                               
         SPACE 1                                                                
         GETEL R7,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
         SPACE                                                                  
**  TABLE OF FEE RULE PARAMETERS  **                                            
         SPACE                                                                  
TABLE    DS    0H                                                               
START    DC    CL7'START'          KEYWORD                                      
         DC    AL1(5)              LENGTH OF KEYWORD                            
         DC    AL4(EDSTART)        EDIT ROUTINE FOR THIS KEYWORD                
         DC    AL1(0)              MAXIMUM INTEGERS ALLOWED                     
         DC    AL1(0)              MAXIMUM DECIMAL PLACES ALLOWED               
         DC    AL1(*-START+1)      LENGTH OF TABLE ENTRY                        
HOURS    DC    CL7'HOURS'                                                       
         DC    AL1(5)                                                           
         DC    AL4(HILO)                                                        
         DC    AL1(4)              NNNN                                         
         DC    AL1(0)                                                           
         DC    AL1(*-HOURS+1)                                                   
OVA      DC    CL7'OVA'                                                         
         DC    AL1(3)                                                           
         DC    AL4(HILO)                                                        
         DC    AL1(1)              N.NNN                                        
         DC    AL1(3)                                                           
         DC    AL1(*-OVA+1)                                                     
OVB      DC    CL7'OVB'                                                         
         DC    AL1(3)                                                           
         DC    AL4(HILO)                                                        
         DC    AL1(1)              N.NNN                                        
         DC    AL1(3)                                                           
         DC    AL1(*-OVB+1)                                                     
OVC      DC    CL7'OVC'                                                         
         DC    AL1(3)                                                           
         DC    AL4(HILO)                                                        
         DC    AL1(1)              N.NNN                                        
         DC    AL1(3)                                                           
         DC    AL1(*-OVC+1)                                                     
BONUS    DC    CL7'BONUS'                                                       
         DC    AL1(3)                                                           
         DC    AL4(HILO)                                                        
         DC    AL1(2)              NN.NN                                        
         DC    AL1(2)                                                           
         DC    AL1(*-BONUS+1)                                                   
TYPE     DC    CL7'TYPE'                                                        
         DC    AL1(4)                                                           
         DC    AL4(EDTYPE)                                                      
         DC    AL1(3)                                                           
         DC    AL1(0)                                                           
         DC    AL1(*-TYPE+1)                                                    
MAX      DC    CL7'MAX'                                                         
         DC    AL1(3)                                                           
         DC    AL4(HILO)                                                        
         DC    AL1(3)              NNN                                          
         DC    AL1(0)                                                           
         DC    AL1(*-MAX+1)                                                     
WMAX     DC    CL7'WMAX'                                                        
         DC    AL1(4)                                                           
         DC    AL4(HILO)                                                        
         DC    AL1(1)              N                                            
         DC    AL1(0)                                                           
         DC    AL1(*-WMAX+1)                                                    
ADJUST   DC    CL7'ADJUST'                                                      
         DC    AL1(6)                                                           
         DC    AL4(EDADJUST)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(*-ADJUST+1)                                                  
RATE     DC    CL7'RATE'                                                        
         DC    AL1(4)                                                           
         DC    AL4(HILO)                                                        
         DC    AL1(3)              NNN.NNN                                      
         DC    AL1(3)                                                           
         DC    AL1(*-RATE+1)                                                    
PART     DC    CL7'PART'                                                        
         DC    AL1(4)                                                           
         DC    AL4(HILO)                                                        
         DC    AL1(2)              NN                                           
         DC    AL1(0)                                                           
         DC    AL1(*-PART+1)                                                    
         DC    X'FF'                                                            
*                                                                               
TYPETBLE DS    0H                                                               
         DC    CL4'ZERO'                                                        
         DC    CL4'NON '                                                        
         DC    CL4'100 '                                                        
         DC    CL4'90  '                                                        
         DC    CL4'66  '                                                        
         DC    CL4'50  '                                                        
         DC    CL4'33  ',X'FF'                                                  
*                                                                               
MONTBLE  DS    0H                                                               
         DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC',X'FF'                    
NUMTBLE  DS    0CL256              TRT TABLES                                   
         DC    240X'FF',10X'00',6X'FF'                                          
FILLCHAR DC    12C'*'                                                           
         SPACE                                                                  
******************* TABLE OF ERROR MESSAGES **********************              
         SPACE                                                                  
M1L      DC    AL1(L'M1)                                                        
M1       DC    C'*** ERROR *'                                                   
M2L      DC    AL1(L'M2)                                                        
M2       DC    C'* IS NOT A VALID KEYWORD'                                      
M3L      DC    AL1(L'M3)                                                        
M3       DC    C'* IS NOT A VALID MONTH'                                        
M4L      DC    AL1(L'M4)                                                        
M4       DC    C'* IS NOT NUMERIC'                                              
M5L      DC    AL1(L'M5)                                                        
M5       DC    C'* HAS TOO MANY DECIMAL PLACES'                                 
M6L      DC    AL1(L'M6)                                                        
M6       DC    C'*** ERROR - FEE RULE *'                                        
M7L      DC    AL1(L'M7)                                                        
M7       DC    C'* DUPLICATED'                                                  
M8L      DC    AL1(L'M8)                                                        
M8       DC    C'* - MONTHS MUST HAVE 3 LETTERS'                                
M9L      DC    AL1(L'M9)                                                        
M9       DC    C'* IS NOT A VALID TYPE'                                         
M10L     DC    AL1(L'M10)                                                       
M10      DC    C'* NOT VALID FOR ADJUST'                                        
M11L     DC    AL1(L'M11)                                                       
M11      DC    C'* HAS TOO MANY INTEGERS'                                       
M12L     DC    AL1(L'M12)                                                       
M12      DC    C'* - INVALID FORMAT (KEYWORD=VALUE)'                            
M13L     DC    AL1(L'M13)                                                       
M13      DC    C'* - DECIMAL PLACES NOT ALLOWED HERE'                           
         SPACE 1                                                                
INTOOLNG EQU   37                                                               
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFME3D                                                       
SAVEHIER DS    CL66                                                             
SAVE20   DS    CL38                                                             
SAVENAME DS    CL37                                                             
HOLDKEY  DS    CL32                                                             
STARKEY  DS    CL32                                                             
*        ACGENBOTH                                                              
*        ACGENFILE                                                              
*        ACLFMEQU                                                               
*        DDFLDIND                                                               
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE ACLFMEQU                                                       
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE ACLFMWORK                                                      
         SPACE                                                                  
RULED    DSECT                                                                  
KEYWORD  DS    CL7                 FEE RULE (KEYWORD)                           
KEYLEN   DS    AL1                 LENGTH OF KEYWORD                            
EDITRTN  DS    AL4                 A(EDIT ROUTINE FOR THAT RULE)                
MAXINT   DS    AL1                 MAX WHOLE NUMBERS ALLOWED                    
DECPOINT DS    AL1                 MAXIMUM DECIMAL PLACES ALLOWED               
LEN      DS    AL1                 LENGTH OF TABLE ENTRY                        
         SPACE                                                                  
FEED     DSECT                                                                  
FEEHEAD  DS    F                   A(CURRENT FEE HEADER)                        
SCANNER  DS    V                                                                
UNSCAN   DS    V                                                                
PRELOC   DS    F                                                                
SCANCNTR DS    PL4                                                              
ERRFLAG  DS    C                                                                
MYWORK   DS    CL2                 FOR HILO                                     
ELCODE   DS    CL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011ACLFM20   05/01/02'                                      
         END                                                                    
