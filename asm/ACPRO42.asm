*          DATA SET ACPRO42    AT LEVEL 102 AS OF 08/30/05                      
*PHASE T60B42A                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T60B42 - JOB ESTIMATE - VALIDATE+PROCESS COMMAND LINE'          
T60B42   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T60B42*,R7,RR=R2                                              
         ST    R2,RELO                                                          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
VALCOM   LA    R2,PROCOMH          R2=A(COMMAND FIELD HEADER)                   
         LA    R3,PROCOM           R3=EDIT POINTER                              
*                                                                               
* PARNSIP SCAN-NO NULLS, ATTRIBUTES OK, MULTIPLE VALUES OK                      
*                                                                               
         MVI   BYTE,PSNNONLQ+PSNAOKQ+PSNMVOKQ                                   
         GOTO1 PARSNIP,DMCB,(R2),BLOCK,(BYTE,COMMSEP)                           
         MVI   ERROR,INVALID                                                    
         CLI   4(R1),0                                                          
         BE    VALCOMR                                                          
*                                                                               
* SEE IF STRING STARTS WITH VALID COMMAND                                       
*                                                                               
         LA    R4,BLOCK                                                         
         USING PSND,R4                                                          
         L     R5,=A(COMMTAB)                                                   
         A     R5,RELO                                                          
         USING COMMTABD,R5                                                      
         CLI   PSNTAG,PSNFLDQ      TEST STRING STARTS WITH FIELD                
         BNE   VALCOMR                                                          
         CLI   PSNLEN,0            TEST USER ENTERED SPECIAL CHAR ONLY          
         BE    VALCOMR                                                          
*                                                                               
         L     R3,PSNCOMP          R3=A(FIRST COMPONENT)                        
         LA    R0,COMMANDS                                                      
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
*                                                                               
VALCOM2  CLC   PSNLEN,COMMMINL                                                  
         BL    VALCOM4                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),COMMNAME                                                 
         BE    VALCOM8                                                          
*                                                                               
VALCOM4  LA    R5,COMMTABL(R5)                                                  
         BCT   R0,VALCOM2                                                       
*                                                                               
VALCOM6  MVI   ERROR,BADCOMM                                                    
         B     VALCOMR                                                          
*                                                                               
* CALL EDITING ROUTINE FOR COMMAND TO SCAN REST OF STRING                       
*                                                                               
VALCOM8  CLC   AGYALPHA,=C'YN'                                                  
         BNE   VALCOM10                                                         
         OC    COMMAUTH,COMMAUTH   ANY SECURITY FOR THIS ACTION?                
         BZ    VALCOM10            NO                                           
         MVC   BYTE,AUTHOR         YES, GET SECURITY                            
         NC    BYTE,COMMAUTH       DO WE HAVE AUTHORITY?                        
         BZ    VALCOM6             NO                                           
*                                                                               
VALCOM10 ZIC   RE,PROCOMH+5        RE=COMMAND FIELD LENGTH                      
         LA    R1,1(R1)            RESTORE COMMAND LENGTH                       
         SR    RE,R1               RE=L'REST OF COMMAND STRING                  
         BP    VALCOM12            SOMETHING LEFT TO EDIT                       
*                                                                               
         SR    RE,RE               CANNOT SCAN FOR NEGATIVE LENGTH              
         STC   RE,LENSCAN                                                       
         LA    R3,1(R1,R3)         POINT 1 BYTE PAST COMMAND                    
         ST    R3,ASTSCAN          JUST IN CASE NOTHING MORE IS OK              
         B     VALCOM14                                                         
*                                                                               
VALCOM12 LA    R3,0(R1,R3)                                                      
         CLI   0(R3),C' '          EAT INTERVENING SPACES AFTER COMMAND         
         BNE   *+12                                                             
         LA    R3,1(R3)                                                         
         BCT   RE,*-12                                                          
         STC   RE,LENSCAN                                                       
         ST    R3,ASTSCAN          START SCAN ON NON-BLANK CHARACTER            
*                                                                               
* CALL COMMAND SPECIFIC ROUTINE TO EDIT REST OF LINE                            
*                                                                               
VALCOM14 SR    RF,RF                                                            
         ICM   RF,3,COMMEDRT       RF=DISP TO EDIT ROUTINE                      
         LA    RF,0(RB,RF)                                                      
         BASR  RE,RF               EDIT FOR COMMAND                             
*                                                                               
* CALL ROUTINE TO PERFORM COMMAND                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,COMMCMRT       RF=DISP TO COMMAND ROUTINE                   
         LA    RF,0(RB,RF)                                                      
         GOTO1 (RF),DMCB,(RC)                                                   
*                                                                               
         MVC   PROCOM,SPACES       CLEAR COMMAND LINE                           
         NI    PROCOMH+4,X'FF'-X'20' TURN OFF PREV VALID BIT                    
         OI    PROCOMH+6,X'80'     XMIT BACK                                    
         LA    R2,PROCLIH                                                       
         ST    R2,ACURFORC         SET CURSOR TO CLIENT FIELD                   
*                                                                               
VALCOMX  GOTO1 ERREX2              FORCE EARLY EXIT TO GENCON                   
*                                                                               
VALCOMR  LA    RE,PROCOM                                                        
         SR    R3,RE               COMPUTE DISP TO CURSOR                       
         STC   R3,ERRNDX                                                        
         B     ERREND                                                           
         DROP  R5                                                               
         SPACE 2                                                                
COMMSEP  DC    AL1(3),C'=+-'       COMMAND SEPARATORS                           
         DC    AL1(1),C' '                                                      
         DC    AL1(1),C' '                                                      
         DC    AL1(2),C', '                                                     
         EJECT                                                                  
***********************************************************                     
* SUB-ROUTINE TO EDIT FOR ADD COMMAND                     *                     
* SYNTAX IS ADD X WHERE X=ESTIMATE TYPE (P OR R)          *                     
***********************************************************                     
*                                                                               
EDADD    NTR1  ,                                                                
         MVI   ERROR,MISESTYP                                                   
         CLI   LENSCAN,0           TEST ANYTHING LEFT TO SCAN                   
         BNE   EDADD1              YES                                          
         BCTR  R3,0                                                             
         MVI   0(R3),C' '                                                       
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'*'          INSERT A MARKER IN COMMAND LINE              
         OI    6(R2),X'80'                                                      
         B     EDADDR                                                           
*                                                                               
EDADD1   MVI   BYTE,PSNVNOKQ+PSNNONLQ   NO VALUES, NO NULLS                     
         XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
         GOTO1 PARSNIP,DMCB,(LENSCAN,ASTSCAN),BLOCK,(BYTE,ADDSEP)               
         MVI   ERROR,BADESTYP                                                   
         CLI   8(R1),0             TEST FOR PARSNIP ERROR                       
         BE    EDADD2              NO                                           
         SR    R3,R3                                                            
         ICM   R3,7,9(R1)          GET ERROR LOCATION                           
         B     EDADDR                                                           
*                                                                               
EDADD2   LA    R4,BLOCK                                                         
         L     R3,PSNCOMP                                                       
         CLI   PSNTAG,PSNFLDQ      MUST BE A FIELD                              
         BNE   EDADDR                                                           
         CLI   PSNLEN,1            MUST BE ONE CHARACTER                        
         BNE   EDADDR                                                           
         CLI   0(R3),C'P'          TEST FOR PLANNING ESTIMATE                   
         BE    *+12                                                             
         CLI   0(R3),C'R'                                                       
         BNE   EDADDR                                                           
*                                                                               
         MVC   ADDTYPE,0(R3)                                                    
*                                                                               
EDADD4   ICM   R4,15,PSNFLD        NOTHING MORE ON LINE ALLOWED                 
         BZ    EDADDX                                                           
         L     R3,PSNCOMP                                                       
         MVI   ERROR,INVALID                                                    
         B     EDADDR                                                           
*                                                                               
EDADDX   B     XIT                                                              
*                                                                               
EDADDR   B     VALCOMR                                                          
*                                                                               
ADDSEP   DC    AL1(1),C'='         COMMAND SEPARATORS                           
         DC    AL1(1),C' '                                                      
         DC    AL1(1),C' '                                                      
         DC    AL1(1),C' '                                                      
         EJECT                                                                  
****************************************************************                
* SUB-ROUTINE TO EDIT COPY COMMAND STRING                      *                
* SYNTAX IS COPY FROM(+/- ADJ%) ESTIMATE TYPE                  *                
*   FROM=ESTIMATE EXPRESSION((FROM CLI/PRO/JOB))               *                
*        OR ACT((FROM CLI/PRO/JOB))                            *                
*        DEFAULT IS CLI,PRO,JOB ON SCREEN                      *                
*   OPTIONAL ADJUSTMENT (+NNN% OR -NN%)                        *                
*   TO ESTIMATE TYPE = P OR R                                  *                
****************************************************************                
         SPACE 1                                                                
EDCOPY   NTR1  ,                                                                
         CLI   LENSCAN,0                                                        
         BNE   EDCOPY1                                                          
         BCTR  R3,0                                                             
         MVI   0(R3),C' '                                                       
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'*'                                                       
         OI    6(R2),X'80'                                                      
         MVI   ERROR,SUPPLIED                                                   
         MVC   CONHEAD(L'MISSFROM),MISSFROM                                     
         B     EDCOPYR                                                          
*                                                                               
EDCOPY1  MVI   BYTE,PSNAOKQ+PSNNONLQ  ATTRIBUTES OK, NO NULLS                   
         XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
         GOTO1 PARSNIP,DMCB,(LENSCAN,ASTSCAN),BLOCK,(BYTE,COPYSEP)              
         MVI   ERROR,INVALID                                                    
         CLI   8(R1),0             TEST FOR PARSNIP ERROR                       
         BE    EDCOPY2                                                          
         SR    R3,R3                                                            
         ICM   R3,7,9(R1)          GET LOCATION OF PARSNIP ERROR                
         B     EDCOPYR                                                          
*                                                                               
EDCOPY2  LA    R4,BLOCK            R4=A(PARSNIP BLOCK)                          
         CLI   PSNTAG,PSNFLDQ      FIRST COMPONENT MUST BE A FIELD              
         BNE   EDCOPYR                                                          
         BAS   RE,EDDATA           LOOK FOR TYPE OF DATA TO BE COPIED           
*                                                                               
EDCOPY4  MVC   ADJOP,PSNVSEP       VALUE SEPARATOR=PERCENTAGE OPERATION         
         OC    PSNATTR,PSNATTR     TEST IF ATTRIBUTE PRESENT                    
         BZ    EDCOPY10            NO                                           
*                                                                               
* EDIT FOR FROM CLIENT, PRODUCT, JOB                                            
*                                                                               
EDCOPY5  BAS   RE,EDFROM           EDIT FOR FROM CLI,PRO,JOB                    
*                                                                               
* EDIT FOR PERCENTAGE ADJUSTMENT (+NNN% OR -NN%)                                
*                                                                               
EDCOPY10 ZIC   R1,PSNLEN           BUMP POINTER AHEAD IN CASE OF ERROR          
         LA    R3,0(R1,R3)                                                      
         LA    R4,PSNL(R4)                                                      
         CLI   PSNTAG,PSNVALQ      TEST IF VALUE PRESENT                        
         BNE   EDCOPY15            NO                                           
*                                                                               
         BAS   RE,EDPER            EDIT THE PERCENTAGE ADJUSTMENT               
         ZIC   R1,PSNLEN                                                        
         LA    R3,0(R1,R3)         SET ERROR POSITION                           
         LA    R4,PSNL(R4)         NEXT PARSNIP BLOCK                           
*                                                                               
* VALIDATE ESTIMATE TYPE TO BE CREATED BY COPY                                  
*                                                                               
EDCOPY15 MVI   ERROR,MISESTYP                                                   
         CLI   PSNTAG,0            TEST FOR END OF PARSNIP TABLE                
         BNE   EDCOPY16                                                         
         MVI   0(R3),C' '                                                       
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'*'                                                       
         OI    6(R2),X'80'                                                      
         B     EDCOPYR                                                          
*                                                                               
EDCOPY16 L     R3,PSNCOMP                                                       
         MVI   ERROR,BADESTYP                                                   
         CLI   PSNLEN,1                                                         
         BNE   EDCOPYR                                                          
         MVC   ADDTYPE,0(R3)                                                    
         CLI   0(R3),C'P'                                                       
         BE    *+12                                                             
         CLI   0(R3),C'R'                                                       
         BNE   EDCOPYR                                                          
*                                                                               
EDCOPY20 ST    R3,ADIFFSCH                                                      
         LA    R4,PSNL(R4)         LOOK FOR ANOTHER FIELD                       
         CLI   PSNTAG,0            TEST FOR END OF SCAN                         
         BE    EDCOPY25            YES                                          
*                                                                               
         L     R3,PSNCOMP                                                       
         MVI   ERROR,INVALID                                                    
         CLI   PSNTAG,PSNFLDQ      TEST ITS A FIELD                             
         BNE   EDCOPYR                                                          
         CLI   PSNLEN,4            TEST FOR 4-9 CHARACTERS                      
         BL    EDCOPYR                                                          
         CLI   PSNLEN,9                                                         
         BH    EDCOPYR                                                          
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),=C'NEWSCHEME'                                            
         BNE   EDCOPYR                                                          
         MVI   NEWSCHSW,C'Y'                                                    
*                                                                               
EDCOPY25 L     R5,ACOLIST                                                       
         USING JBCLD,R5                                                         
         CLC   JBCLCN1,ACTUALS     TEST FOR ACTUALS COPY                        
         BE    EDCOPY30            YES                                          
*                                                                               
         SR    R0,R0               NO-CHECK THAT ESTIMATE IS ON FILE            
         OC    FROMCLI,FROMCLI                                                  
         BZ    *+8                                                              
         LA    R0,1                                                             
         GOTO1 AVALEST,DMCB,(RC),(R0)                                           
         BE    EDCOPY30            OK                                           
         L     R3,AFROMEST                                                      
         B     EDCOPYR                                                          
*                                                                               
EDCOPY30 OC    FROMCLI,FROMCLI     TEST COPYING FROM ANOTHER JOB                
         BZ    EDCOPYX             NO                                           
*                                                                               
         MVC   GOADM,DATAMGR       READ 'TO' JOB'S OPTIONS                      
         MVC   GOSELCUL,CUL                                                     
         MVC   GOSELCLI,CLICODE                                                 
         MVC   GOSELPRO,PRODCODE                                                
         MVC   GOSELJOB,JOBNUM                                                  
         MVI   GOWHICH,C'N'                                                     
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         MVC   JOBSCH,GOSCHEME     EXTRACT JOB'S SCHEME                         
*                                                                               
         MVC   GOSELCLI,FROMCLI    READ 'FROM' JOB'S OPTIONS                    
         MVC   GOSELPRO,FROMPRO                                                 
         MVC   GOSELJOB,FROMJOB                                                 
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         CLC   GOSCHEME,JOBSCH     TEST FROM SCHEME=TO SCHEME                   
         BE    EDCOPYX                                                          
         CLI   NEWSCHSW,C'Y'       TEST USER SAYS ITS OK                        
         BE    EDCOPYX                                                          
*                                                                               
         L     R3,ADIFFSCH         GIVE A WARNING MESSAGE                       
         MVI   1(R3),C' '                                                       
         LA    R3,2(R3)            POSITION FOR WARNING MESSAGE                 
         MVI   0(R3),C'*'                                                       
         OI    6(R2),X'80'                                                      
         MVC   CONHEAD(L'WARNSCH),WARNSCH                                       
         MVI   ERROR,SUPPLIED                                                   
         B     EDCOPYR                                                          
*                                                                               
EDCOPYX  B     XIT                                                              
*                                                                               
EDCOPYR  B     VALCOMR                                                          
         SPACE 2                                                                
COPYSEP  DC    AL1(2),C'+-'                                                     
         DC    AL1(1),C' '                                                      
         DC    AL1(1),C' '                                                      
         DC    AL1(1),C','                                                      
         EJECT                                                                  
****************************************************************                
* SUB-ROUTINE TO EDIT REPLACE COMMAND STRING                   *                
* SYNTAX IS REPLACE ESTIMATE WITH                              *                
*   ESTIMATE=ESTIMATE EXPRESSION                               *                
*   WITH=ESTIMATE EXPRESSION((FROM CLI/PRO/JOB))               *                
*        OR ACT((FROM CLI/PRO/JOB))                            *                
*        DEFAULT IS CLI,PRO,JOB ON SCREEN                      *                
*   OPTIONAL ADJUSTMENT (+NNN% OR -NN%)                        *                
****************************************************************                
         SPACE 1                                                                
EDREP    NTR1  ,                                                                
         CLI   LENSCAN,0                                                        
         BNE   EDREP1                                                           
*                                                                               
         BCTR  R3,0                                                             
         MVI   0(R3),C' '                                                       
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'*'                                                       
         OI    6(R2),X'80'                                                      
         MVI   ERROR,MISSEST                                                    
         B     EDREPR                                                           
*                                                                               
EDREP1   MVI   BYTE,PSNAOKQ+PSNNONLQ  ATTRIBUTES OK, NO NULLS                   
         XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
         GOTO1 PARSNIP,DMCB,(LENSCAN,ASTSCAN),BLOCK,(BYTE,REPSEP)               
         MVI   ERROR,INVALID                                                    
         CLI   8(R1),0                                                          
         BE    EDREP2                                                           
         SR    R3,R3               GET LOCATION OF PARSNIP PROBLEM              
         ICM   R3,7,9(R1)                                                       
         B     EDREPR                                                           
*                                                                               
EDREP2   LA    R4,BLOCK            R4=A(PARSNIP BLOCK)                          
         GOTO1 EDEST,DMCB,REPTYPE                                               
         CLI   GONEEDAE,C'Y'       TEST NEED APPROVED ESTIMATE                  
         BNE   EDREP3              NO                                           
*                                                                               
         GOTO1 AGETEST,DMCB,(RC),REPTYPE                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ELCODE,ACEAELQ                                                   
         BAS   RE,GETELIO                                                       
         BNE   EDREP3              ESTIMATE IS NOT APPROVED                     
         MVI   ERROR,APPVDERR      CANNOT UPDATE APPROVED ESTIMATE              
         B     EDREPR                                                           
*                                                                               
EDREP3   ZIC   R1,PSNLEN                                                        
         LA    R3,0(R1,R3)         POINT AHEAD IN CASE OF ERROR                 
         LA    R4,PSNL(R4)                                                      
         CLI   PSNTAG,0            TEST FOR EOT                                 
         BNE   EDREP4              NO                                           
         MVI   0(R3),C' '                                                       
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'*'          PUT MARKER IN FIELD                          
         OI    6(R2),X'80'                                                      
         MVI   ERROR,SUPPLIED                                                   
         MVC   CONHEAD(L'MISSFROM),MISSFROM                                     
         B     EDREPR                                                           
*                                                                               
EDREP4   BAS   RE,EDDATA           LOOK FOR TYPE OF DATA TO BE COPIED           
*                                                                               
EDREP6   MVC   ADJOP,PSNVSEP       VALUE SEPARATOR=PERCENTAGE OPERATION         
         OC    PSNATTR,PSNATTR     TEST IF ATTRIBUTE PRESENT                    
         BZ    EDREP10             NO                                           
*                                                                               
* EDIT FOR FROM CLIENT, PRODUCT, JOB                                            
*                                                                               
EDREP8   BAS   RE,EDFROM           EDIT FOR FROM CLI,PRO,JOB                    
*                                                                               
* EDIT FOR PERCENTAGE ADJUSTMENT (+NNN% OR -NN%)                                
*                                                                               
EDREP10  ZIC   R1,PSNLEN           BUMP POINTER AHEAD IN CASE OF ERROR          
         LA    R3,1(R1,R3)                                                      
         ST    R3,ADIFFSCH                                                      
         LA    R4,PSNL(R4)                                                      
         CLI   PSNTAG,0            TEST FOR EOT                                 
         BE    EDREP20             YES-FINISH UP                                
         CLI   PSNTAG,PSNVALQ      TEST IF VALUE PRESENT                        
         BNE   EDREP15             NO                                           
*                                                                               
         BAS   RE,EDPER            EDIT THE PERCENTAGE ADJUSTMENT               
         LA    R4,PSNL(R4)         NEXT PARSNIP BLOCK                           
         CLI   PSNTAG,0            TEST FOR EOT                                 
         BE    EDREP20             YES                                          
*                                                                               
* EDIT FOR 'NEWSCHEME' KEYWORD                                                  
*                                                                               
EDREP15  L     R3,PSNCOMP                                                       
         MVI   ERROR,INVALID                                                    
         CLI   PSNTAG,PSNFLDQ      TEST FOR FIELD                               
         BNE   EDREPR              NO-MUST BE A PROBLEM                         
         CLI   PSNLEN,4            TEST FOR 4-9 CHARACTERS                      
         BL    EDREPR                                                           
         CLI   PSNLEN,9                                                         
         BH    EDREPR                                                           
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),=C'NEWSCHEME'                                            
         BNE   EDREPR                                                           
         MVI   NEWSCHSW,C'Y'                                                    
*                                                                               
* COMPLETED EDIT, PERFORM ADDITIONAL CHECKS                                     
*                                                                               
EDREP20  L     R5,ACOLIST                                                       
         USING JBCLD,R5                                                         
         CLC   JBCLCN1,ACTUALS     TEST FOR ACTUALS COPY                        
         BE    EDREP30             YES                                          
*                                                                               
         SR    R0,R0               NO-CHECK THAT ESTIMATE IS ON FILE            
         OC    FROMCLI,FROMCLI                                                  
         BZ    *+8                                                              
         LA    R0,1                                                             
         GOTO1 AVALEST,DMCB,(RC),(R0)                                           
         BE    EDREP30             OK                                           
         L     R3,AFROMEST                                                      
         B     EDREPR                                                           
*                                                                               
EDREP30  OC    FROMCLI,FROMCLI     TEST COPYING FROM ANOTHER JOB                
         BZ    EDREPX              NO                                           
*                                                                               
         MVC   GOADM,DATAMGR       READ 'TO' JOB'S OPTIONS                      
         MVC   GOSELCUL,CUL                                                     
         MVC   GOSELCLI,CLICODE                                                 
         MVC   GOSELPRO,PRODCODE                                                
         MVC   GOSELJOB,JOBNUM                                                  
         MVI   GOWHICH,C'N'                                                     
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         MVC   JOBSCH,GOSCHEME     EXTRACT JOB'S SCHEME                         
*                                                                               
         MVC   GOSELCLI,FROMCLI    READ 'FROM' JOB'S OPTIONS                    
         MVC   GOSELPRO,FROMPRO                                                 
         MVC   GOSELJOB,FROMJOB                                                 
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         CLC   GOSCHEME,JOBSCH     TEST FROM SCHEME=TO SCHEME                   
         BE    EDREPX                                                           
         CLI   NEWSCHSW,C'Y'       TEST USER SAYS ITS OK                        
         BE    EDREPX                                                           
*                                                                               
         L     R3,ADIFFSCH         GIVE A WARNING MESSAGE                       
         MVC   CONHEAD(L'WARNSCH),WARNSCH                                       
         MVI   ERROR,SUPPLIED                                                   
         B     EDREPR                                                           
*                                                                               
EDREPX   B     XIT                                                              
*                                                                               
EDREPR   B     VALCOMR                                                          
         SPACE 2                                                                
REPSEP   DC    AL1(2),C'+-'                                                     
         DC    AL1(1),C' '                                                      
         DC    AL1(1),C' '                                                      
         DC    AL1(1),C','                                                      
         EJECT                                                                  
*************************************************************                   
* SUB-ROUTINE TO EDIT APPROVAL LINE                         *                   
* SYNTAX IS APP(ROVE) RNN (BY=PERSON ON=MMMDD OR MMMDDYY)   *                   
*    RNN=REVISION NUMBER                                    *                   
* TO REPLACE EXISTING APPROVAL TYPE APP(ROVE) CHANGE ETC.   *                   
*************************************************************                   
         SPACE 1                                                                
EDAPP    NTR1  ,                                                                
         MVI   ERROR,MISSEST                                                    
         CLI   LENSCAN,0                                                        
         BNE   EDAPP1              SOMETHING MORE TO SCAN                       
*                                                                               
         BCTR  R3,0                                                             
         MVI   0(R3),C' '                                                       
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'*'                                                       
         OI    6(R2),X'80'                                                      
         B     EDAPPR                                                           
*                                                                               
EDAPP1   MVI   ERROR,PRESAPP       CHECK IF ALLOWED M/F APPROVAL                
         CLI   GOAPPMFP,C'M'                                                    
         BNE   EDAPPR                                                           
*                                                                               
         XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
         GOTO1 PARSNIP,DMCB,(LENSCAN,ASTSCAN),BLOCK,('PSNNONLQ',APPSEP)         
         MVI   ERROR,INVALID                                                    
         CLI   8(R1),0                                                          
         BE    EDAPP2                                                           
         SR    R3,R3                                                            
         ICM   R3,7,9(R1)                                                       
         B     EDAPPR                                                           
*                                                                               
EDAPP2   LA    R4,BLOCK                                                         
         MVI   CHAPPSW,C'N'                                                     
         CLI   PSNLEN,6            TEST FOR LENGTH OF 6                         
         BNE   EDAPP4              NO                                           
*                                                                               
         L     R3,PSNCOMP                                                       
         CLC   =C'CHANGE',0(R3)    TEST FOR 'CHANGE'                            
         BNE   EDAPP4                                                           
         MVI   CHAPPSW,C'Y'        NOTE USER SAID OK TO CHANGE                  
         L     R4,PSNFLD           POINT TO NEXT FIELD                          
*                                                                               
* EDIT ESTIMATE TO BE APPROVED 'R1-R99'                                         
*                                                                               
EDAPP4   MVI   ERROR,BADEST                                                     
         CLI   PSNTAG,PSNFLDQ                                                   
         BNE   EDAPPR                                                           
*                                                                               
         L     R3,PSNCOMP                                                       
         CLI   PSNLEN,2            TEST AT LEAST TWO CHARACTERS                 
         BL    EDAPPR                                                           
         CLI   PSNLEN,3            AND NO MORE THAN 3 CHARACTERS                
         BH    EDAPPR                                                           
         CLI   0(R3),C'R'          TEST THAT ESTIMATE TYPE=R                    
         BE    EDAPP5                                                           
         CLI   0(R3),C'P'          TEST IF THEY INPUT PLANNING                  
         BNE   EDAPPR                                                           
         MVI   ERROR,APPLNERR      RETURN CANNOT APPROVE PLANNING               
         B     EDAPPR                                                           
*                                                                               
EDAPP5   ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         MVC   DUB,=8X'F0'                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   DUB(0),1(R3)                                                     
         CLC   DUB,=8X'F0'                                                      
         BNE   EDAPPR                                                           
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,1(0,R3)                                                      
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    EDAPPR                                                           
*                                                                               
         MVC   ESTTYPE,0(R3)                                                    
         STC   R0,ESTVERS                                                       
*                                                                               
EDAPP6   MVI   ERROR,NOTFOUND                                                   
         GOTO1 AGETEST,DMCB,(RC),ESTTYPE                                        
         BNE   EDAPPR                                                           
*                                                                               
         MVI   ERROR,CANTAPPR                                                   
         GOTO1 =A(CHKFUND),DMCB,(C'A',(RC)),RR=RELO                             
         BNE   EDAPPR                                                           
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('ACEAELQ',AIO),0                       
         CLI   12(R1),0                                                         
         BNE   EDAPP8                                                           
*                                                                               
         CLI   CHAPPSW,C'Y'        TEST USER SAID OK TO CHANGE                  
         BE    EDAPP8              YES                                          
*                                                                               
         MVI   ERROR,SUPPLIED                                                   
         MVC   CONHEAD(L'CHAPPMSG),CHAPPMSG                                     
         B     EDAPPR                                                           
*                                                                               
EDAPP8   LA    R4,PSNL(R4)                                                      
         CLI   PSNTAG,0            TEST FOR EOT                                 
         BE    EDAPPX              YES-ALL DONE                                 
*                                                                               
         L     R3,PSNCOMP                                                       
         MVI   ERROR,INVALID                                                    
         CLI   PSNLEN,2                                                         
         BNE   EDAPPR                                                           
         CLC   =C'BY',0(R3)        TEST FOR VALID KEYWORDS                      
         BE    EDAPP10                                                          
         CLC   =C'ON',0(R3)                                                     
         BE    EDAPP12                                                          
*                                                                               
         MVI   ERROR,SUPPLIED                                                   
         MVC   CONHEAD(L'APPMSG),APPMSG                                         
         B     EDAPPR                                                           
*                                                                               
EDAPP10  MVI   ERROR,DUPINPUT                                                   
         OC    APPBY,APPBY                                                      
         BNZ   EDAPPR                                                           
         MVI   ERROR,MISSING                                                    
         LA    R3,3(R3)                                                         
         ICM   R4,15,PSNVAL        GET A(VALUE)                                 
         BZ    EDAPPR                                                           
         L     R3,PSNCOMP                                                       
         MVI   ERROR,INVALID                                                    
         CLI   PSNLEN,0                                                         
         BE    EDAPPR                                                           
         CLI   PSNLEN,8                                                         
         BH    EDAPPR                                                           
*                                                                               
         MVC   APPBY,SPACES                                                     
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APPBY(0),0(R3)                                                   
         B     EDAPP8              LOOK FOR ANOTHER KEY                         
*                                                                               
* EDIT FOR ON=MMMDD OR ON=MMMDDYY                                               
*                                                                               
EDAPP12  MVI   ERROR,DUPINPUT                                                   
         OC    APPDATE,APPDATE                                                  
         BNZ   EDAPPR                                                           
         LA    R3,3(R3)                                                         
         MVI   ERROR,MISSING                                                    
         ICM   R4,15,PSNVAL                                                     
         BZ    EDAPPR                                                           
         L     R3,PSNCOMP                                                       
         CLI   PSNLEN,0                                                         
         BE    EDAPPR                                                           
*                                                                               
         BAS   RE,EDDATE                                                        
         MVC   APPDATE,DUB                                                      
         B     EDAPP8              LOOK FOR ANOTHER KEY                         
*                                                                               
EDAPPX   B     XIT                                                              
         DROP  R5                                                               
*                                                                               
EDAPPR   B     VALCOMR                                                          
         SPACE 2                                                                
APPSEP   DC    AL1(1),C'='                                                      
         DC    AL1(1),C' '                                                      
         DC    AL1(1),C' '                                                      
         DC    AL1(1),C' '                                                      
         EJECT                                                                  
*****************************************************************               
* EDIT UNAPPROVE COMMAND LINE                                   *               
* SYNTAX IS UNA(PPROVE) EST  WHERE EST=RN,CE,OE                 *               
*****************************************************************               
         SPACE 1                                                                
EDUNA    NTR1  ,                                                                
         MVI   ERROR,MISSEST                                                    
         CLI   LENSCAN,0           TEST SOMETHING MORE TO SCAN                  
         BNE   EDUNA1                                                           
*                                                                               
         BCTR  R3,0                                                             
         MVI   0(R3),C' '                                                       
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'*'                                                       
         OI    6(R2),X'80'                                                      
         B     EDUNAR                                                           
*                                                                               
EDUNA1   MVI   ERROR,PRESAPP       CHECK IF ALLOWED M/F APPROVAL                
         CLI   GOAPPMFP,C'M'                                                    
         BNE   EDAPPR                                                           
*                                                                               
         XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
         GOTO1 PARSNIP,DMCB,(LENSCAN,ASTSCAN),BLOCK,('PSNNONLQ',UNASEP)         
         MVI   ERROR,INVALID                                                    
         CLI   8(R1),0                                                          
         BE    EDUNA2                                                           
         SR    R3,R3                                                            
         ICM   R3,7,9(R1)                                                       
         B     EDUNAR                                                           
*                                                                               
EDUNA2   LA    R4,BLOCK                                                         
         GOTO1 EDEST,PARAS,ESTTYPE EDIT FOR SINGLE ESTIMATE                     
         L     R3,PSNCOMP                                                       
         MVI   ERROR,BADEST                                                     
         CLI   ESTTYPE,ACEVREV     TEST FOR REVISION                            
         BNE   EDUNAR                                                           
         GOTO1 =A(CHKFUND),DMCB,(C'U',(RC)),RR=RELO                             
*                                                                               
EDUNA4   LA    R4,PSNL(R4)         CHECK FOR NO MORE INPUT                      
         CLI   PSNTAG,0                                                         
         BE    EDUNAX              STRING IF OVER                               
         MVI   ERROR,INVALID                                                    
         L     R3,PSNCOMP          SET ERROR POINTER                            
         B     EDUNAR                                                           
*                                                                               
EDUNAX   B     XIT                                                              
*                                                                               
EDUNAR   B     VALCOMR                                                          
         SPACE 2                                                                
UNASEP   DC    AL1(1),C'='                                                      
         DC    AL1(1),C' '                                                      
         DC    AL1(1),C' '                                                      
         DC    AL1(1),C' '                                                      
         EJECT                                                                  
*****************************************************************               
* EDIT DELETE COMMAND LINE                                      *               
* SYNTAX IS DEL(ETE) EST WHERE EST=ESTIMATE EXPRESSION          *               
*****************************************************************               
         SPACE 1                                                                
EDDEL    NTR1  ,                                                                
         MVI   ERROR,MISSEST                                                    
         CLI   LENSCAN,0           TEST SOMETHING MORE TO SCAN                  
         BNE   EDDEL1                                                           
*                                                                               
         BCTR  R3,0                                                             
         MVI   0(R3),C' '                                                       
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'*'                                                       
         OI    6(R2),X'80'                                                      
         B     EDDELR                                                           
*                                                                               
EDDEL1   XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
         GOTO1 PARSNIP,DMCB,(LENSCAN,ASTSCAN),BLOCK,('PSNNONLQ',DELSEP)         
         CLI   8(R1),0                                                          
         BE    EDDEL2                                                           
         MVI   ERROR,INVALID                                                    
         SR    R3,R3                                                            
         ICM   R3,7,9(R1)                                                       
         B     EDDELR                                                           
*                                                                               
EDDEL2   LA    R4,BLOCK                                                         
         GOTO1 EDEST,PARAS,ESTTYPE  EDIT FOR SINGLE ESTIMATE                    
*                                                                               
         MVI   ERROR,DELORERR                                                   
         CLC   ESTTYPE(2),ORGTYPE  TEST TRYING TO DELETE ORIGINAL EST           
         BE    EDDELR              YES-DON'T ALLOW DELETE FOR R1                
*                                                                               
         MVI   ERROR,CANTDFUN                                                   
         TM    JOBJSTA2,ACJBFUN    CAN'T DELETE IF ESTIMATE IS FUNDED           
         BZ    EDDEL4                                                           
         MVC   AIO,AIO3            GET JOB RECORD                               
         MVI   ELCODE,JFNELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   EDDEL3                                                           
*                                                                               
         USING JFNELD,R6                                                        
         CLC   JFNEST,ESTTYPE      IS THIS ESTIMATE FUNDED?                     
         BE    EDDELR              YES, CAN'T DELETE                            
*                                                                               
EDDEL3   MVC   AIO,AIO1            RESTORE AIO AREA                             
*                                                                               
EDDEL4   LA    R4,PSNL(R4)                                                      
         CLI   PSNTAG,0            TEST FOR NO MORE INPUT                       
         BE    EDDELX              NOTHING MORE TO EDIT                         
         L     R3,PSNCOMP                                                       
         MVI   ERROR,INVALID                                                    
         B     EDDELR                                                           
*                                                                               
EDDELX   B     XIT                                                              
*                                                                               
EDDELR   B     VALCOMR                                                          
         SPACE 2                                                                
DELSEP   DC    AL1(1),C'='                                                      
         DC    AL1(1),C' '                                                      
         DC    AL1(1),C' '                                                      
         DC    AL1(1),C' '                                                      
         EJECT                                                                  
*********************************************************************           
* EDIT PREPARED COMMAND LINE                                        *           
* SYNTAX IS PRE(PARED) EST=PERSON( DATE) + EST=ESTIMATE EXPRESSION  *           
*********************************************************************           
         SPACE 1                                                                
EDPREP   NTR1  ,                                                                
         MVI   ERROR,PRESPRP       CHECK IF ALLOWED M/F PREPARER                
         CLI   GOPRPMFP,C'M'                                                    
         BNE   EDPREPR                                                          
         MVI   ERROR,MISSEST                                                    
         CLI   LENSCAN,0           TEST FOR SOMETHING AFTER COMMAND             
         BNE   EDPREP1                                                          
*                                                                               
         BCTR  R3,0                                                             
         MVI   0(R3),C' '                                                       
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'*'                                                       
         OI    6(R2),X'80'                                                      
         B     EDPREPR             NO                                           
*                                                                               
EDPREP1  XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
         MVI   BYTE,PSNNONLQ+PSNMVOKQ  NO NULLS, MULTIPLE VALUES OK             
         GOTO1 PARSNIP,DMCB,(LENSCAN,ASTSCAN),BLOCK,(BYTE,PREPSEP)              
         CLI   8(R1),0                                                          
         BE    EDPREP2                                                          
         MVI   ERROR,INVALID                                                    
         SR    R3,R3                                                            
         ICM   R3,7,9(R1)                                                       
         B     EDPREPR                                                          
*                                                                               
EDPREP2  LA    R4,BLOCK            EDIT ESTIMATE EXPRESSION                     
         GOTO1 EDEST,PARAS,ESTTYPE                                              
*                                                                               
EDPREP4  ZIC   R1,PSNLEN           POSITION STRING POINTER AHEAD                
         LA    R3,0(R1,R3)                                                      
         LA    R4,PSNL(R4)                                                      
         CLI   PSNTAG,0            TEST FOR MORE INPUT                          
         BNE   EDPREP5             YES                                          
*                                                                               
         MVI   ERROR,MISSING       PERSON IS MISSING                            
         MVI   0(R3),C' '                                                       
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'*'                                                       
         B     EDPREPR             NO                                           
*                                                                               
EDPREP5  L     R3,PSNCOMP                                                       
         MVI   ERROR,INVALID                                                    
         CLI   PSNTAG,PSNFLDQ      TEST FOR FIELD                               
         BE    *+12                YES                                          
         CLI   PSNTAG,PSNVALQ      TEST FOR VALUE(S)                            
         BNE   EDPREPR             PROBLEM WITH SYNTAX                          
         MVI   ERROR,INP2LONG                                                   
         CLI   PSNLEN,L'PREPBY     TEST FOR OVERLY LONG NAME                    
         BH    EDPREPR                                                          
*                                                                               
         MVC   PREPBY,SPACES                                                    
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PREPBY(0),0(R3)                                                  
*                                                                               
EDPREP6  LA    R4,PSNL(R4)         LOOK FOR ANOTHER VALUE                       
         CLI   PSNTAG,0            TEST FOR END-OF-SCAN                         
         BE    EDPREPX             YES                                          
*                                                                               
         L     R3,PSNCOMP          LOOK FOR MMMDD OR MMMDDYY                    
         BAS   RE,EDDATE                                                        
         MVC   PREPDATE,DUB                                                     
*                                                                               
         LA    R4,PSNL(R4)                                                      
         CLI   PSNTAG,0            MAKE SURE NO MORE                            
         BE    EDPREPX                                                          
         L     R3,PSNCOMP                                                       
         MVI   ERROR,INVALID                                                    
         B     EDPREPR                                                          
*                                                                               
EDPREPX  B     XIT                                                              
*                                                                               
EDPREPR  B     VALCOMR                                                          
         SPACE 2                                                                
PREPSEP  DC    AL1(1),C'='                                                      
         DC    AL1(1),C' '                                                      
         DC    AL1(1),C' '                                                      
         DC    AL1(1),C' '                                                      
         EJECT                                                                  
***************************************************************                 
* EDIT SET COMMAND LINE                                       *                 
* SYNTAX IS   SET EST WC=PERCENT*CATEGORY                     *                 
***************************************************************                 
         SPACE 1                                                                
EDSET    NTR1  ,                                                                
         CLI   LENSCAN,0                                                        
         BNE   EDSET1                                                           
         BCTR  R3,0                                                             
         MVI   0(R3),C' '                                                       
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'*'                                                       
         OI    6(R2),X'80'                                                      
         MVI   ERROR,MISSEST       MISSING ESTIMATE                             
         B     EDSETR                                                           
*                                                                               
EDSET1   MVI   BYTE,PSNMVOKQ+PSNNONLQ  MULTIPLE VALUES OK, NO NULLS             
         XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
         GOTO1 PARSNIP,DMCB,(LENSCAN,ASTSCAN),BLOCK,(BYTE,SETSEP)               
         CLI   8(R1),0             TEST FOR ERROR                               
         BE    EDSET2                                                           
         MVI   ERROR,INVALID                                                    
         SR    R3,R3                                                            
         ICM   R3,7,9(R1)          LOCATION WHERE PARSNIP HAD A PROBLEM         
         B     EDSETR                                                           
*                                                                               
EDSET2   LA    R4,BLOCK                                                         
         GOTO1 EDEST,DMCB,ESTTYPE                                               
         GOTO1 AGETEST,DMCB,(RC),ESTTYPE                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   GONEEDAE,C'Y'       TEST NEED APPROVED EST OPTION                
         BNE   EDSET4              NO                                           
*                                                                               
         L     R3,PSNCOMP                                                       
         MVI   ELCODE,ACEAELQ                                                   
         BAS   RE,GETELIO          SEARCH FOR APPROVED ELEMENT                  
         BNE   EDSET4              NO                                           
         MVI   ERROR,APPVDERR                                                   
         B     EDSETR                                                           
*                                                                               
EDSET4   GOTO1 =A(LOOK),DMCB,(RC),RR=RELO                                       
         ZIC   R1,PSNLEN                                                        
         LA    R3,0(R1,R3)         POINT TO NEXT POSITION                       
         LA    R4,PSNL(R4)                                                      
         CLI   PSNTAG,0                                                         
         BNE   EDSET6                                                           
*                                                                               
         MVI   ERROR,MISSWC                                                     
         MVI   0(R3),C' '                                                       
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'*'                                                       
         OI    6(R2),X'80'                                                      
         B     EDSETR                                                           
*                                                                               
EDSET6   L     R3,PSNCOMP                                                       
         MVI   ERROR,INVALID                                                    
         CLI   PSNTAG,PSNFLDQ      TEST FOR FIELD                               
         BNE   EDSETR                                                           
         MVC   SETWC,0(R3)                                                      
         CLI   PSNLEN,2                                                         
         BH    EDSETR                                                           
         BE    *+8                                                              
         MVI   SETWC+1,C' '                                                     
*                                                                               
         L     R5,AIO2                                                          
         USING JBLOCKD,R5                                                       
         LH    R0,JBNROWS          R0=LOOP COUNTER                              
         L     R6,JBACOLTB                                                      
         USING JBCOLD,R6                                                        
*                                                                               
EDSET7   CLI   JBCOLTYP,JBCOLTWC   TEST FOR WORKCODE ITEM                       
         BNE   *+14                NO                                           
         CLC   JBCOLWC,SETWC                                                    
         BE    EDSET8              FOUND IT                                     
*                                                                               
         AH    R6,JBLCOL           NEXT ENTRY                                   
         BCT   R0,EDSET7                                                        
         MVI   ERROR,BADWORK                                                    
         B     EDSETR                                                           
*                                                                               
EDSET8   ZIC   R1,PSNLEN                                                        
         LA    R3,0(R1,R3)                                                      
         LA    R4,PSNL(R4)                                                      
         CLI   PSNTAG,0                                                         
         BNE   EDSET9                                                           
*                                                                               
         MVI   ERROR,MISSPERC      MISSING PERCENTAGE                           
         MVI   0(R3),C'='                                                       
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'*'                                                       
         OI    6(R2),X'80'                                                      
         B     EDSETR                                                           
*                                                                               
EDSET9   L     R3,PSNCOMP                                                       
         MVI   ERROR,INVALID                                                    
         CLI   PSNTAG,PSNVALQ      TEST FOR VALUE FIELD                         
         BNE   EDSETR              NO-SOMETHING WRONG                           
*                                                                               
         ZIC   R6,PSNLEN                                                        
         GOTO1 CASHVAL,DMCB,(X'84',(R3)),(R6)                                   
         MVI   ERROR,BADPERC                                                    
         CLI   0(R1),X'FF'                                                      
         BE    EDSETR                                                           
         ZAP   DUB,4(8,R1)         PERCENT TO 4 DEC PLS.                        
*                                                                               
EDSET10  LA    R3,0(R6,R3)                                                      
         LA    R4,PSNL(R4)                                                      
         CLI   PSNTAG,0                                                         
         BNE   EDSET11                                                          
*                                                                               
         MVI   ERROR,MISSCAT                                                    
         MVI   0(R3),C'*'          MULTIPLICATION SIGN                          
         LA    R3,1(R3)                                                         
         MVI   0(R3),C' '                                                       
         OI    6(R2),X'80'                                                      
         B     EDSETR                                                           
*                                                                               
EDSET11  MVI   ERROR,INVALID                                                    
         L     R3,PSNCOMP                                                       
         CLI   PSNTAG,PSNVALQ                                                   
         BNE   EDSETR                                                           
         MVC   HALF,0(R3)          EXTRACT POTENTIAL CATEGORY CODE              
         CLI   PSNLEN,2                                                         
         BH    EDSETR                                                           
         BE    *+8                                                              
         MVI   HALF+1,C' '                                                      
*                                                                               
         L     R5,AIO2                                                          
         USING JBLOCKD,R5                                                       
         LH    R0,JBNROWS          R0=LOOP COUNTER                              
         L     R6,JBACOLTB         R6=A(COLUMN OUTPUT TABLE)                    
         USING JBCOLD,R6                                                        
*                                                                               
EDSET12  CLI   JBCOLTYP,JBCOLTCT                                                
         BL    EDSET13                                                          
         CLI   JBCOLTYP,JBCOLTCF                                                
         BH    EDSET13                                                          
         CLC   JBCOLCAT,HALF                                                    
         BE    EDSET14                                                          
*                                                                               
EDSET13  AH    R6,JBLCOL                                                        
         BCT   R0,EDSET12                                                       
         MVI   ERROR,BADCAT                                                     
         B     EDSETR                                                           
*                                                                               
EDSET14  ZAP   WORK(16),JBCOLVAL   GET COLUMN VALUE                             
         MP    WORK(16),DUB        MULTIPLY BY PERCENTAGE                       
         SRP   WORK(16),64-6,5     SHIFT/ROUND TO 2 DEC PLS                     
         ZAP   SETAMT,WORK(16)                                                  
*                                                                               
EDSETX   B     XIT                                                              
*                                                                               
EDSETR   B     VALCOMR                                                          
         DROP  R5,R6                                                            
         SPACE 2                                                                
SETSEP   DC    AL1(1),C'='                                                      
         DC    AL1(1),C' '                                                      
         DC    AL1(2),C'*',C','                                                 
         DC    AL1(1),C' '                                                      
         EJECT                                                                  
*************************************************************                   
* SUB-ROUTINE TO EDIT INQUIRY LINE                          *                   
* SYNTAX IS I(NQUIRY) (TYPE)  WHERE TYPE =P,R,S,SN          *                   
*************************************************************                   
         SPACE 1                                                                
EDINQ    NTR1  ,                                                                
         XC    INQTYPE,INQTYPE                                                  
         CLI   LENSCAN,0           JUST AN 'I', NOTHING TO SCAN                 
         BE    EDINQX                                                           
*                                                                               
         MVI   BYTE,PSNVNOKQ+PSNNONLQ                                           
         XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
         GOTO1 PARSNIP,DMCB,(LENSCAN,ASTSCAN),BLOCK,(BYTE,INQSEP)               
         MVI   ERROR,INVALID                                                    
         CLI   8(R1),0                                                          
         BE    EDINQ2                                                           
         SR    R3,R3                                                            
         ICM   R3,7,9(R1)                                                       
         B     EDINQR                                                           
*                                                                               
EDINQ2   LA    R4,BLOCK                                                         
         L     R3,PSNCOMP                                                       
         CLI   PSNTAG,PSNFLDQ      MUST BE A FIELD                              
         BNE   EDINQR                                                           
*                                                                               
         CLI   PSNLEN,1            TEST FOR ONE CHARACTER TYPE                  
         BNE   EDINQ4                                                           
*                                                                               
         MVC   INQTYPE,0(R3)                                                    
         CLI   0(R3),C'P'          TEST FOR PLANNING ESTIMATE                   
         BE    EDINQ6                                                           
         CLI   0(R3),C'R'                                                       
         BE    EDINQ6                                                           
*                                                                               
         ST    R3,FULL             SAVE ERROR POINTER                           
         CLI   0(R3),C'S'          TEST S=SUPPLEMENT                            
         BE    EDINQ6                                                           
         B     EDINQR                                                           
*                                                                               
* EDIT FOR SN WHERE N=SUPPLEMENT NUMBER                                         
*                                                                               
EDINQ4   CLI   0(R3),C'S'          TEST 'S'=SUPPLEMENT                          
         BNE   EDINQR                                                           
*                                                                               
         MVC   INQTYPE,0(R3)                                                    
         ST    R3,FULL             SAVE A(INQUIRY TYPE)                         
         MVI   ERROR,INP2LONG                                                   
         CLI   PSNLEN,4            CAN'T BE MORE THAN 3 DIGITS                  
         BH    EDINQR                                                           
         LA    R3,1(R3)            POINT AHEAD TO SUPPLEMENT NUMBER             
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         MVC   DUB,=8X'F0'         SET UP TO TEST FOR NUMERICS                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   DUB(0),0(R3)                                                     
         MVI   ERROR,NOTNUM                                                     
         CLC   DUB,=8X'F0'         TEST FOR NUMERICS                            
         BNE   EDINQR                                                           
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R3)                                                      
         CVB   R0,DUB                                                           
         MVI   ERROR,INVALID                                                    
         LTR   R0,R0                                                            
         BZ    EDINQR                                                           
         CH    R0,=H'254'                                                       
         BH    EDINQR                                                           
         STC   R0,INQSUPP          SAVE SUPPLEMENT NUMBER                       
*                                                                               
EDINQ6   CLI   INQTYPE,C'S'        TEST FOR SUPPLEMENT INQUIRY                  
         BNE   EDINQ8              NO                                           
*                                                                               
         L     R3,FULL             RE-POSITION FOR ERROR                        
         GOTO1 ABLDLST,DMCB,(RC),('ACEVREV',AIO2)                               
         MVI   ERROR,NOREVERR                                                   
         CLI   NREVS,0             TEST FOR ANY REVISIONS                       
         BE    EDINQR              NO                                           
         CLI   INQSUPP,0           TEST FOR SPECIFIC SUPPLEMENT                 
         BE    EDINQ8              NO                                           
         CLC   INQSUPP,NREVS       TEST IF SUPPLEMENT EXISTS                    
         BL    EDINQ8              YES                                          
         MVI   ERROR,SUPPERR                                                    
         B     EDINQR                                                           
*                                                                               
EDINQ8   ICM   R4,15,PSNFLD        NOTHING MORE ON LINE ALLOWED                 
         BZ    EDINQX                                                           
         L     R3,PSNCOMP                                                       
         MVI   ERROR,INVALID                                                    
*                                                                               
EDINQR   B     VALCOMR                                                          
*                                                                               
EDINQX   MVC   PROCOM,SPACES       CLEAR COMMAND LINE                           
         MVI   PROCOMH+5,X'00'     AND LENGTH                                   
         NI    PROCOMH+4,X'FF'-X'20' TURN OFF PREV VALID BIT                    
         OI    PROCOMH+6,X'80'     XMIT BACK                                    
         B     XIT                                                              
         SPACE 2                                                                
INQSEP   DC    AL1(1),C'='         COMMAND SEPARATORS                           
         DC    AL1(1),C' '                                                      
         DC    AL1(1),C' '                                                      
         DC    AL1(1),C' '                                                      
         EJECT                                                                  
***************************************************************                 
* EDIT SUP COMMAND LINE                                       *                 
* SYNTAX IS   SUP                                             *                 
***************************************************************                 
         SPACE 1                                                                
EDSUP    NTR1  ,                                                                
         CLI   LENSCAN,0           TEST ANYTHING TO SCAN                        
         BE    EDSUP2              NO                                           
*                                                                               
         MVI   ERROR,INVALID                                                    
         B     EDSUPR                                                           
*                                                                               
EDSUP2   GOTO1 ABLDLST,DMCB,(RC),('ACEVREV',AIO2)                               
         MVI   ERROR,NOREVERR                                                   
         CLI   NREVS,0             TEST ANY REVISIONS                           
         BE    EDSUPR                                                           
         MVI   ERROR,INVALID                                                    
         CLI   HIRVERS,255                                                      
         BE    EDSUPR                                                           
*                                                                               
EDSUPX   B     XIT                                                              
*                                                                               
EDSUPR   B     VALCOMR                                                          
         EJECT                                                                  
* SUB-ROUTINE TO EDIT A SINGLE ESTIMATE EXPRESSION                              
* AT ENTRY, P1=A(OUTPUT AREA), R4=A(PARSNIP BLOCK)                              
*                                                                               
EDEST    NTR1  ,                                                                
         USING PSND,R4                                                          
         L     R6,0(R1)            R6=A(OUTPUT)                                 
         MVI   ERROR,BADEST                                                     
         CLI   PSNTAG,PSNFLDQ                                                   
         BNE   EDESTR                                                           
         L     R3,PSNCOMP                                                       
         BAS   RE,BLDFLD                                                        
         GOTO1 VJOBCOL,DMCB,(1,WORK),ACOLIST,ACOMFACS                           
         CLI   4(R1),0                                                          
         BE    EDESTR                                                           
*                                                                               
EDEST2   L     R5,ACOLIST                                                       
         USING JBCLD,R5                                                         
         CLI   JBCLTYP,JBCLCOL                                                  
         BNE   EDESTR                                                           
         CLC   JBCLCN1,ORIGEST     TEST ORIGINAL ESTIMATE                       
         BE    EDEST4                                                           
         CLC   JBCLCN1,CURREST     TEST CURRENT ESTIMATE                        
         BE    EDEST6                                                           
         CLC   JBCLCN1,REGEST      TEST REGULAR ESTIMATE                        
         BNE   EDESTR                                                           
*                                                                               
         GOTO1 AVALEST,DMCB,(RC),0                                              
         BNE   EDESTR                                                           
         MVC   0(2,R6),JBCLCN1E                                                 
         B     EDESTX                                                           
*                                                                               
EDEST4   GOTO1 AVALEST,DMCB,(RC),0                                              
         BNE   EDESTR                                                           
         MVC   0(2,R6),ORGTYPE                                                  
         B     EDESTX                                                           
*                                                                               
EDEST6   GOTO1 AVALEST,DMCB,(RC),0                                              
         BNE   EDESTR                                                           
         MVC   0(2,R6),CURTYPE                                                  
*                                                                               
EDESTX   B     XIT                                                              
*                                                                               
EDESTR   B     VALCOMR                                                          
         SPACE 2                                                                
* SUB-ROUTINE TO EDIT FOR COPY FROM DATA--EITHER ESTIMATE OR ACTUALS            
* AT ENTRY, R4=A(PARSNIP BLOCK), ON EXIT, ACOLIST POINTS TO COLUMN              
* LIST AND AFROMEST=A(EXPRESSION)                                               
*                                                                               
EDDATA   ST    RE,SAVERE                                                        
         L     R3,PSNCOMP          R3=A(FROM ESTIMATE EXPRESSION)               
         ST    R3,AFROMEST         SAVE LOCATION                                
         MVI   ERROR,BADEST                                                     
         CLI   PSNTAG,PSNFLDQ      TEST FOR FIELD                               
         BNE   EDDATAR                                                          
         BAS   RE,BLDFLD           BUILD DUMMY FIELD HEADER                     
         GOTO1 VJOBCOL,DMCB,(1,WORK),ACOLIST,ACOMFACS                           
         CLI   4(R1),0                                                          
         BE    EDDATAR                                                          
*                                                                               
         L     R5,ACOLIST                                                       
         USING JBCLD,R5                                                         
         CLI   JBCLTYP,JBCLCOL     TEST FOR SINGLE COLUMN                       
         BNE   EDDATAR                                                          
         CLC   JBCLCN1,ORIGEST     CHECK DATA NUMBER FOR NET                    
         BE    EDDATA4                                                          
         CLC   JBCLCN1,CURREST                                                  
         BE    EDDATA4                                                          
         CLC   JBCLCN1,REGEST                                                   
         BE    EDDATA4                                                          
         CLC   JBCLCN1,ACTUALS                                                  
         BE    EDDATA4                                                          
         B     EDDATAR                                                          
*                                                                               
EDDATA4  L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
EDDATAR  B     VALCOMR                                                          
         SPACE 2                                                                
* SUB-ROUTINE TO EDIT FOR OVERRIDE FROM CLIENT,PRODUCT,JOB AS                   
* ATTRIBUTE FIELDS.  CALLED FROM EDCOPY AND EDREP.  AT ENTRY,                   
* R4=A(PARSNIP BLOCK POINTING TO ATTRIBUTE).  ON EXIT, FROMCLI,                 
* FROMPRO, AND FROMJOB CONTAIN OVERRIDE CODES                                   
*                                                                               
EDFROM   ST    RE,SAVERE           WANT TO RETURN ALL REGISTERS                 
         L     R4,PSNATTR          R4=A(FIRST ATTRIBUTE ENTRY)                  
         L     R3,PSNCOMP          R3=A(FIRST ATTRIBUTE)                        
         MVI   ERROR,INP2LONG                                                   
         CLC   PSNLEN,LCLI         TEST CODE LONGER THAN A CLIENT               
         BH    EDFROMR             YES                                          
*                                                                               
         MVC   FROMCLI,SPACES      SPACE PAD 'FROM' CLIENT                      
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FROMCLI(0),0(R3)                                                 
*                                                                               
         MVI   ERROR,BADCLI                                                     
         MVC   KEY,SPACES                                                       
         MVC   KEY(3),CUL                                                       
         MVC   KEY+3(L'FROMCLI),FROMCLI                                         
         GOTO1 HIGH                                                             
         CLC   KEY(L'ACKEYACC),KEYSAVE TEST IF RECORD FOUND                     
         BNE   EDFROMR                                                          
*                                                                               
* EDIT FOR PRODUCT                                                              
*                                                                               
EDFROM2  LA    R3,2(R1,R3)         SET CURSOR POSITION                          
         MVI   ERROR,MISSPRO                                                    
         OC    PSNATTR,PSNATTR     TEST IF ATTRIBUTE FOLLOWS                    
         BZ    EDFROMR             NO-MUST BE AN ERROR                          
*                                                                               
         L     R4,PSNATTR                                                       
         L     R3,PSNCOMP                                                       
         MVI   ERROR,INP2LONG                                                   
         CLC   PSNLEN,LPRO         TEST ITEM LONGER THAN PRODUCT CODE           
         BH    EDFROMR                                                          
*                                                                               
         MVC   FROMPRO,SPACES                                                   
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FROMPRO(0),0(R3)                                                 
*                                                                               
         ZIC   RE,LCLI                                                          
         LA    RE,KEY+3(RE)                                                     
         MVC   0(L'FROMPRO,RE),FROMPRO                                          
         GOTO1 HIGH                                                             
         MVI   ERROR,BADPRO                                                     
         CLC   KEY(L'ACKEYACC),KEYSAVE                                          
         BNE   EDFROMR                                                          
*                                                                               
* EDIT FOR JOB                                                                  
*                                                                               
EDFROM4  ZIC   R1,PSNLEN                                                        
         LA    R3,1(R1,R3)         RESET CURSOR POSITION                        
         MVI   ERROR,MISSJOB                                                    
         OC    PSNATTR,PSNATTR                                                  
         BZ    EDFROMR                                                          
*                                                                               
         L     R4,PSNATTR                                                       
         L     R3,PSNCOMP                                                       
*                                                                               
         MVC   FROMJOB,SPACES                                                   
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FROMJOB(0),0(R3)                                                 
*                                                                               
         ST    R3,SVJOBAD                                                       
         ZIC   RE,LCLIPRO                                                       
         LA    RE,KEY+3(RE)                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),FROMJOB                                                  
         GOTO1 HIGH                                                             
         MVI   ERROR,BADJOB                                                     
         CLC   KEY(L'ACKEYACC),KEYSAVE                                          
         BNE   EDFROMR                                                          
*                                                                               
EDFROMX  L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
EDFROMR  B     VALCOMR                                                          
         SPACE 2                                                                
* SUB-ROUTINE TO EDIT A PARSNIP FIELD FOR A VALID DATE--EITHER                  
* MMMDD OR MMMDDYY                                                              
* AT ENTRY, R4=A(PARSNIP BLOCK), R3=A(COMPONENT)                                
* ON EXIT, DUB CONTAINS VALID DATE (PWO)                                        
*                                                                               
EDDATE   ST    RE,SAVERE                                                        
         MVI   ERROR,INVDATE                                                    
         MVC   BYTE,AGYLANG                                                     
         OI    BYTE,PVINSGLS       ALLOW ONLY A SINGLE DATE                     
         OI    BYTE,PVINSGLO                                                    
         GOTO1 PERVAL,DMCB,(PSNLEN,(R3)),(BYTE,WORK)                            
         CLI   4(R1),PVRCONE                                                    
         BNE   EDDATER                                                          
         LA    R1,WORK                                                          
         MVC   DUB(3),PVALPSTA-PERVALD(R1)                                      
*                                                                               
EDDATEX  L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
EDDATER  B     VALCOMR                                                          
         SPACE 2                                                                
* SUB-ROUTINE TO EDIT FOR PERCENTAGE ADJUSTMENT.  CALLED FROM                   
* EDCOPY AND EDREP.  AT ENTRY, R4=A(PARSNIP BLOCK) AND ADJOP                    
* CONTAINS ADJUSTMENT OPERATION (+ OR -).  ON EXIT, ADJ CONTAINS                
* ADJUSTMENT AMOUNT.                                                            
*                                                                               
EDPER    ST    RE,SAVERE                                                        
         L     R3,PSNCOMP          LOOK FOR PERCENTAGE                          
         MVI   ERROR,BADPER                                                     
         CLI   PSNLEN,4                                                         
         BH    EDPERR                                                           
*                                                                               
EDPER2   BAS   RE,BLDFLD                                                        
         ZIC   R1,WORK+5                                                        
         LA    RE,WORK+8-1(R1)                                                  
         CLI   0(RE),C'%'          TEST PERCENTAGE SIGN ENDS FIELD              
         BNE   *+14                                                             
         BCTR  R1,0                                                             
         MVI   0(RE),C' '                                                       
         STC   R1,WORK+5           REPLACE UPDATED LENGTH                       
*                                                                               
         MVC   DUB,=8X'F0'         TEST FOR NUMERIC VALUE                       
         ZIC   R1,WORK+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   DUB(0),WORK+8                                                    
         CLC   DUB,=8X'F0'                                                      
         BNE   EDPERR                                                           
*                                                                               
EDPER4   EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+8(0)                                                    
*                                                                               
         CP    DUB,=P'0'                                                        
         BE    EDPERR                                                           
*                                                                               
         LA    RE,=PL2'99'                                                      
         CLI   ADJOP,C'-'          TEST FOR MINUS PERCENTAGE                    
         BE    *+8                                                              
         LA    RE,=PL2'999'                                                     
*                                                                               
         CP    DUB,0(2,RE)         TEST AGAINST MAXIMUM VALUE                   
         BH    EDPERR                                                           
         CLI   ADJOP,C'+'          TEST FOR POSITIVE ADJUSTMENT                 
         BE    EDPER5                                                           
*                                                                               
         ZAP   WORK(8),=PL2'100'                                                
         SP    WORK(8),DUB         COMPUTE ADJUSTEMENT RELATIVE TO 100          
         ZAP   DUB,WORK(8)                                                      
         B     EDPER6                                                           
*                                                                               
EDPER5   AP    DUB,=PL2'100'                                                    
*                                                                               
EDPER6   ZAP   ADJ,DUB                                                          
*                                                                               
EDPERX   L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
EDPERR   B     VALCOMR                                                          
         SPACE 2                                                                
* SUB-ROUTINE TO BUILD A DUMMY FIELD HEADER AT WORK FOR A                       
* PARSNIP BLOCK ENTRY                                                           
*                                                                               
BLDFLD   ST    RE,FULL                                                          
         XC    WORK(8),WORK                                                     
         LA    RE,L'WORK                                                        
         STC   RE,WORK                                                          
         MVC   WORK+8(L'WORK-8),SPACES                                          
         L     RE,PSNCOMP                                                       
         ZIC   R1,PSNLEN                                                        
         STC   R1,WORK+5           SET INPUT LENGTH                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),0(RE)                                                  
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
* SUB-ROUTINE TO PERFORM ADD COMMAND                        *                   
*************************************************************                   
         SPACE 1                                                                
PERADD   ST    RE,SAVERE                                                        
         BAS   RE,INADD                                                         
         GOTO1 ADD                                                              
*                                                                               
         MVC   CONHEAD(8),=C'ESTIMATE'                                          
         MVC   CONHEAD+9(1),ADDTYPE                                             
         LA    R4,CONHEAD+10                                                    
         ZIC   R0,ADDVERS          GET VERSION NUMBER                           
         EDIT  (R0),(3,(R4)),ALIGN=LEFT                                         
         AR    R4,R0                                                            
         MVC   1(5,R4),=C'ADDED'                                                
*                                                                               
         GOTO1 =A(NEWCOL),DMCB,(RC),ADDTYPE,RR=RELO                             
*                                                                               
PERADDX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
*****************************************************************               
* SUB-ROUTINE TO PERFORM COPY COMMAND                           *               
*****************************************************************               
         SPACE 1                                                                
PERCPY   NTR1  ,                                                                
         MVI   CPYSW,C'C'          SET SWITCH TO COPY                           
         L     R5,ACOLIST                                                       
         USING JBCLD,R5                                                         
         LA    RF,CPYACT                                                        
         CLC   JBCLCN1,ACTUALS     TEST TO COPY ACTUAL CHARGES                  
         BE    *+8                 YES                                          
         LA    RF,CPYEST                                                        
         BASR  RE,RF                                                            
*                                                                               
PERCPY2  GOTO1 ADD                                                              
         CLC   JBCLCN1,ACTUALS     IF COPY ACTUAL, NO SESSION COPY              
         BE    PERCPY4                                                          
         GOTO1 =A(CPYSES),DMCB,(RC),RR=RELO                                     
*                                                                               
PERCPY4  MVC   CONHEAD(8),=C'ESTIMATE'                                          
         MVC   CONHEAD+9(1),ADDTYPE                                             
         LA    R3,CONHEAD+10                                                    
         ZIC   R0,ADDVERS          GET VERSION NUMBER                           
         EDIT  (R0),(3,(R3)),ALIGN=LEFT                                         
         AR    R3,R0                                                            
         MVC   1(15,R3),=C'CREATED BY COPY'                                     
*                                                                               
         CLI   CALLER,X'42'        TEST COMMAND MODULE CALLED                   
         BE    PERCPYX             YES                                          
         GOTO1 =A(NEWCOL),DMCB,(RC),ADDTYPE,RR=RELO                             
*                                                                               
PERCPYX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*****************************************************************               
* SUB-ROUTINE TO PERFORM REPLACE COMMAND                        *               
*****************************************************************               
         SPACE 1                                                                
PERREP   NTR1  ,                                                                
         MVI   CPYSW,C'R'                                                       
         L     R5,ACOLIST                                                       
         USING JBCLD,R5                                                         
         LA    RF,CPYACT                                                        
         CLC   JBCLCN1,ACTUALS     TEST TO COPY ACTUAL CHARGES                  
         BE    *+8                 YES                                          
         LA    RF,CPYEST                                                        
         BASR  RE,RF                                                            
*                                                                               
PERREP2  GOTO1 WRITE                                                            
         CLC   JBCLCN1,ACTUALS     IF REPLACE ACTUAL, NO SESSION                
         BE    PERREP4                                                          
         GOTO1 =A(CPYSES),DMCB,(RC),RR=RELO COPY SESSION ESTIMATE               
*                                                                               
PERREP4  MVC   CONHEAD(8),=C'ESTIMATE'                                          
         MVC   CONHEAD+9(1),REPTYPE                                             
         LA    R3,CONHEAD+10                                                    
         ZIC   R0,REPVERS          GET VERSION NUMBER                           
         EDIT  (R0),(3,(R3)),ALIGN=LEFT                                         
         AR    R3,R0                                                            
         MVC   1(8,R3),=C'REPLACED'                                             
*                                                                               
* RE-DISPLAY IF ESTIMATE IS IN COLUMN LIST, ADD TO LIST AND RE-DISPLAY          
* IF ITS NOT PRESENTLY IN THE LIST                                              
*                                                                               
PERREP6  SR    R0,R0                                                            
         ICM   R0,1,NCOLS          GET N'COLUMNS ON SCREEN                      
         BZ    PERREP9             NONE-SO ADD TO LIST                          
*                                                                               
         LA    R5,COLINDS                                                       
         USING COLD,R5                                                          
PERREP8  TM    COLFLAG,COLFEST     TEST IF COLUMN IS ESTIMATE                   
         BZ    *+14                NO                                           
         CLC   COLEST,REPTYPE      TEST IF ESTIMATE IS ON SCREEN                
         BE    PERREP10            YES-JUST CAUSE A RE-DISPLAY                  
         LA    R5,COLLENQ(R5)      NEXT ENTRY                                   
         BCT   R0,PERREP8                                                       
*                                                                               
PERREP9  GOTO1 =A(NEWCOL),DMCB,(RC),REPTYPE,RR=RELO                             
*                                                                               
PERREP10 GOTO1 =A(REDIS),DMCB,(RC),RR=RELO                                      
*                                                                               
PERREPX  B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO COPY AN ESTIMATE RECORD--CALLED FROM PERCPY + PERREP *         
***********************************************************************         
         SPACE 1                                                                
CPYEST   NTR1  ,                                                                
         L     R5,ACOLIST                                                       
         USING JBCLD,R5                                                         
         LA    R3,JBCLCN1E                                                      
         CLC   JBCLCN1,REGEST      TEST FOR REGULAR ESTIMATE                    
         BE    CPYEST2                                                          
         LA    R3,CURTYPE                                                       
         CLC   JBCLCN1,CURREST     TEST FOR CURRENT ESTIMATE                    
         BE    CPYEST2                                                          
         LA    R3,ORGTYPE                                                       
*                                                                               
CPYEST2  LA    R4,KEY                                                           
         USING ACEVKEY,R4                                                       
         XC    ACEVKEY,ACEVKEY                                                  
         MVI   ACEVRTYP,ACEVEQU                                                 
         MVI   ACEVSREC,ACEVSEQU                                                
         MVC   ACEVCUL,CUL                                                      
         MVC   ACEVCLI,CLICODE                                                  
         MVC   ACEVPROD,PRODCODE                                                
         MVC   ACEVJOB,JOBNUM                                                   
         OC    FROMCLI,FROMCLI     TEST COPYING FROM ANOTHER JOB                
         BZ    *+10                NO                                           
         MVC   ACEVCLI(18),FROMCLI                                              
         MVC   ACEVTYPE(2),0(R3)   TYPE/VERSION NUMBER                          
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLC   ACEVKEY,KEYSAVE                                                  
         BE    CPYEST4                                                          
*                                                                               
         MVI   ERROR,NOESJOB       NO ESTIMATE ON FILE FOR THIS JOB             
CPYEST3  L     R3,SVJOBAD                                                       
         B     VALCOMR                                                          
*                                                                               
CPYEST4  MVC   AIO,AIO1                                                         
         LA    RF,INADD            INITIALIZE NEW RECORD IF COPY                
         CLI   CPYSW,C'C'                                                       
         BE    *+8                                                              
         LA    RF,INREP            INITIALIZE FOR REPLACEMENT                   
         BASR  RE,RF                                                            
         L     R6,AIO2             R6=OLD ELEMENT POINTER                       
         LA    R6,ACRECORD-ACKEYD(R6)                                           
*                                                                               
CPYEST6  CLI   0(R6),0             TEST FOR EOR                                 
         BE    CPYESTX                                                          
         CLI   0(R6),EDAELQ                                                     
         BE    CPYEST8                                                          
*                                                                               
CPYEST7  ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CPYEST6                                                          
*                                                                               
         USING EDAELD,R6                                                        
CPYEST8  TM    EDATYPE,EDATWORK    TEST FOR WORKCODE TYPE                       
         BZ    CPYEST7             NO                                           
*                                                                               
         XC    ELEM,ELEM                                                        
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)       EXTRACT ELEMENT                              
*                                                                               
         ZAP   DUB,EDACOMM                                                      
         BAS   RE,ADJEST                                                        
         DROP  R6                                                               
         LA    R3,ELEM                                                          
         USING EDAELD,R3                                                        
         ZAP   EDACOMM,DUB                                                      
         CLI   EDALN-EDAELD(R6),EDALNQ1 TEST FOR ONE VALUE ONLY                 
         BNH   CPYEST10            YES                                          
         ZAP   DUB,EDANCOM-EDAELD(L'EDANCOM,R6)                                 
         BAS   RE,ADJEST                                                        
         ZAP   EDANCOM,DUB                                                      
         CLI   EDALN,EDALN4Q       DO WE HAVE HOURS?                            
         BL    CPYEST10            NO                                           
         OC    FROMCLI,FROMCLI     YES, ARE WE COPYING ACROSS JOBS?             
         BZ    CPYEST9             NO, CONTINUE ON                              
         MVI   ERROR,TIMEST        YES, SET ERROR MESSAGE                       
         B     CPYEST3             CAN'T COPY IF THERE IS TIME                  
*                                                                               
CPYEST9  MVI   EDATYPE,EDATWORK    FIX THE TYPE                                 
         MVI   EDALN,EDALNQ1       CHANGE THE LENGTH                            
         CP    EDANCOM,=P'0'       DO WE HAVE COMMISSION                        
         BE    *+8                                                              
         MVI   EDALN,EDALNQ2       YES, MAKE IT LONGER                          
*                                                                               
CPYEST10 GOTO1 ADDELEM                                                          
         B     CPYEST7             NEXT ELEMENT                                 
*                                                                               
CPYESTX  B     XIT                                                              
         DROP  R3,R4,R5                                                         
         SPACE 2                                                                
ADJEST   CLI   ADJOP,0             TEST FOR ADJUSTMENT NEEDED                   
         BER   RE                  NO                                           
         SRP   DUB,1,0                                                          
         ZAP   WORK(16),DUB                                                     
         MP    WORK(16),ADJ                                                     
         SRP   WORK(16),64-3,5                                                  
         ZAP   DUB,WORK(16)                                                     
         BR    RE                                                               
         EJECT                                                                  
******************************************************************              
* SUB-ROUTINE TO COPY THE ACTUAL CHARGES FROM A JOB INTO NEW     *              
* ESTIMATE--CALLED FROM PERCPY AND PERREP                        *              
******************************************************************              
         SPACE 1                                                                
CPYACT   NTR1  ,                                                                
         SR    R2,R2               R2=N'WORKCODE ENTRIES                        
*                                                                               
         MVC   KEY,SPACES                                                       
         LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC(3),CUL                                                  
         MVC   ACKEYACC+3(L'CLICODE),CLICODE                                    
         OC    FROMCLI,FROMCLI                                                  
         BZ    *+10                                                             
         MVC   ACKEYACC+3(L'CLICODE),FROMCLI                                    
*                                                                               
         ZIC   R1,LCLI                                                          
         LA    R1,ACKEYACC+3(R1)                                                
         MVC   0(L'PRODCODE,R1),PRODCODE                                        
         OC    FROMPRO,FROMPRO                                                  
         BZ    *+10                                                             
         MVC   0(L'FROMPRO,R1),FROMPRO                                          
*                                                                               
         ZIC   R0,LPRO                                                          
         AR    R1,R0                                                            
         MVC   0(L'JOBNUM,R1),JOBNUM                                            
         OC    FROMJOB,FROMJOB                                                  
         BZ    *+10                                                             
         MVC   0(L'JOBNUM,R1),FROMJOB                                           
*                                                                               
         MVI   ACKEYWRK,C'A'       START WITH TRANSACTIONS                      
         GOTO1 HIGH                                                             
         B     CPYACT2                                                          
*                                                                               
CPYACT1  LA    R4,KEY                                                           
         GOTO1 SEQ                                                              
*                                                                               
CPYACT2  CLC   ACKEYACC,KEYSAVE    TEST SAME JOB                                
         BNE   CPYACT4             NO                                           
         CLC   ACKEYWRK,=C'99'     TEST FOR BILL                                
         BE    CPYACT4             YES-WE'RE ALL DONE                           
*                                                                               
         L     R4,AIO                                                           
         CLI   ACRECORD,X'44'                                                   
         BNE   CPYACT1                                                          
         USING TRNRECD,R4                                                       
         CLI   DFTOPT,C'Y'         INCLUDE DRAFT & LIVE TRANSACTIONS ?          
         BE    CPYACT3             YES                                          
         TM    TRNRSTAT,TRNSDRFT   NO, IS THIS A DRAFT TRANSACTION ?            
         BZ    CPYACT2A            NO                                           
         CLI   DFTOPT,C'O'         YES, DO WE WANT DRAFTS ONLY ?                
         BE    CPYACT3             YES                                          
         B     CPYACT1             NO, SKIP IT                                  
*                                                                               
CPYACT2A CLI   DFTOPT,C'O'         THIS IS LIVE, ARE WE TAKING IT ?             
         BE    CPYACT1             NO, WANT DRAFTS ONLY                         
*                                                                               
CPYACT3  BAS   RE,GETACT                                                        
         B     CPYACT1                                                          
         DROP  R4                                                               
*                                                                               
         USING ACKEYD,R4                                                        
CPYACT4  LA    RF,INADD            INITIALIZE FOR COPY                          
         CLI   CPYSW,C'C'                                                       
         BE    *+8                                                              
         LA    RF,INREP                                                         
         BASR  RE,RF                                                            
         LTR   R2,R2               TEST ANY WORKCODES FOUND                     
         BZ    CPYACTX             NO                                           
         LA    R3,BUFF             R3=A(WORKCODE TABLE)                         
*                                                                               
CPYACT6  XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING ACEDD,R6                                                         
         MVI   ACEDEL,ACEDELQ                                                   
         MVI   ACEDLEN,ACEDLNQ1                                                 
         MVI   ACEDTYPE,1                                                       
         MVC   ACEDWORK,0(R3)                                                   
         ZAP   DUB,2(6,R3)         COMMISSIONABLE TOTAL                         
         BAS   RE,ADJEST                                                        
         ZAP   ACEDCOMM,DUB                                                     
*                                                                               
         CP    8(6,R3),=P'0'       TEST ANY NON-COMMISSIONABLE CHARGES          
         BE    CPYACT8                                                          
         ZAP   DUB,8(6,R3)                                                      
         BAS   RE,ADJEST                                                        
         ZAP   ACEDNCOM,DUB                                                     
         MVI   ACEDLEN,ACEDLNQ2    LARGE ELEMENT LENGTH                         
*                                                                               
CPYACT8  GOTO1 ADDELEM                                                          
         LA    R3,14(R3)           NEXT TABLE ENTRY                             
         BCT   R2,CPYACT6                                                       
*                                                                               
CPYACTX  B     XIT                                                              
         EJECT                                                                  
******************************************************************              
* SUB-ROUTINE TO GET THE ACTUAL CHARGE AND UPDATE WORKCODE TABLE *              
******************************************************************              
         SPACE 1                                                                
GETACT   NTR1  ,                                                                
         LA    R6,ACRECORD                                                      
         USING TRANSD,R6                                                        
         MVC   WORK(2),TRNSANAL                                                 
         ZAP   WORK+2(6),=P'0'                                                  
         ZAP   WORK+8(6),=P'0'                                                  
         LA    R1,WORK+2                                                        
         TM    TRNSSTAT,X'01'      TEST FOR NCOM CHARGE                         
         BZ    *+8                 NO                                           
         LA    R1,WORK+8                                                        
         ZAP   0(6,R1),TRNSAMNT                                                 
         TM    JOBJSTAT,ACJBXJOB   IS THIS AN X-JOB ?                           
         BZ    *+8                 NO                                           
         BAS   RE,GETXAMT          YES, GET AMOUTN FROM X'50' ELEMENT           
*                                                                               
         LA    RF,BUFF             RF=A(WORKCODE TABLE)                         
         LTR   RE,R2               SHIFT WORKCODE COUNT TO RE                   
         BZ    GETACT6             TABLE IS EMPTY                               
*                                                                               
GETACT2  CLC   WORK(2),0(RF)       TEST IF WORKCODE IS IN TABLE                 
         BE    GETACT4             YES                                          
         LA    RF,14(RF)                                                        
         BCT   RE,GETACT2                                                       
         B     GETACT6             ADD NEW ENTRY                                
*                                                                               
GETACT4  AP    2(6,RF),WORK+2(6)   UPDATE ENTRY                                 
         AP    8(6,RF),WORK+8(6)                                                
         B     GETACTX                                                          
*                                                                               
GETACT6  MVC   0(14,RF),WORK       SET NEW ENTRY                                
         LA    R2,1(R2)            INCREMENT ENTRY COUNT                        
*                                                                               
GETACTX  XIT1  REGS=(R2)                                                        
         EJECT                                                                  
* SUB-ROUTINE TO GET ACTUAL AMOUNT OF TRANSACTION FROM X'50', TYPE S,           
* ELEMENT FOR X-JOBS ONLY                                                       
*                                                                               
         USING TRCASHD,R6                                                       
GETXAMT  NTR1  ,                                                                
         MVI   ELCODE,TRCSELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    GETXAMT4                                                         
         B     GETXAMTX            USE TRNSAMNT IF NOT THERE                    
*                                                                               
GETXAMT2 SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         AR    R6,RF                                                            
         CLI   0(R6),0             END OF RECORD ?                              
         BE    GETXAMTX            YES                                          
         CLI   0(R6),TRCSELQ       NO, X'50' ELEMENT ?                          
         BNE   GETXAMT2            NO, KEEP LOOKING                             
*                                                                               
GETXAMT4 CLI   TRCSTYPE,C'S'       IS THIS THE EXPENSE AMOUNT ?                 
         BNE   GETXAMT2            NO, KEEP LOOKING                             
         ZAP   0(6,R1),TRCSAMNT    YES, SAVE IT                                 
*                                                                               
GETXAMTX B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*********************************************************************           
* SUB-ROUTINE TO INITIALIZE A NEW ESTIMATE RECORD TO ADD            *           
* CALLED FROM PERADD AND PERCPY, ADDTYPE CONTAINS ESTIMATE TYPE     *           
*********************************************************************           
         SPACE 1                                                                
INADD    NTR1  ,                                                                
         SR    R3,R3               R3=HIGHEST VERSION NUMBER                    
         LA    R4,KEY                                                           
         USING ACEVKEY,R4                                                       
         XC    ACEVKEY,ACEVKEY                                                  
         MVI   ACEVRTYP,ACEVEQU                                                 
         MVI   ACEVSREC,ACEVSEQU                                                
         MVC   ACEVCUL,CUL                                                      
         MVC   ACEVCLI,CLICODE                                                  
         MVC   ACEVPROD,PRODCODE                                                
         MVC   ACEVJOB,JOBNUM                                                   
         MVC   ACEVTYPE,ADDTYPE                                                 
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         B     INADD2                                                           
*                                                                               
INADD1   GOTO1 SEQ                                                              
*                                                                               
INADD2   CLC   ACEVKEY(ACEVERS-ACEVKEY),KEYSAVE                                 
         BNE   INADD4              FOUND HIGHEST ONE                            
         IC    R3,ACEVERS                                                       
         B     INADD1                                                           
*                                                                               
INADD4   NI    DMINBTS,X'FF'-X'08'                                              
         L     R4,AIO                                                           
         LR    RE,R4                                                            
         L     RF,SIZEIO                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   ACEVKEY,KEYSAVE                                                  
         LA    R3,1(R3)            INCREMENT HIGHEST VERSION                    
*                                                                               
         MVI   ERROR,MAXEST                                                     
         CH    R3,=H'99'           NO MORE THAN 99                              
         BH    ERREND                                                           
*                                                                               
         STC   R3,ACEVERS          SET IT IN NEW RECORD                         
         STC   R3,ADDVERS          AND SAVE IT IN STORAGE                       
         MVC   ACLENGTH,=Y(ACRECORD-ACKEYD+1) INITIALIZE RECORD LENGTH          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING EUPELD,R6                                                        
         MVI   EUPEL,EUPELQ        BUILD AN ESTIMATE UPDATE ELEMENT             
         MVI   EUPLN,EUPLNQ                                                     
         MVC   EUPADD,TODAYP                                                    
         MVC   EUPERS,TWAALIAS                                                  
         MVC   EUPLAST,TODAYP                                                   
         GOTO1 GETFACT,DMCB,(1,0)  GET CURRENT TIME                             
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   EUPTIME,FATIME+1    TIME IN BINARY SECONDS                       
         GOTO1 ADDELEM                                                          
*                                                                               
INADDX   B     XIT                                                              
         DROP  R1,R4,R6                                                         
         EJECT                                                                  
*********************************************************************           
* SUB-ROUTINE TO INITIALIZE FOR REPLACEMENT OF AN ESTIMATE          *           
* RECORD.  CALLED FROM CPYEST AND CPYACT                            *           
*********************************************************************           
         SPACE 1                                                                
INREP    NTR1  ,                                                                
         GOTO1 AGETEST,DMCB,(RC),REPTYPE                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ELCODE,ACEDELQ      HANDLE DATA ELEMENTS                         
         GOTO1 REMELEM                                                          
*                                                                               
         MVI   ELCODE,EUPELQ       HANDLE UPDATE ELEMENTS                       
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING EUPELD,R6                                                        
         MVI   EUPLN,EUPLNQ                                                     
         MVC   EUPERS,TWAALIAS                                                  
         MVC   EUPLAST,TODAYP                                                   
         GOTO1 GETFACT,DMCB,(1,0)  GET CURRENT TIME                             
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   EUPTIME,FATIME+1    TIME IN BINARY SECONDS                       
         GOTO1 ADDELEM                                                          
         DROP  R1,R6                                                            
*                                                                               
INREPX   B     XIT                                                              
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
*                                                                               
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
         SPACE 1                                                                
YESXIT   CR    RB,RB               SET CC=EQ AND EXIT                           
         B     XIT                                                              
         SPACE 1                                                                
NOXIT    LTR   RB,RB               SET CC=NEQ AND EXIT                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
MISSFROM DC    C'**MISSING THE ESTIMATE TO COPY FROM**'                         
CHAPPMSG DC    C'**ESTIMATE IS ALREADY APPROVED**'                              
APPMSG   DC    C'**TRY BY=PERSON OR ON=DATE**'                                  
WARNSCH  DC    C'**TO/FROM JOB SCHEMES DIFFER--TYPE ''NEWSCHEME'' TO PRX        
               OCEED**'                                                         
         SPACE 2                                                                
SVJOBAD  DS    A                                                                
RELO     DS    A(0)                                                             
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* TABLE OF COMMANDS                                                             
*                                                                               
         DS    0D                                                               
COMMTAB  DS    0CL(COMMTABL)                                                    
*                                                                               
         DC    CL8'ADD     ',AL1(3)                                             
         DC    AL2(EDADD-T60B42),AL2(PERADD-T60B42)                             
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'COPY    ',AL1(4)                                             
         DC    AL2(EDCOPY-T60B42),AL2(PERCPY-T60B42)                            
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'REPLACE ',AL1(3)                                             
         DC    AL2(EDREP-T60B42),AL2(PERREP-T60B42)                             
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'APPROVE ',AL1(3)                                             
         DC    AL2(EDAPP-T60B42),AL2(PERAPP-T60B42)                             
         DC    AL1(CAT3Q)                                                       
*                                                                               
         DC    CL8'DELETE  ',AL1(3)                                             
         DC    AL2(EDDEL-T60B42),AL2(PERDEL-T60B42)                             
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'UNAPPROV',AL1(3)                                             
         DC    AL2(EDUNA-T60B42),AL2(PERUNA-T60B42)                             
         DC    AL1(CAT3Q)                                                       
*                                                                               
         DC    CL8'PREPARED',AL1(3)                                             
         DC    AL2(EDPREP-T60B42),AL2(PERPREP-T60B42)                           
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'SET     ',AL1(3)                                             
         DC    AL2(EDSET-T60B42),AL2(PERSET-T60B42)                             
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'INQUIRY ',AL1(1)                                             
         DC    AL2(EDINQ-T60B42),AL2(PERINQ-T60B42)                             
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'SUP     ',AL1(3)                                             
         DC    AL2(EDSUP-T60B42),AL2(PERSUP-T60B42)                             
         DC    AL1(0)                                                           
*                                                                               
COMMANDS EQU   (*-COMMTAB)/L'COMMTAB                                            
         EJECT                                                                  
* SUB-ROUTINE TO PERFORM A JOB LOOKUP--CALLED BY EDSET                          
*                                                                               
LOOK     NMOD1 0,**LOOK                                                         
         L     RC,0(R1)                                                         
         L     R5,AIO2             USE IO2 AS JOBBLOCK AREA                     
         USING JBLOCKD,R5                                                       
         LR    RE,R5                                                            
         LA    RF,JBLOCKL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
LOOK1    XC    LISTAR,LISTAR       BUILD A DUMMY FIELD HEADER                   
         MVC   LISTAR+8(1),ESTTYPE FOR THE ESTIMATE                             
         ZIC   R0,ESTVERS                                                       
         EDIT  (R0),(3,LISTAR+9),ALIGN=LEFT                                     
         AH    R0,=H'1'            FORM INPUT LENGTH                            
         STC   R0,LISTAR+5                                                      
         MVI   LISTAR,L'LISTAR                                                  
         GOTO1 VJOBCOL,DMCB,(1,LISTAR),ACOLIST,ACOMFACS                         
         CLI   4(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                DUMP IF AN ERROR                             
*                                                                               
LOOK2    MVC   JBAJOB,AJOB                                                      
         MVC   JBACOLS,ACOLIST                                                  
         MVC   JBACOM,ACOMFACS                                                  
         LA    RE,GOBLOCK                                                       
         ST    RE,JBAGOBLK                                                      
         MVC   JBAIO,AIO1                                                       
         MVC   JBGETOPT,GETOPT                                                  
         MVC   JBSELSCH,JOBSCH                                                  
         MVI   JBSELFUN,JBGETEST   EXTRACT VALUE FROM ESTIMATE                  
*                                                                               
         L     R1,ATIA                                                          
         ST    R1,JBACOLTB                                                      
         L     RE,=A(LENTIA)                                                    
         SRL   RE,1                DIVIDE THE 14K TIA IN HALF                   
         ST    RE,JBLCOLTB                                                      
         ST    RE,JBLOPVTB                                                      
         LA    R1,0(RE,R1)         POINT TO SECOND HALF OF TIA                  
         ST    R1,JBAOPVTB         USE FOR OPERAND VALUE TABLE                  
         GOTO1 JOBBER,DMCB,JBLOCK                                               
         CLI   JBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LOOKX    XMOD1                                                                  
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**************************************************************                  
* SUB-ROUTINE TO PERFORM APPROVE COMMAND                     *                  
**************************************************************                  
         SPACE 1                                                                
PERAPP   NMOD1 0,*PERAPP*,RR=R6                                                 
         L     RC,0(R1)                                                         
         ST    R6,PARELO                                                        
         GOTO1 AGETEST,DMCB,(RC),ESTTYPE                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PERAPP2  MVI   ELCODE,ACEAELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING ACEAD,R6            BUILD APPROVAL ELEMENT                       
         MVI   ACEAEL,ACEAELQ                                                   
         MVI   ACEALEN,ACEALENQ                                                 
         MVC   ACEAINP,TODAYP      DATE INPUT                                   
*                                                                               
         MVC   ACEAPPBY,APPBY      SET PERSON APPROVING                         
         OC    APPBY,APPBY         TEST IF PERSON INPUT                         
         BNZ   *+10                YES                                          
         MVC   ACEAPPBY,TWAALIAS   NO-USE PERSON WHO INPUT                      
*                                                                               
         MVC   ACEADATE,APPDATE    SET APPROVAL DATE                            
         OC    ACEADATE,ACEADATE                                                
         BNZ   *+10                                                             
         MVC   ACEADATE,TODAYP     USE TODAY'S DATE AS A DEFAULT                
*                                                                               
         GOTO1 ADDELEM                                                          
         GOTO1 WRITE                                                            
*                                                                               
PERAPP4  MVC   CONHEAD(8),=C'ESTIMATE'                                          
         MVC   CONHEAD+9(1),ESTTYPE                                             
         LA    R4,CONHEAD+10                                                    
         ZIC   R0,ESTVERS                                                       
         EDIT  (R0),(3,(R4)),ALIGN=LEFT                                         
         AR    R4,R0                                                            
         MVC   1(8,R4),=C'APPROVED'                                             
         LA    R4,10(R4)                                                        
         MVC   0(2,R4),=C'BY'     ECHO BACK THE INPUT DATA                      
         LA    R4,3(R4)                                                         
         MVC   0(L'ACEAPPBY,R4),ACEAPPBY                                        
         LA    R4,L'ACEAPPBY+1(R4)                                              
         MVC   0(2,R4),=C'ON'                                                   
         LA    R4,3(R4)                                                         
         GOTO1 DATCON,DMCB,(1,ACEADATE),(17,(R4))                               
         OC    CONHEAD,SPACES                                                   
         GOTO1 SQUASHER,DMCB,CONHEAD,L'CONHEAD                                  
*                                                                               
* CHANGE COLUMN LIST AND RE-DISPLAY IF APPROVED ESTIMATE IS                     
* NOT ON THE SCREEN--JUST RE-DISPLAY IF ITS ON THE SCREEN                       
*                                                                               
PERAPP6  SR    R0,R0                                                            
         ICM   R0,1,NCOLS          GET N'COLUMNS ON SCREEN                      
         BZ    PERAPP9             NONE-SO ADD TO LIST                          
*                                                                               
         LA    R5,COLINDS                                                       
         USING COLD,R5                                                          
PERAPP8  TM    COLFLAG,COLFEST     TEST IF COLUMN IS ESTIMATE                   
         BZ    *+14                NO                                           
         CLC   COLEST,ESTTYPE      TEST IF ESTIMATE IS ON SCREEN                
         BE    PERAPP10            YES-JUST CAUSE A RE-DISPLAY                  
         LA    R5,COLLENQ(R5)      NEXT ENTRY                                   
         BCT   R0,PERAPP8                                                       
*                                                                               
PERAPP9  GOTO1 =A(NEWCOL),DMCB,(RC),ESTTYPE,RR=PARELO                           
*                                                                               
PERAPP10 GOTO1 =A(REDIS),DMCB,(RC),RR=PARELO                                    
*                                                                               
PERAPPX  XMOD1                                                                  
         DROP  R5,R6                                                            
         SPACE 2                                                                
PARELO   DS    A                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
**************************************************************                  
* SUB-ROUTINE TO PERFORM UNAPPROVE COMMAND                   *                  
**************************************************************                  
         SPACE 1                                                                
PERUNA   NMOD1 0,**UNAPPR,RR=R6                                                 
         L     RC,0(R1)                                                         
         ST    R6,PURELO                                                        
         GOTO1 AGETEST,DMCB,(RC),ESTTYPE                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PERUNA2  MVI   ELCODE,ACEAELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         GOTO1 WRITE                                                            
*                                                                               
PERUNA4  MVC   CONHEAD(8),=C'ESTIMATE'                                          
         MVC   CONHEAD+9(1),ESTTYPE                                             
         LA    R4,CONHEAD+10                                                    
         ZIC   R0,ESTVERS                                                       
         EDIT  (R0),(3,(R4)),ALIGN=LEFT                                         
         AR    R4,R0                                                            
         MVC   1(10,R4),=C'UNAPPROVED'                                          
*                                                                               
* CAUSE A RE-DISPLAY IF UNAPPROVED ESTIMATE IS ON THE SCREEN                    
*                                                                               
PERUNA6  SR    R0,R0                                                            
         ICM   R0,1,NCOLS          GET N'COLUMNS ON SCREEN                      
         BZ    PERUNAX             NONE-SO DON'T REDISPLAY                      
*                                                                               
         LA    R5,COLINDS                                                       
         USING COLD,R5                                                          
PERUNA8  TM    COLFLAG,COLFEST     TEST IF COLUMN IS ESTIMATE                   
         BZ    *+14                NO                                           
         CLC   COLEST,ESTTYPE      TEST IF ESTIMATE IS ON SCREEN                
         BE    PERUNA10            YES-JUST CAUSE A RE-DISPLAY                  
         LA    R5,COLLENQ(R5)      NEXT ENTRY                                   
         BCT   R0,PERUNA8                                                       
         B     PERUNAX                                                          
*                                                                               
PERUNA10 GOTO1 =A(REDIS),DMCB,(RC),RR=PURELO                                    
*                                                                               
PERUNAX  XMOD1 1                                                                
         SPACE 1                                                                
PURELO   DC    A(0)                                                             
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**************************************************************                  
* SUB-ROUTINE TO PERFORM DELETE COMMAND                      *                  
**************************************************************                  
         SPACE 1                                                                
PERDEL   NMOD1 0,**DELETE                                                       
         L     RC,0(R1)                                                         
         GOTO1 AGETEST,DMCB,(RC),ESTTYPE                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         OI    ACSTATUS,X'80'      TURN ON DELETE BIT                           
*                                                                               
         GOTO1 WRITE                                                            
*                                                                               
PERDEL2  MVC   CONHEAD(8),=C'ESTIMATE'                                          
         MVC   CONHEAD+9(1),ESTTYPE                                             
         LA    R4,CONHEAD+10                                                    
         ZIC   R0,ESTVERS                                                       
         EDIT  (R0),(3,(R4)),ALIGN=LEFT                                         
         AR    R4,R0                                                            
         MVC   1(7,R4),=C'DELETED'                                              
*                                                                               
         L     R6,AIO              SEE IF THERE WAS A SESSION EST HERE          
         LA    R4,KEY                                                           
         USING SESRECD,R4                                                       
         XC    SESKEY,SESKEY                                                    
         MVC   SESKEY(SESKMED-SESKEY),0(R6) CPJ, VERSION                        
         MVI   SESKSUB,SESKSUBQ   SESSION EST SUB TYPE                          
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
*                                                                               
PERDEL10 LA    R4,KEY                                                           
         CLC   SESKEY(SESKMED-SESKEY),KEYSAVE                                   
         BNE   PERDELX             IS THIS A SESSION REC FOR THIS EST           
*                                                                               
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         OI    ACSTATUS,X'80'      TURN ON DELETE BIT                           
         GOTO1 WRITE               WRITE SESSION EST OUT                        
*                                                                               
         GOTO1 SEQ                                                              
         B     PERDEL10                                                         
*                                                                               
PERDELX  XMOD1 1                                                                
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
**************************************************************                  
* SUB-ROUTINE TO PERFORM PREPARE COMMAND                     *                  
**************************************************************                  
         SPACE 1                                                                
PERPREP  NMOD1 0,*PERPRE*,RR=R5                                                 
         L     RC,0(R1)                                                         
         ST    R5,PPRELO                                                        
         GOTO1 AGETEST,DMCB,(RC),ESTTYPE                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PERPREP2 MVI   ELCODE,ACEPELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING ACEPD,R6                                                         
         MVI   ACEPEL,ACEPELQ                                                   
         MVI   ACEPLEN,ACEPLENQ                                                 
         MVC   ACEPINP,TODAYP      DATE INPUT                                   
         MVC   ACEPREP,PREPBY      PREPARER                                     
         MVC   ACEPDATE,PREPDATE                                                
         OC    ACEPDATE,ACEPDATE   TEST PREPARED DATE SPECIFIED                 
         BNZ   *+10                                                             
         MVC   ACEPDATE,TODAYP                                                  
*                                                                               
         GOTO1 ADDELEM                                                          
         GOTO1 WRITE                                                            
*                                                                               
PERPREP4 MVC   CONHEAD(8),=C'ESTIMATE'                                          
         MVC   CONHEAD+9(1),ESTTYPE                                             
         LA    R4,CONHEAD+10                                                    
         ZIC   R0,ESTVERS                                                       
         EDIT  (R0),(3,(R4)),ALIGN=LEFT                                         
         AR    R4,R0                                                            
         MVC   1(8,R4),=C'PREPARED'                                             
         LA    R4,10(R4)                                                        
         MVC   0(2,R4),=C'BY'     ECHO BACK THE INPUT DATA                      
         LA    R4,3(R4)                                                         
         MVC   0(L'ACEPREP,R4),ACEPREP                                          
         LA    R4,L'ACEPREP+1(R4)                                               
         MVC   0(2,R4),=C'ON'                                                   
         LA    R4,3(R4)                                                         
         GOTO1 DATCON,DMCB,(1,ACEPDATE),(17,(R4))                               
         OC    CONHEAD,SPACES                                                   
         GOTO1 SQUASHER,DMCB,CONHEAD,L'CONHEAD                                  
*                                                                               
* CHANGE COLUMN LIST AND RE-DISPLAY IF PREPARED ESTIMATE IS                     
* NOT ON THE SCREEN                                                             
*                                                                               
PERPREP6 SR    R0,R0                                                            
         ICM   R0,1,NCOLS          GET N'COLUMNS ON SCREEN                      
         BZ    PERPREP8            NONE-SO ADD TO LIST                          
*                                                                               
         LA    R5,COLINDS                                                       
         USING COLD,R5                                                          
PERPREP7 TM    COLFLAG,COLFEST     TEST IF COLUMN IS ESTIMATE                   
         BZ    *+14                NO                                           
         CLC   COLEST,ESTTYPE      TEST IF ESTIMATE IS ON SCREEN                
         BE    PERPREPX            YES                                          
         LA    R5,COLLENQ(R5)      NEXT ENTRY                                   
         BCT   R0,PERPREP7                                                      
*                                                                               
PERPREP8 GOTO1 =A(NEWCOL),DMCB,(RC),ESTTYPE,RR=PPRELO                           
*                                                                               
PERPREPX XMOD1                                                                  
         DROP  R5,R6                                                            
         SPACE 2                                                                
PPRELO   DS    A                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
****************************************************************                
* SUB-ROUTINE TO PERFORM THE SET COMMAND                       *                
****************************************************************                
         SPACE 1                                                                
PERSET   NMOD1 0,*PERSET*                                                       
         L     RC,0(R1)                                                         
         GOTO1 AGETEST,DMCB,(RC),ESTTYPE                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING ACEDD,R6                                                         
         MVI   ACEDEL,ACEDELQ      BUILD ESTIMATE DATA ELEMENT                  
         MVI   ACEDLEN,ACEDLNQ1                                                 
         MVI   ACEDTYPE,1                                                       
         MVC   ACEDWORK,SETWC                                                   
         ZAP   ACEDCOMM,SETAMT                                                  
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),(ACEDEL,AIO),(3,ACEDTYPE)               
         GOTO1 ADDELEM                                                          
*                                                                               
PERSET2  MVI   ELCODE,EUPELQ       CHANGE ESTIMATE UPDATE ELEM                  
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING EUPELD,R6                                                        
         MVC   EUPERS,TWAALIAS                                                  
         MVC   EUPLAST,TODAYP                                                   
         GOTO1 GETFACT,DMCB,(1,0)  GET CURRENT TIME                             
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   EUPTIME,FATIME+1    TIME IN BINARY SECONDS                       
         GOTO1 ADDELEM                                                          
*                                                                               
PERSET4  GOTO1 WRITE                                                            
*                                                                               
PERSET6  MVC   CONHEAD(8),=C'WORKCODE'                                          
         MVC   CONHEAD+9(2),SETWC                                               
         MVC   CONHEAD+12(7),=C'CHANGED'                                        
         MVC   CONHEAD+20(11),=C'ON ESTIMATE'                                   
         MVC   CONHEAD+32(1),ESTTYPE                                            
         LA    R4,CONHEAD+33                                                    
         ZIC   R0,ESTVERS                                                       
         EDIT  (R0),(3,(R4)),ALIGN=LEFT                                         
*                                                                               
PERSETX  GOTO1 =A(REDIS),DMCB,(RC),RR=RELO                                      
         XMOD1                                                                  
         DROP  R1,R6                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
****************************************************************                
* SUB-ROUTINE TO PERFORM THE INQUIRY COMMAND                   *                
****************************************************************                
         SPACE 1                                                                
PERINQ   NMOD1 0,*PERINQ*                                                       
         L     RC,0(R1)                                                         
         OC    INQTYPE,INQTYPE     ANY INQUIRY TYPE                             
         BZ    PERINQ8             NO, GET LOOK AT GOBLOCK                      
         CLI   INQTYPE,C'S'        TEST FOR SUPPLEMENT                          
         BE    PERINQ10                                                         
*                                                                               
PERINQ1  GOTO1 ABLDLST,DMCB,(RC),(INQTYPE,AIO2)                                 
         XC    LISTAR,LISTAR       CLEAR SPACE TO BUILD COLUMN                  
         ICM   R3,15,0(R1)                                                      
         BZ    PERINQR                                                          
         BCTR  R3,0                                                             
         SLL   R3,1                                                             
         L     R4,AIO2                                                          
         LA    R4,0(R4,R3)         GET LAST ENTRY IN LIST                       
*                                                                               
         CLI   INQTYPE,C'R'        TEST FOR 'R' INQUIRY                         
         BE    PERINQ2             YES-SHOW R1 FOLLOWED BY HIGHEST R'S          
         CLI   INQTYPE,0           TEST FOR NO ESTIMATE TYPE                    
         BNE   *+12                NO-USER INPUT 'P'                            
         CLI   NREVS,0             TEST FOR ANY REVISIONS                       
         BNE   PERINQ2             YES-SHOW R1 FOLLOWED BY HIGHEST R'S          
*                                                                               
         L     R3,0(R1)            GET # OF ENTRIES AGAIN                       
         LA    RE,MAXCOLS                                                       
         CR    R3,RE               CAN'T BE MORE THAN MAXIMUM COLS              
         BNH   *+6                                                              
         LR    R3,RE                                                            
         LA    R5,LISTAR-2                                                      
*                                                                               
         LR    R6,R3               NUMBER OF ESTIMATES IN DISPLAY               
         BCTR  R6,0                                                             
         SLL   R6,1                BACK UP TO FIRST EST IN DISPLAY              
         SR    R4,R6                                                            
         B     PERINQ4                                                          
*                                                                               
PERINQ2  MVC   LISTAR(2),=C'R1'    START LIST WITH R1                           
         CLI   NREVS,1             TEST ONLY ONE REVISION                       
         BE    PERINQ6             YES-ALL DONE                                 
*                                                                               
         MVI   LISTAR+2,C','       PLACE COMMA AFTER R1                         
         LA    R5,LISTAR+1         INITIALIZE OUTPUT POINTER                    
         ZIC   R3,NREVS            SET LOOP COUNTER                             
         BCTR  R3,0                SUBTRACT FOR R1                              
         LA    RE,MAXCOLS-1                                                     
         CR    R3,RE               TEST TOO MANY COLUMNS                        
         BNH   *+6                                                              
         LR    R3,RE                                                            
*                                                                               
         LR    R6,R3               NUMBER OF REVISIONS BESIDES R1               
         BCTR  R6,0                                                             
         SLL   R6,1                BACK UP TO LOWEST REVISION IN DISPLY         
         SR    R4,R6                                                            
*                                                                               
PERINQ4  LA    R5,2(R5)                                                         
         MVC   0(1,R5),0(R4)                                                    
         EDIT  (B1,1(R4)),(3,1(R5)),ALIGN=LEFT                                  
         AR    R5,R0                                                            
         MVI   1(R5),C','                                                       
         LA    R4,2(R4)            NEXT ENTRY                                   
         BCT   R3,PERINQ4                                                       
         MVI   1(R5),C' '          CLEAR COMMA ON LAST                          
*                                                                               
PERINQ6  LA    R2,PROCOLH                                                       
         MVC   8(L'PROCOL,R2),LISTAR                                            
         LA    R1,L'PROCOL                                                      
         LA    RE,PROCOL+L'PROCOL-1                                             
         B     PERINQ20                                                         
*                                                                               
PERINQ8  OC    GOINQ,GOINQ         DO WE HAVE A DEFUALT ?                       
         BZ    PERINQ1             NO, GET LAST 4                               
         LA    R2,PROCOLH          YES, USE THEM                                
         XC    PROCOL,PROCOL       CLEAR COLUMN FIELD                           
         MVC   8(L'GOINQ,R2),GOINQ                                              
         LA    R1,L'GOINQ                                                       
         LA    RE,GOINQ+L'GOINQ-1                                               
         B     PERINQ20                                                         
*                                                                               
* PERFORM THE SUPPLEMENT INQUIRY                                                
*                                                                               
PERINQ10 XC    LISTAR,LISTAR                                                    
         LA    R5,LISTAR           R5=OUTPUT POINTER                            
         ZIC   R3,NREVS            SET COUNTER=N'REVISIONS                      
         CLI   INQSUPP,0           TEST FOR SPECIFIC SUPPLEMENT                 
         BE    PERINQ12            NO                                           
*                                                                               
         L     R4,AIO2                                                          
         ZIC   R1,INQSUPP                                                       
         BCTR  R1,0                                                             
         SLL   R1,1                                                             
         LA    R4,0(R1,R4)         INDEX INTO REVISION LIST                     
*                                                                               
         MVI   0(R5),C'R'                                                       
         LA    R5,1(R5)            SHOW LOWER REVISION FIRST                    
         EDIT  (B1,1(R4)),(3,0(R5)),ALIGN=LEFT                                  
         AR    R5,R0                                                            
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
*                                                                               
         LA    R4,2(R4)            ADVANCE TO HIGHER REVISION NOW               
         MVI   0(R5),C'R'                                                       
         LA    R5,1(R5)                                                         
         EDIT  (B1,1(R4)),(3,0(R5)),ALIGN=LEFT                                  
         AR    R5,R0                                                            
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
*                                                                               
         MVI   0(R5),C'S'          NOW THE SUPPLEMENT                           
         EDIT  (B1,INQSUPP),(3,1(R5)),ALIGN=LEFT                                
         B     PERINQ16                                                         
*                                                                               
PERINQ12 MVC   LISTAR(2),=C'R1'    START WITH R1                                
         LA    R5,2(R5)                                                         
         BCTR  R3,0                DECREMENT N'REVISIONS                        
         LTR   R6,R3               MOVE LOOP COUNTER TO R6                      
         BZ    PERINQ16            ONLY ONE REVISION                            
         LA    RE,MAXCOLS-1                                                     
         CR    R6,RE               TEST FOR MAXIMUM N'COLUMNS                   
         BNH   *+6                                                              
         LR    R6,RE                                                            
         LA    R3,1(R3)            COMPUTE LOWEST SUPPLEMENT IN DISPLAY         
         SR    R3,R6                                                            
*                                                                               
PERINQ14 MVI   0(R5),C','          ADD A COMMA AFTER LAST COLUMN                
         LA    R5,1(R5)            ADVANCE OUTPUT POINTER                       
         MVI   0(R5),C'S'                                                       
         LA    R5,1(R5)                                                         
         EDIT  (R3),(3,0(R5)),ALIGN=LEFT                                        
         AR    R5,R0                                                            
         LA    R3,1(R3)            INCREMENT SUPPLEMENT NUMBER                  
         BCT   R6,PERINQ14                                                      
*                                                                               
PERINQ16 LA    R2,PROCOLH                                                       
         MVC   8(L'PROCOL,R2),LISTAR                                            
         LA    R1,L'PROCOL                                                      
         LA    RE,PROCOL+L'PROCOL-1                                             
*                                                                               
PERINQ20 CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
         STC   R1,5(R2)                                                         
         NI    4(R2),X'FF'-X'20'   FIELD HAS CHANGED                            
         OI    6(R2),X'80'                                                      
*                                                                               
* CAUSE ANOTHER JOB ESTIMATE TRANSACTION BY CALLING GENCON AGAIN                
* TO TRIGGER A DISPLAY OF THE NEW COLUMNS                                       
*                                                                               
         GOTO1 GENCON,DMCB,(R8)    COME BACK TO EDIT                            
*                                                                               
PERINQX  XMOD1 1                                                                
*                                                                               
PERINQR  LA    R2,PROCOMH                                                       
         MVI   ERROR,NOPSORRS                                                   
         GOTO1 VERRCUR                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
****************************************************************                
* SUB-ROUTINE TO PERFORM THE SUP COMMAND                       *                
*                                                              *                
*   ON ENTRY, P1=A(GEND)                                       *                
*   ROUTINE EXITS DIRECTLY TO USER                             *                
****************************************************************                
         SPACE 1                                                                
PERSUP   NMOD1 (PSWORKX-PSWORKD),**PERSUP,RR=R2,CLEAR=YES                       
         LR    R7,RC               R7=A(LOCAL STORAGE)                          
         USING PSWORKD,R7                                                       
         L     RC,0(R1)            RESTORE GENCON RC                            
         ST    R2,PSRELO                                                        
         MVC   PSSYSRD,SYSRD       SAVE SYSRD AT ENTRANCE                       
         MVC   PSNUM,HIRVERS       SAVE HIGHEST REVISION NUMBER                 
         MVC   PSNREVS,NREVS       SAVE N'REVISIONS                             
*                                                                               
PERSUP2  MVC   LISTAR,SPACES       BUILD COPY COMMAND LINE                      
         MVC   LISTAR(4),=C'COPY'                                               
         LA    R5,LISTAR+5                                                      
         MVI   0(R5),C'R'                                                       
         LA    R5,1(R5)                                                         
         ZIC   R0,PSNUM            EDIT OUT HIGHEST REVISION NUM                
         CURED (R0),(3,0(R5)),0,ALIGN=LEFT                                      
         AR    R5,R0                                                            
         LA    R5,1(R5)                                                         
         MVI   0(R5),C'R'                                                       
         LA    R5,1(R5)                                                         
         LA    RE,LISTAR           COMPUTE INPUT LENGTH                         
         SR    R5,RE                                                            
         STC   R5,PROCOMH+5                                                     
         NI    PROCOMH+4,X'FF'-X'20'  TURN OFF PREV VALID                       
         MVC   PROCOM,LISTAR       MOVE COMMAND TO FIELD                        
*                                                                               
PERSUP4  MVI   PFKEY,0             CLEAR PFKEY VALUE                            
         MVI   CALLER,X'42'        CALLER IS COMMAND MODULE                     
         L     RF,=V(DUMMY)                                                     
         A     RF,PSRELO                                                        
         ST    RF,SYSDUMMY         LOAD IN PHASES BEHIND PRO42                  
         BAS   RE,PSSETRD          FORCE RETURN TO THIS ROUTINE                 
         GOTO1 GENCON,DMCB,(R8)                                                 
*                                                                               
         MVC   PSMSG,CONHEAD       SAVE COPY MESSAGE                            
*                                                                               
PERSUP6  MVC   LISTAR,SPACES                                                    
         LA    R5,LISTAR                                                        
         MVI   0(R5),C'I'          BUILD INQUIRY COMMAND LINE                   
         LA    R5,2(R5)                                                         
         MVI   0(R5),C'S'                                                       
         LA    R5,1(R5)                                                         
         ZIC   R0,PSNREVS          GET N'REVISIONS                              
         CURED (R0),(3,0(R5)),0,ALIGN=LEFT                                      
         AR    R5,R0                                                            
         LA    RE,LISTAR                                                        
         SR    R5,RE                                                            
         STC   R5,PROCOMH+5                                                     
         NI    PROCOMH+4,X'FF'-X'20'                                            
         MVC   PROCOM,LISTAR                                                    
*                                                                               
         GOTO1 GENCON,DMCB,(R8)                                                 
*                                                                               
         MVC   SYSRD,PSSYSRD       RESTORE SYSRD                                
         MVC   CONHEAD,PSMSG       RESTORE HEADER MESSAGE                       
         GOTO1 ERREX2              GO RIGHT OUT TO USER                         
*                                                                               
PERSUPX  XMOD1 1                                                                
*                                                                               
PSSETRD  NTR1  ,                                                                
         ST    RD,SYSRD                                                         
         XIT1  1                                                                
         DROP  R7                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
****************************************************************                
* SUB-ROUTINE TO FORM A NEW COLUMN FIELD AFTER AN ADD OR       *                
* COPY COMMAND AND TO RE-DISPLAY THE NEW COLUMN DATA           *                
* ROUTINE CAN ALSO BE CALLED FROM APPROVE OR PREPARE TO SHOW   *                
* THE TARGET ESTIMATE ON THE SCREEN                            *                
*                                                              *                
*   ON ENTRY, P1=A(GEND), P2=A(ESTIMATE TYPE/VERSION)          *                
*   ROUTINE EXITS DIRECTLY TO USER                             *                
****************************************************************                
         SPACE 1                                                                
NEWCOL   NMOD1 (NCWORKX-NCWORKD),**NEWCOL,RR=R2,CLEAR=YES                       
         LR    R7,RC               R7=A(LOCAL STORAGE)                          
         USING NCWORKD,R7                                                       
         L     RC,0(R1)            RESTORE GENCON RC                            
         ST    R2,NCRELO                                                        
         L     RE,4(R1)            GET A(ESTIMATE TYPE/VERSION)                 
         MVC   NCEST,0(RE)         SAVE ESTIMATE TYPE/VERSION                   
         MVC   NCMSG,CONHEAD       SAVE COMMAND MESSAGE                         
         MVC   NCSYSRD,SYSRD       SAVE SYSRD AT ENTRANCE                       
*                                                                               
         CLI   PROCOLH+5,0         TEST FOR BLANK COLUMN FIELD                  
         BE    NEWCOL2             YES                                          
         MVC   NCNCOLS,NCOLS       SAVE N'COLUMNS                               
*                                                                               
NEWCOL2  XC    LISTAR,LISTAR       CLEAR NEW COLUMN DATA FIELD                  
         MVC   LISTAR(1),NCESTYPE  ESTIMATE TYPE                                
         LA    R5,LISTAR+1                                                      
         EDIT  (B1,NCESTVER),(3,0(R5)),ALIGN=LEFT                               
         AR    R5,R0                                                            
         CLI   NCNCOLS,0           TEST FOR ANY COLUMNS                         
         BE    NEWCOL15            NONE                                         
*                                                                               
NEWCOL4  MVI   0(R5),C','          ATTACH THE INPUT IN COLUMN FIELD             
         LA    R5,1(R5)                                                         
         ZIC   R1,PROCOLH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),PROCOL      COPY OVER REST OF FIELD                      
         LA    R5,1(R1,R5)         RE=A(END OF NEW STRING)                      
*                                                                               
NEWCOL6  ZIC   R1,NCNCOLS                                                       
         LA    R1,1(R1)            INCREMENT COLUMN COUNT                       
         STC   R1,NCNCOLS          SET N'NEW COLUMNS                            
         CH    R1,=Y(MAXCOLS)      TEST FOR TOO MANY COLUMNS                    
         BNH   NEWCOL9             NO                                           
         LA    R1,MAXCOLS          SET TO MAXIMUM                               
*                                                                               
NEWCOL7  LA    R5,LISTAR-1         YES-FIND END OF NEW LIST                     
         STC   R1,NCNCOLS          RESET N'NEW COLUMNS                          
*                                                                               
NEWCOL8  LA    R5,1(R5)            BUMP TO NEXT CHARACTER                       
         CLI   0(R5),C','          TEST FOR A COMMA                             
         BNE   *-8                                                              
         BCT   R1,NEWCOL8                                                       
*                                                                               
NEWCOL9  LA    RF,LISTAR           COMPUTE LENGTH OF STRING                     
         LR    RE,R5               RE=A(END OF STRING)                          
         SR    RE,RF               FIND LENGTH                                  
         CH    RE,=Y(L'PROCOL)     TEST FOR FIT IN COLUMNS                      
         BNH   NEWCOL10                                                         
*                                                                               
         ZIC   R1,NCNCOLS          CUT OFF ONE MORE COLUMN                      
         BCTR  R1,0                                                             
         B     NEWCOL7                                                          
*                                                                               
NEWCOL10 LA    R1,LISTAR+L'LISTAR-1                                             
         SR    R1,R5               COMPUTE EXEC LEN OF REST OF FLD              
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R5),0(R5)       CLEAR REST OF FIELD                          
*                                                                               
NEWCOL15 MVC   PROCOM,SPACES       CLEAR AND XMIT COMMAND                       
         MVI   PROCOMH+5,0                                                      
         NI    PROCOMH+4,X'FF'-X'20'                                            
         OI    PROCOMH+6,X'80'                                                  
*                                                                               
         MVC   PROCOL,LISTAR                                                    
         LA    RE,PROCOL+L'PROCOL-1 RESET FIELD LENGTH                          
         LA    R1,L'PROCOL                                                      
         CLI   0(RE),C' '          TEST FOR LAST SIGNIFICANT CHAR               
         BH    *+10                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
         STC   R1,PROCOLH+5                                                     
         NI    PROCOLH+4,X'FF'-X'20'  TURN OFF PREV VALID BIT                   
         OI    PROCOLH+6,X'80'     XMIT BACK NEW COLUMNS                        
*                                                                               
NEWCOL20 MVI   PFKEY,0             CLEAR PFKEY VALUE                            
         L     RF,=V(DUMMY)                                                     
         A     RF,NCRELO                                                        
         ST    RF,SYSDUMMY         LOAD IN PHASES BEHIND PRO42                  
         BAS   RE,NCSETRD          FORCE RETURN TO THIS ROUTINE                 
         GOTO1 GENCON,DMCB,(R8)                                                 
*                                                                               
* RE-DISPLAY SHOULD NOT CREATE AN ERROR-CURSOR SETTING IS PROTECTION            
*                                                                               
         CLI   ERROR,0             TEST NORMAL EXIT                             
         BE    *+8                                                              
         LA    R2,PROCLIH          SET CURSOR POSITION IN CASE                  
*                                                                               
         MVC   SYSRD,NCSYSRD       RESTORE SYSRD                                
         MVC   CONHEAD,NCMSG       RESTORE HEADER MESSAGE                       
         GOTO1 ERREX2              GO RIGHT OUT TO USER                         
*                                                                               
NEWCOLX  XMOD1 1                                                                
*                                                                               
NCSETRD  NTR1  ,                                                                
         ST    RD,SYSRD                                                         
         XIT1  1                                                                
         DROP  R7                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
****************************************************************                
* SUB-ROUTINE TO RE-DISPLAY THE EXISTING COLUMN LIST ON THE    *                
* SCREEN.                                                      *                
*                                                              *                
*   ON ENTRY, P1=A(GEND)                                       *                
*   ROUTINE EXITS DIRECTLY TO USER                             *                
****************************************************************                
         SPACE 1                                                                
REDIS    NMOD1 (RDWORKX-RDWORKD),**REDISP,RR=R2,CLEAR=YES                       
         LR    R7,RC               R7=A(LOCAL STORAGE)                          
         USING RDWORKD,R7                                                       
         L     RC,0(R1)            RESTORE GENCON RC                            
         ST    R2,RDRELO                                                        
         MVC   RDMSG,CONHEAD       SAVE COMMAND MESSAGE                         
         MVC   RDSYSRD,SYSRD       SAVE SYSRD AT ENTRANCE                       
*                                                                               
REDIS2   MVC   PROCOM,SPACES       CLEAR AND XMIT COMMAND                       
         MVI   PROCOMH+5,0                                                      
         NI    PROCOMH+4,X'FF'-X'20'                                            
         OI    PROCOMH+6,X'80'                                                  
*                                                                               
         NI    PROCOLH+4,X'FF'-X'20'  TURN OFF PREV VALID BIT                   
*                                                                               
REDIS4   MVI   PFKEY,0             CLEAR PFKEY VALUE                            
         L     RF,=V(DUMMY)                                                     
         A     RF,RDRELO                                                        
         ST    RF,SYSDUMMY         LOAD IN PHASES BEHIND PRO42                  
         BAS   RE,RDSETRD          FORCE RETURN TO THIS ROUTINE                 
         GOTO1 GENCON,DMCB,(R8)                                                 
*                                                                               
* RE-DISPLAY SHOULD NOT CREATE AN ERROR-CURSOR SETTING IS PROTECTION            
*                                                                               
         CLI   ERROR,0             TEST NORMAL EXIT                             
         BE    *+8                                                              
         LA    R2,PROCLIH          SET CURSOR POSITION IN CASE                  
*                                                                               
         MVC   SYSRD,RDSYSRD       RESTORE SYSRD                                
         MVC   CONHEAD,RDMSG       RESTORE HEADER MESSAGE                       
         GOTO1 ERREX2              GO RIGHT OUT TO USER                         
*                                                                               
RDDISX   XMOD1 1                                                                
*                                                                               
RDSETRD  NTR1  ,                                                                
         ST    RD,SYSRD                                                         
         XIT1  1                                                                
         DROP  R7                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
******************************************************************              
* SUB-ROUTINE TO COPY A SESSION ESTIMATE ATTATCHED TO THE EST    *              
* WHICH WAS COPIED OR DID THE REPLACING                          *              
*        NEEDS FROM ESTIMATE IN AOI2 AND TO ESTIMATE IN AIO1     *              
******************************************************************              
CPYSES   NMOD1 (CPWORKX-CPWORKD),**CPYSES,RR=R2                                 
         LR    R7,RC               R7=A(LOCAL STORAGE)                          
         USING CPWORKD,R7                                                       
         L     RC,0(R1)                                                         
         ST    R2,CPRELO                                                        
         MVC   AIO,AIO2            WAS THERE A SESSION EST ON FROM EST          
         L     R6,AIO                                                           
         LA    R4,KEY                                                           
         USING SESRECD,R4                                                       
         MVC   SESKEY,0(R6)                                                     
         MVI   SESKSUB,SESKSUBQ                                                 
         GOTO1 HIGH                                                             
         CLC   SESKEY(SESKMED-SESKEY),KEYSAVE                                   
         BNE   CPYSX                            NO SESSION HERE                 
*                                                                               
         MVC   CPFROMKY,SESKEY     SAVE "FROM" SESSION KEY                      
         MVC   TALMEDIA,SESKMED    SAVE SESSION MEDIA                           
*                                                                               
         MVC   AIO,AIO1            BUILD "TO" SES KEY FROM "TO" EST KEY         
         L     R6,AIO                                                           
         LA    R4,KEY                                                           
         USING SESRECD,R4                                                       
         MVC   SESKEY,0(R6)        PRIME KEY W/"TO" ESTIMATE KEY                
         MVI   SESKSUB,SESKSUBQ    THEN FILL IN SESSION KEY DATA                
         MVC   SESKMED,TALMEDIA                                                 
         XC    SESKSEQ,SESKSEQ                                                  
*                                                                               
CPYS30   MVC   CPTOKY,KEY          SAVE CURRENT "TO" KEY                        
         MVC   AIO,AIO1            BUILD "TO" RECORDS IN AIO1                   
         CLI   CPYSW,C'C'          IS THIS A NEW ESTIMATE                       
*****    BE    CPYS50   NOP'D      YES, THERE CAN'T BE A SESSION RECORD         
*                                  ON "TO" ESTIMATE                             
         BE    CPYS45              BUT CHECK ANYWAY                             
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   SESKEY,KEYSAVE                                                   
         BE    CPYS40              YES, WRITE                                   
*                                                                               
         CLC   SESKEY(SESKMED-SESKEY),KEYSAVE  SAME EST, DIFF MEDIA             
         BNE   CPYS35                          NO, OK TO ADD                    
         MVI   ERROR,BADTMED                                                    
         LA    R2,PROCOMH                                                       
         GOTO1 VERRCUR                                                          
*                                                                               
CPYS35   MVC   KEY,KEYSAVE         RESTORE KEY TO WHAT YOU LOOKED FOR           
         B     CPYS50              AND ADD                                      
*                                                                               
CPYS40   MVI   RDUPDATE,C'Y'                                                    
         GOTO1 READ                READ EXISTING SESSION RECORD                 
         BAS   RE,CPYSBLD          BUILD NEW SESSION                            
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         MVI   ACSTATUS,0            TURN OFF DELETE                            
         GOTO1 WRITE                                                            
         B     CPYS70                                                           
*                                                                               
CPYS45   OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         USING SESRECD,R4                                                       
         CLC   SESKEY,KEYSAVE      TEST ALREADY ON FILE                         
         BNE   CPYS48              NO, ADD IT                                   
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 READ                READ EXISTING SESSION RECORD                 
         BAS   RE,CPYSBLD          BUILD NEW SESSION                            
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         MVI   ACSTATUS,0            TURN OFF DELETE                            
         GOTO1 WRITE                                                            
         B     CPYS70                                                           
*                                                                               
CPYS48   MVC   KEY,KEYSAVE         RESTORE KEY BEFORE ADDING                    
*                                                                               
CPYS50   BAS   RE,CPYSBLD          BUILD RECORD TO ADD                          
         GOTO1 ADD                                                              
         GOTO1 READ                RESET NEW ACC SEQ                            
*                                                                               
CPYS70   MVC   KEY,CPFROMKY        GET KEY OF FROM SESSION RECORD               
         MVC   AIO,AIO2            KEEP "FROM"S IN AIO2                         
*                                                                               
         BAS   RE,CPNEXTKY         BUMP  "FROM" KEY                             
*                                                                               
         GOTO1 HIGH                                                             
         LA    R4,KEY                                                           
         USING SESRECD,R4                                                       
         CLC   SESKEY(SESKSEQ-SESKEY),KEYSAVE  ANY MORE "FROM"S                 
         BNE   CPYS100                         NO                               
*                                                                               
         MVC   CPFROMKY,SESKEY     SAVE "FROM" SESSION KEY                      
         MVC   KEY,CPTOKY          PREP KEY AS NEXT "TO"                        
         BAS   RE,CPNEXTKY         BUMP "TO" KEY                                
*                                                                               
         B     CPYS30              WRITE/ADD NEXT SESSION REC                   
*                                                                               
CPYS100  EQU   *                   DELETE ANY OLD TO'S NOT NEEDED               
*                                  INCASE REPLACE W/SMALLER RECORD              
         CLI   CPYSW,C'R'                                                       
         BNE   CPYSX                                                            
*                                                                               
         MVC   KEY,CPTOKY          PRIME KEY WITH LAST "TO" WRITTEN             
*                                                                               
CPYS110  BAS   RE,CPNEXTKY         BUMP KEY                                     
         LA    R4,KEY                                                           
         USING SESRECD,R4                                                       
*                                                                               
         GOTO1 HIGH                                                             
         CLC   SESKEY(SESKSEQ-SESKEY),KEYSAVE  ANY MORE "TO"S                   
         BNE   CPYSX               NO                                           
*                                                                               
         CLI   KEY+1,X'3C'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 READ                                                             
         LA    R4,AIO                                                           
         USING ACKEYD,R4                                                        
         OI    ACSTATUS,X'80'      DELETE                                       
         GOTO1 WRITE                                                            
         B     CPYS110                                                          
*                                                                               
CPYSX    B     CPYSESX                                                          
         DROP  R4                                                               
*                                                                               
CPNEXTKY EQU   *                   BUMP THE SEQ NUMBER OF KEY                   
         LA    R4,KEY                                                           
         USING SESRECD,R4                                                       
         ZIC   RF,SESKSEQ          BUMP SEQ NUMBER                              
         LA    RF,1(RF)                                                         
         XC    SESKSEQ(L'SESKEY-(SESKSEQ-SESKEY)),SESKSEQ                       
         STC   RF,SESKSEQ                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        BUILD A RECORD USING THE KEY IN KEY AND THE RECORD DATA OF             
*        AIO2,                                                                  
***********************************************************************         
CPYSBLD  NTR1                                                                   
         MVC   AIO,AIO1            BUILD RECORD IN AIO1                         
         L     R6,AIO              MOVE KEY INTO AIO                            
         LA    R4,KEY                                                           
         USING SESRECD,R4                                                       
         MVC   0(L'SESKEY,R6),SESKEY                                            
         USING ACKEYD,R6                                                        
*        LA    RF,=Y(ACRECORD-ACKEYD+1)                                         
*        A     RF,CPRELO           RELO                                         
*        MVC   ACLENGTH,0(RF)      INITIALIZE RECORD LENGTH                     
         MVC   ACLENGTH,=Y(ACRECORD-ACKEYD+1)                                   
         MVI   ACSTATUS,0              JUST IN CASE                             
         LA    R6,ACRECORD-ACKEYD(R6)                                           
         XC    0(10,R6),0(R6)      CLEAR RECORD START                           
*                                                                               
         L     R6,AIO2             ADDRESS OLD SESSION RECORD DATA              
         LA    R6,ACRECORD-ACKEYD(R6)                                           
*                                                                               
CPYSB50  XC    ELEM,ELEM           COPY ELEMENTS FROM OLD SESSION REC           
         CLI   0(R6),0             E-O-R                                        
         BE    CPYSBX                                                           
*                                                                               
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)       EXTRACT ELEMENT                              
         LA    R2,PROCLIH                                                       
         GOTO1 ADDELEM                                                          
         LA    R6,1(R1,R6)         NEXT ELEMENT                                 
         B     CPYSB50                                                          
*                                                                               
CPYSBX   GOTO1 PERSIN              ADD ACTIVITY ELEMENT                         
         B     CPYSESX                                                          
CPYSESX  XMOD1 1                                                                
         DROP  R4,R6                                                            
         EJECT                                                                  
CHKFUND  NMOD1 0,**CHKFNA                                                       
         L     RC,0(R1)                                                         
         MVC   BYTE,0(R1)                                                       
         CLI   ESTTYPE,ACEVREV    ONLY CONCERNED WITH REVISIONS                 
         BNE   CHKFOK                                                           
         MVC   SVEST,ESTTYPE                                                    
*                                                                               
         ZAP   SVNET,=P'0'                                                      
*                                                                               
         USING EDAELD,R6                                                        
         MVI   ELCODE,EDAELQ       GET THE DATA ELEMENTS                        
         BAS   RE,GETELFND                                                      
         B     *+8                                                              
*                                                                               
CHKF02   BAS   RE,NEXTEL1                                                       
         BNE   CHKF04                                                           
         AP    SVNET,EDACOMM                                                    
         B     CHKF02                                                           
         DROP  R6                                                               
*                                                                               
CHKF04   MVC   AIO,AIO3                                                         
         USING JFNELD,R6                                                        
         MVI   ELCODE,JFNELQ                                                    
         BAS   RE,GETELFND                                                      
         BNE   CHKFOK              NOT FUNDED, NOTHING ELSE TO DO               
*                                                                               
         CLI   BYTE,C'U'           ARE WE UNAPPROVING?                          
         BNE   CHKF06              NO                                           
*                                                                               
         CLC   SVEST,JFNEST        YES, IS ESTIMATE FUNDED?                     
         BNE   CHKFOK              NOPE, NOTHING ELSE TO DO                     
*                                                                               
CHKF06   ZAP   SVJAMT,JFNAMT       SAVE THE AMOUNT CURRENTLY FUNDED             
*                                                                               
         MVC   AIO,AIO2            USE IO2 FOR AUTHORIZATION RECORD             
         LA    R4,KEY                                                           
         USING AUTKEY,R4                                                        
         MVC   AUTKEY,SPACES                                                    
         MVI   AUTKTYP,AUTKTYPQ                                                 
         MVI   AUTKSUB,AUTKSUBQ                                                 
         MVC   AUTKCPY(3),CUL                                                   
         MVC   AUTKOGR(L'JFNKEY),JFNKEY                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   AUTKEY,KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                BIG TROUBLE IF NOT FOUND                     
         DROP  R4,R6                                                            
*                                                                               
         USING AUTHELD,R6                                                       
         MVI   ELCODE,AUTHELQ      GET DATA ELEMENT                             
         BAS   RE,GETELFND                                                      
         BE    *+6                                                              
         DC    H'0'                MUST HAVE THIS ONE                           
         ST    R6,SAVER6                                                        
*                                                                               
CHKF08   BAS   RE,NEXTEL1          ANY MORE?                                    
         BNE   CHKF10              NO                                           
         ST    R6,SAVER6           YES, FIND THE LAST ONE                       
         B     CHKF08                                                           
*                                                                               
CHKF10   L     R6,SAVER6           GET ADDRESS OF LAST AUTHELQ                  
         ZAP   SVAAMT,AUTHAMT      SAVE THE AMOUNT                              
*                                                                               
         LA    R4,KEY              READ THE FUND RECORD NOW                     
         USING FUNKEY,R4                                                        
         MVC   FUNKEY,SPACES                                                    
         MVI   FUNKTYP,FUNKTYPQ                                                 
         MVI   FUNKSUB,FUNKSUBQ                                                 
         MVC   FUNKCPY(3),CUL      SAME KEY AS AUTRECD                          
         MVC   FUNKOGR(L'JFNKEY),KEYSAVE+5                                      
*                                                                               
         MVI   RDUPDATE,C'Y'       MAY NEED TO UPDATE THIS RECORD               
         GOTO1 HIGH                                                             
         CLC   FUNKEY,KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                BIG TROUBLE IF NOT FOUND                     
         DROP  R4,R6                                                            
*                                                                               
         USING FUNELD,R6                                                        
         MVI   ELCODE,FUNELQ       GET DATA ELEMENT                             
         BAS   RE,GETELFND                                                      
         BE    *+6                                                              
         DC    H'0'                BIG TROUBLE IF NOT FOUND                     
         ZAP   SVFAMT,FUNAMT       SAVE THE FUNDED AMOUNT                       
*                                                                               
         CLI   BYTE,C'U'           ARE WE UNAPPROVING?                          
         BNE   CHKF14              NO                                           
         SP    FUNAMT,SVJAMT       YES, DECREASE AMOUNT FUNDED                  
         GOTO1 WRITE               REWRITE THE FUND RECORD                      
*                                                                               
         USING FJNELD,R6                                                        
CHKF11   MVI   ELCODE,FJNELQ                                                    
         BAS   RE,GETELFND         FIND THE FUND JOB ELEMENT                    
         B     *+8                                                              
*                                                                               
CHKF12   BAS   RE,NEXTEL1                                                       
         BE    CHKF13                                                           
         GOTO1 SEQ                 IF FUNDED, IT MUST BE HERE                   
         LA    R4,KEY                                                           
         USING FUNKEY,R4                                                        
         CLC   FUNKEY(L'FUNKEY-1),KEYSAVE                                       
         BE    CHKF11                                                           
         DC    H'0'                                                             
*                                                                               
CHKF13   L     R4,AIO3                                                          
         CLC   FJNJOB,3(R4)        IS THIS THE JOB?                             
         BNE   CHKF12              NO, KEEP LOOKING                             
*                                                                               
         MVI   0(R6),X'FF'         YES, MARK IT FOR DELETION                    
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         L     R4,AIO                                                           
         CLI   FUNKSEQ,X'40'                                                    
         BE    CHKF13A             ORIGINAL FUND RECORD, OK                     
         MVI   ELCODE,FJNELQ       OTHERWISE, CHECK FOR JOBS                    
         BAS   RE,GETELFND                                                      
         BE    CHKF13A                                                          
         OI    FUNRSTA,X'80'       NONE, DELETE IT                              
*                                                                               
CHKF13A  GOTO1 WRITE               REWRITE FUND RECORD                          
         DROP  R6                                                               
*                                                                               
         MVC   AIO,AIO3            GET JOB IO AREA                              
         MVI   ELCODE,JOBELQ       GET JOB ELEMENT                              
         BAS   RE,GETELFND                                                      
         BE    *+6                 WE ALWAYS HAVE THIS ELEMENT                  
         DC    H'0'                                                             
*                                                                               
         USING JOBELD,R6                                                        
         NI    JOBSTA2,X'FF'-JOBSFUN    INDICATE NOT FUNDED                     
*                                                                               
         MVI   ELCODE,JFNELQ                                                    
         GOTO1 REMELEM             REMOVE JOB FUNDING ELEMENT                   
         GOTO1 WRITE               WRITE BACK THE JOB RECORD                    
         B     CHKFOK                                                           
*                                                                               
         USING FUNELD,R6                                                        
CHKF14   ZAP   DUB,SVAAMT          APPROVALS PROCESSED HERE                     
         SP    DUB,SVFAMT          FIND AVAILABLE AMOUNT                        
         AP    DUB,SVJAMT          ADD BACK CURRENTLY FUNDED AMOUNT             
*                                                                               
         CP    DUB,SVNET           IS THERE ENOUGH MONEY LEFT?                  
         BL    CHKFNG              NO                                           
*                                                                               
         SP    FUNAMT,SVJAMT       YES, REMOVE OLD AMOUNT                       
         AP    FUNAMT,SVNET        AND ADD IN NEW AMOUNT                        
         GOTO1 WRITE               REWRITE FUNDED RECORD                        
         DROP  R6                                                               
*                                                                               
         USING JFNELD,R6                                                        
         MVC   AIO,AIO3            GET JOB IO AREA                              
         MVI   ELCODE,JFNELQ       GET JOB FUND ELEMENT                         
         BAS   RE,GETELFND                                                      
         BE    *+6                 WE HAVE TO HAVE THIS ELEMENT                 
         DC    H'0'                                                             
*                                                                               
         ZAP   JFNAMT,SVNET        CHANGE THE FUNDED AMOUNT                     
         MVC   JFNEST,SVEST          AND THE ESTIMATE NUMBER                    
         GOTO1 WRITE               WRITE BACK THE JOB RECORD                    
*                                                                               
CHKFOK   CR    RB,RB               CONDITION CODE EQUAL                         
         B     CHKFX                                                            
*                                                                               
CHKFNG   LTR   RB,RB               CONDITION CODE NOT EQUAL                     
*                                                                               
CHKFX    MVC   AIO,AIO1                                                         
         XMOD1                                                                  
*                                                                               
GETELFND L     R6,AIO                                                           
         GETELN (R6),DATADISP,ELCODE,1                                          
*                                                                               
SVAAMT   DS    PL6                                                              
SVFAMT   DS    PL6                                                              
SVJAMT   DS    PL6                                                              
SVNET    DS    PL6                                                              
SVEST    DS    CL2                                                              
SAVER6   DS    A                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              DSECTS ARE HIDDEN IN HERE                                        
         SPACE 3                                                                
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*DDTWABLDD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDTWABLDD                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*FAFACTS                                                                        
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE ACPRO32COM                                                     
* DSECT TO COVER COMMAND TABLE                                                  
*                                                                               
COMMTABD DSECT                     **COMMAND TABLE ENTRY**                      
COMMNAME DS    CL8                 COMMAND NAME                                 
COMMMINL DS    X                   MINIMUM LENGTH TO RECOGNIZE COMMAND          
COMMEDRT DS    XL2                 DISPLACEMENT TO EDIT ROUTINE                 
COMMCMRT DS    XL2                 DISPLACEMENT TO COMMAND ROUTINE              
COMMAUTH DS    X                   SECURITY CATEGORY                            
COMMTABL EQU   *-COMMTABD          TABLE ENTRY LENGTH                           
         SPACE 2                                                                
* DSECT TO COVER LOCAL WORKING STORAGE FOR NEWCOL ROUTINE                       
*                                                                               
NCWORKD  DSECT                     **NEWCOL LOCAL STORAGE**                     
NCRELO   DS    A                   ROUTINE RELOCATION FACTOR                    
NCSYSRD  DS    A                   SAVED SYSRD                                  
NCNCOLS  DS    XL1                                                              
NCEST    DS    0CL2                                                             
NCESTYPE DS    CL1                                                              
NCESTVER DS    XL1                                                              
NCMSG    DS    CL(L'CONHEAD)                                                    
NCWORKX  EQU   *                                                                
         SPACE 2                                                                
* DSECT TO COVER LOCAL WORKING STORAGE FOR REDIS ROUTINE                        
*                                                                               
RDWORKD  DSECT                     **REDIS LOCAL STORAGE**                      
RDRELO   DS    A                   ROUTINE RELOCATION FACTOR                    
RDSYSRD  DS    A                   SAVED SYSRD                                  
RDMSG    DS    CL(L'CONHEAD)                                                    
RDWORKX  EQU   *                                                                
         SPACE 2                                                                
* DSECT TO COVER LOCAL WORKING STORAGE FOR CPYSES ROUTINE                       
*                                                                               
CPWORKD  DSECT                     **REDIS LOCAL STORAGE**                      
CPRELO   DS    A                   ROUTINE RELOCATION FACTOR                    
CPFROMKY DS    CL(L'KEY)           "FROM" SESSION KEY                           
CPTOKY   DS    CL(L'KEY)           "TO" SESSION KEY                             
CPWORKX  EQU   *                                                                
         SPACE 2                                                                
* DSECT TO COVER LOCAL WORKING STORAGE FOR PERSUP ROUTINE                       
*                                                                               
PSWORKD  DSECT                     **PERSUP LOCAL STORAGE**                     
PSRELO   DS    A                   ROUTINE RELOCATION FACTOR                    
PSSYSRD  DS    A                   SAVED SYSRD                                  
PSNUM    DS    XL1                 HIGHEST REVISION NUMBER                      
PSNREVS  DS    XL1                 N'REVISIONS=SUPPLEMENT NUMBER                
PSMSG    DS    CL(L'CONHEAD)                                                    
PSWORKX  EQU   *                                                                
         SPACE 2                                                                
       ++INCLUDE DDPARSNIPD                                                     
         EJECT                                                                  
JBLOCKD  DSECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'102ACPRO42   08/30/05'                                      
         END                                                                    
