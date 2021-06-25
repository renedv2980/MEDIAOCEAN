*          DATA SET DDRUNON    AT LEVEL 004 AS OF 11/15/17                      
*PHASE RUNONA                                                                   
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE REGSAVE                                                                
         TITLE 'JOB RUN ON DAY CHECKER'                                         
********************************************************************            
* THIS MODULE CHECKS WHICH DAY THE JOB RUNS ON                     *            
* E.G. MONDAY, END OF MONTH, ETC.                                               
* RETURN CODE OF 0, IF CONDITIONS SATISFIED, 1 IF NOT                           
*                                                                               
* SUPPORTED CARDS:                                                              
* RUNON=                                                                        
*    SUPPORTED VALUES: STARTMONTH, ENDMONTH, MON-SUN,                           
*                      STARTQTR, ENDQTR, 01ST-31ST                              
* DATE=                                                                         
*    MODIFIES TODAY'S DATE                                                      
********************************************************************            
                                                                                
RUNON    CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**RUNON*,=V(REGSAVE),CLEAR=YES                       
         USING WORKD,RC                                                         
*                                                                               
* READ NEXT CONTROL CARD                                                        
*                                                                               
RUNO10   GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END OF INPUT?                                
         JE    RUNO500                                                          
*                                                                               
         CLC   =C'*',CARD          COMMENT?                                     
         JE    RUNO10              NEXT CONTROL CARD                            
*                                                                               
         CLC   =C'RUNON=',CARD                                                  
         JE    RUNO100                                                          
*                                                                               
         CLC   =C'DATE=',CARD                                                   
         JE    RUNO200                                                          
*                                                                               
         DC    H'0'                INVALID CONTROL CARD                         
*                                                                               
* VALIDATE RUNON= CARD                                                          
*                                                                               
RUNO100  DS    0H                                                               
         CLC   =C'STARTMONTH',CARD+6                                            
         JNE   RUNO110                                                          
*                                                                               
         CLI   REQMSE,X'00'                                                     
         JE    *+6                                                              
         DC    H'0'                ENDMONTH ALREADY REQUESTED                   
*                                                                               
         MVI   REQMSE,C'S'        DATCON: SET DAY TO FIRST DAY OF MONTH         
         J     RUNO10              NEXT CONTROL CARD                            
*                                                                               
RUNO110  DS    0H                                                               
         CLC   =C'ENDMONTH',CARD+6                                              
         JNE   RUNO115                                                          
*                                                                               
         CLI   REQMSE,X'00'                                                     
         JE    *+6                                                              
         DC    H'0'                STARTMONTH ALREADY REQUESTED                 
*                                                                               
         MVI   REQMSE,C'E'         DATCON: SET DAY TO LAST DAY OF MONTH         
         J     RUNO10              NEXT CONTROL CARD                            
*                                                                               
RUNO115  DS    0H                                                               
         CLC   =C'STARTQTR',CARD+6                                              
         JNE   RUNO120                                                          
*                                                                               
         CLI   REQQTR,X'00'                                                     
         JE    *+6                                                              
         DC    H'0'                ENDQTR ALREADY REQUESTED                     
*                                                                               
         MVI   REQQTR,C'S'                                                      
         J     RUNO10              NEXT CONTROL CARD                            
*                                                                               
RUNO120  DS    0H                                                               
         CLC   =C'ENDQTR',CARD+6                                                
         JNE   RUNO130                                                          
*                                                                               
         CLI   REQQTR,X'00'                                                     
         JE    *+6                                                              
         DC    H'0'                ENDQTR ALREADY REQUESTED                     
*                                                                               
         MVI   REQQTR,C'E'                                                      
         J     RUNO10              NEXT CONTROL CARD                            
                                                                                
*----------------------------------------------------------------------         
* SPECIFIC DAY OF THE MONTH                                                     
*----------------------------------------------------------------------         
RUNO130  DS    0H                  CHECK FOR VALID SUFFIX                       
         LA    R3,CARD+8           R3 POINTS TO SUFFIX                          
         CLI   CARD+9,C' '         3 CHARACTERS?                                
         BH    *+8                 NO: EXPECT 4                                 
         LA    R3,CARD+7           ADJUST START OF SUFFIX                       
         CLC   =C'ST',0(R3)        1ST                                          
         JE    RUNO132                                                          
         CLC   =C'ND',0(R3)        2ND                                          
         JE    RUNO132                                                          
         CLC   =C'RD',0(R3)        3RD                                          
         JE    RUNO132                                                          
         CLC   =C'TH',0(R3)        4TH,ETC                                      
         JNE   RUNO140             NOT VALID SUFFIX, IS ANOTHER KEYWORD         
*                                                                               
RUNO132  MVI   WORK,C'0'           INIT WORK                                    
         LA    R4,WORK             WORK WILL GET THE DAY OF MONTH               
         CLI   CARD+9,C' '         ONE DIGIT?                                   
         BH    *+8                 NO: EXPECT 2                                 
         LA    R4,1(,R4)           ADJUST FOR ONE DIGIT NUMBER                  
         MVC   0(2,R4),CARD+6      MOVE THE DAY OF MONTH TO WORK                
*                                                                               
         LA    R3,2                                                             
         LA    R4,WORK                                                          
RUNO134  CLI   0(R4),C'0'          VALIDATE NUMERIC                             
         BL    RUNO140                                                          
         CLI   0(R4),C'9'                                                       
         BH    RUNO140                                                          
         LA    R4,1(R4)                                                         
         BCT   R3,RUNO134                                                       
         CLC   WORK(2),=C'01'      AND 01-31                                    
         BL    RUNO140                                                          
         CLC   WORK(2),=C'31'                                                   
         BH    RUNO140                                                          
         MVC   REQDAYM,WORK        SAVE DAY OF THE MONTH                        
         J     RUNO10              NEXT CONTROL CARD                            
*                                                                               
* CHECK FOR 3-CHARACTER DAY OF THE WEEK                                         
*                                                                               
RUNO140  GOTO1 =V(DATVAL),DMCB,(X'03',CARD+6),(X'80',WORK)                      
         OC    DMCB(4),DMCB                                                     
         JZ    RUNO150             NOT D.O.W. - CHECK FOR SPECIFIC DATE         
*                                                                               
         MVC   REQDAY,CARD+6       VALID DAY HERE - SAVE AND CONTINUE           
         J     RUNO10              NEXT CONTROL CARD                            
*                                                                               
* CHECK FOR SPECIFIC DATE HERE                                                  
*                                                                               
RUNO150  DS    0H                                                               
         GOTO1 =V(DATVAL),DMCB,(X'00',CARD+6),(X'80',WORK)                      
         OC    DMCB(4),DMCB                                                     
         JNZ   *+6                                                              
         DC    H'0'                INVALID CONTROL CARD                         
*                                                                               
* INSERT SPECIFIC DATE INTO THE DATE LIST                                       
*                                                                               
         LAY   R1,REQDATES                                                      
         LHI   R0,REQDATEN                                                      
         CLI   0(R1),X'00'         ANYTHING THERE?                              
         JE    *+14                                                             
         LA    R1,L'REQDATES(R1)                                                
         BRCT  R0,*-12                                                          
         DC    H'0'                REQDATES TABLE FULL                          
*                                                                               
         MVC   0(L'REQDATES,R1),WORK                                            
         J     RUNO10              NEXT CONTROL CARD                            
*                                                                               
* VALIDATE DATE= CARD                                                           
* THIS CARD OVERRIDES THE VALUE OF TODAY'S DATE                                 
*                                                                               
RUNO200  DS    0H                                                               
         GOTO1 =V(DATVAL),DMCB,(X'00',CARD+5),(X'80',TODAY)                     
         OC    DMCB(4),DMCB                                                     
         JNZ   *+6                                                              
         DC    H'0'                INVALID CONTROL CARD                         
*                                                                               
         J     RUNO10              NEXT CONTROL CARD                            
                                                                                
***********************************************************************         
* ALL CONTROL CARDS HAVE BEEN READ IN HERE                                      
***********************************************************************         
RUNO500  DS    0H                                                               
         XC    RETCODE,RETCODE                                                  
         OC    TODAY,TODAY         OVERRIDE FOR TODAY'S DATE?                   
         JNZ   RUNO510                                                          
*                                                                               
* NO OVERRIDE - PUT IN TODAY'S DATE                                             
         GOTO1 =V(DATCON),DMCB,(5,0),(X'20',TODAY)                              
*                                                                               
RUNO510  DS    0H                                                               
         CLI   REQMSE,X'00'        MONTH START/END REQUESTED?                   
         JE    RUNO540                                                          
*                                                                               
         CLI   REQMSE,C'S'         START OF MONTH REQUESTED?                    
         JNE   *+8                                                              
         MVI   BYTE,X'00'     SET FIRST DAY DATE MODIFIER FOR DATCON            
         CLI   REQMSE,C'E'         END OF MONTH REQUESTED?                      
         JNE   *+8                                                              
         MVI   BYTE,X'01'     SET LAST DAY DATE MODIFIER FOR DATCON             
*                                                                               
* SET DATE TO FIRST/LAST DAY OF THE MONTH (BYTE=0/1)                            
*                                                                               
         GOTO1 =V(DATCON),DMCB,(X'30',TODAY),(X'20',WORK),(BYTE,0)              
*                                                                               
         CLC   TODAY,WORK          JOB RUNS ON THE REQUESTED DATE?              
         JNE   RUNORET1                                                         
*                                                                               
RUNO540  DS    0H                                                               
         OC    REQDAYM,REQDAYM     SPECIFIC DAY OF THE MONTH REQUESTED?         
         JZ    RUNO550                                                          
*                                                                               
         CLC   REQDAYM,TODAY+4     JOB RUNS ON REQUESTED DAY OF MONTH?          
         JNE   RUNORET1                                                         
*                                                                               
RUNO550  DS    0H                                                               
         OC    REQDAY,REQDAY       SPECIFIC DAY OF THE WEEK REQUESTED?          
         JZ    RUNO560                                                          
*                                                                               
         GOTO1 =V(GETDAY),DMCB,TODAY,WORK     GET DAY OF THE WEEK               
*                                                                               
         CLC   REQDAY,WORK         JOB RUNS ON REQUESTED DAY OF WEEK?           
         JNE   RUNORET1                                                         
*                                                                               
RUNO560  DS    0H                                                               
         OC    REQDATES,REQDATES   SPECIFIC RUN DATES REQUESTED?                
         JZ    RUNO600             ALL DONE, SET RET CODE OF ZERO               
*                                                                               
         LAY   R1,REQDATES                                                      
         LHI   R0,REQDATEN                                                      
RUNO570  CLI   0(R1),X'00'         ANYTHING THERE?                              
         JE    RUNORET1            REACHED EOT, DATE NOT FOUND                  
         CLC   TODAY,0(R1)                                                      
         JE    RUNO600             SUCCESSFUL MATCH, DO NEXT FILTER             
         LA    R1,L'REQDATES(R1)                                                
         BRCT  R0,RUNO570                                                       
         J     RUNORET1            REACHED EOT, DATE NOT FOUND                  
*                                                                               
RUNO600  DS    0H                                                               
         CLI   REQQTR,X'00'        QUARTER START/END REQUESTED?                 
         JE    RUNOEXIT            ALL DONE, SET RET CODE OF ZERO               
*                                                                               
         CLI   REQQTR,C'S'         QUARTER START REQUESTED?                     
         JNE   RUNO650                                                          
*                                                                               
         CLC   =C'0101',TODAY+2                                                 
         JE    RUNO900                                                          
         CLC   =C'0401',TODAY+2                                                 
         JE    RUNO900                                                          
         CLC   =C'0701',TODAY+2                                                 
         JE    RUNO900                                                          
         CLC   =C'1001',TODAY+2                                                 
         JNE   RUNORET1                                                         
*                                                                               
RUNO650  DS    0H                                                               
         CLI   REQQTR,C'E'         QUARTER END REQUESTED?                       
         JNE   RUNOEXIT                                                         
*                                                                               
         CLC   =C'0331',TODAY+2                                                 
         JE    RUNO900                                                          
         CLC   =C'0630',TODAY+2                                                 
         JE    RUNO900                                                          
         CLC   =C'0930',TODAY+2                                                 
         JE    RUNO900                                                          
         CLC   =C'1231',TODAY+2                                                 
         JNE   RUNORET1                                                         
*                                                                               
RUNO900  DS    0H                                                               
         J     RUNOEXIT            PASSED ALL FILTERS, EXIT WITH RC=0           
*                                                                               
RUNORET1 DS    0H                                                               
         MVI   RETCODE+3,1         SET RC=1                                     
*                                                                               
RUNOEXIT XBASE RC=RETCODE                                                       
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* WORKING STORAGE                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WORKD    DSECT                                                                  
DMCB     DS    6F                                                               
RETCODE  DS    F                                                                
TODAY    DS    CL6                                                              
CARD     DS    CL80                                                             
WORK     DS    CL60                                                             
BYTE     DS    X                                                                
*                                                                               
REQUESTS DS    0XL(REQLQ)                                                       
REQ1     EQU   *                                                                
REQMSE   DS    C                   MONTH START OR END                           
REQQTR   DS    C                   QUARTER START OR END                         
REQDAY   DS    CL3                 DAY OF THE WEEK                              
REQDAYM  DS    CL2                 DAY OF THE MONTH                             
REQDATEN EQU   100                 100 DATES                                    
REQDATES DS    (REQDATEN)CL6       LIST OF SPECIFIC DATES                       
REQLQ    EQU   *-REQ1                                                           
*                                                                               
WORKX    EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004DDRUNON   11/15/17'                                      
         END                                                                    
