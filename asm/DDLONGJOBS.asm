*          DATA SET DDLONGJOBS AT LEVEL 038 AS OF 05/10/04                      
*PHASE LONGJOBA                                                                 
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
         PRINT NOGEN                                                            
LONGJOB  TITLE 'LONG SOON JOB ANALYSIS'                                         
*=============================================================                  
* CREATE AN EXTRACT OF LONG SOON JOBS TO TAPEOUT AND                            
* MERGE ALL THE INPUT FILES TO HISTOUT                                          
*=============================================================                  
LONGJOB  CSECT                                                                  
         NBASE WORKL,*LONGJOB,=A(WORKAREA),RA                                   
         USING WORKD,RC                                                         
*                                                                               
         BRAS  RE,INIT             INITIALISE JOB                               
         BRAS  RE,PROCESS          BUILD TAPEIN OUTPUT                          
*                                                                               
XBASE    CLOSE TAPEIN                                                           
         CLOSE TAPEOUT                                                          
         CLOSE HISTOUT                                                          
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD OUTPUT RECORDS FROM SOON.LONG.MONSTER.*                       *         
***********************************************************************         
         SPACE 1                                                                
PROCESS  NTR1  ,                                                                
         OPEN  (TAPEIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         OPEN  (HISTOUT,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PR10     LA    R0,RECOUT           CLEAR SAVE RECORD AREA                       
         LHI   R1,RECOUTX-RECOUT                                                
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
* FILL HEADER AREA WITH TABS !                                                  
         LA    R0,RECOUT                                                        
         LHI   R1,RECREQ2-RECOUT                                                
         LA    RF,X'05'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
PR11     GET   TAPEIN,RECIN                                                     
         PUT   HISTOUT,RECIN                                                    
*                                                                               
         CLC   =C'**',RECIN        SEEING STARS YET?                            
         BNE   PR11                                                             
*                                                                               
         MVI   SPOOFJOB,C'N'                                                    
         CLC   =C'SPOOF',RECIN+15                                               
         BNE   *+8                                                              
         MVI   SPOOFJOB,C'Y'                                                    
*                                                                               
         MVC   RECUSER,RECIN+4                                                  
         MVC   RECJOB,RECIN+12                                                  
         MVC   RECDATE,RECIN+23                                                 
         MVC   RECMONST(1),RECIN+53  MOVE ADV NUMBER                            
         MVI   RECMONST+1,C' '                                                  
         CLI   RECIN+54,C'.'       TEST TERMINATOR                              
         BE    *+8                                                              
         MVI   RECMONST+1,C'S'     INDICATE UPDATIVE SOON                       
         MVC   RECSUBTM,RECIN+32                                                
*                                                                               
PR12     GET   TAPEIN,RECIN        REQUEST CARDS FOLLOW RFHDR                   
         PUT   HISTOUT,RECIN                                                    
         CLC   =C'RFHDR',RECIN                                                  
         BNE   PR12                                                             
*                                                                               
         LA    R4,RECREQ1                                                       
*                                                                               
PR14     GET   TAPEIN,RECIN        READ  REQUEST CARD                           
         PUT   HISTOUT,RECIN                                                    
         CLC   =C'/*',RECIN                                                     
         BE    PR30                                                             
         CLI   SPOOFJOB,C'Y'                                                    
         BE    PR20                                                             
         MVC   0(80,R4),RECIN                                                   
         MVI   80(R4),X'05'                                                     
         LA    R4,81(R4)                                                        
         B     PR14                                                             
*                                                                               
PR20     LA    R5,RECIN+12                                                      
         LA    R4,RECREQ1          POINT TO OUTPUT AREA                         
*                                                                               
PR22     CLC   0(2,R5),=C'02'                                                   
         BE    PR24                                                             
         CLC   0(2,R5),=C'05'                                                   
         BE    PR25                                                             
         CLC   0(2,R5),=C'12'      FIELDNUM AT LEAST 12                         
         BL    PR26                                                             
* MOVE FIELD DATA                                                               
PR24     CLC   SVFLDNUM,0(R5)      TEST SAME FIELD NUMBER AS PREV               
         BNE   *+6                                                              
         BCTR  R4,0                BACK UP TO CONCATENATE                       
*                                                                               
         MVC   SVFLDNUM,0(R5)      SAVE FIELD NUMBER                            
         PACK  DUB,2(2,R5)                                                      
         CVB   RE,DUB                                                           
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),4(R5)                                                    
         B     PR25X                                                            
*                                                                               
PR25     PACK  DUB,2(2,R5)         05XXSOON,WHO                                 
         CVB   RE,DUB                                                           
         AHI   RE,-6               DROP SOON, AND 1 FOR EX                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),9(R5)                                                    
*                                                                               
PR25X    LA    R4,1(RE,R4)         POINT TO END                                 
         MVI   0(R4),C' '          ADD A SPACE                                  
         LA    R4,1(R4)                                                         
*                                                                               
PR26     PACK  DUB,2(2,R5)                                                      
         CVB   RE,DUB                                                           
         LA    R5,4(RE,R5)         POINT BEYOND FIELD                           
*                                                                               
         CLI   0(R5),C'*'          TEST E-O-R                                   
         BE    PR28                                                             
*                                                                               
         CLI   1(R5),C' '          ANYTHING MORE ON THIS CARD                   
         BNH   PR28                NO - GET NEXT CARD                           
         LA    R5,1(R5)            POINT TO FIELD NUMBER                        
         B     PR22                                                             
*                                                                               
PR28     GET   TAPEIN,RECIN                                                     
         PUT   HISTOUT,RECIN                                                    
         CLC   =C'/*',RECIN                                                     
         BE    PR30                                                             
         LA    R5,RECIN+12         POINT TO FIRST DATA FIELD                    
         B     PR22                                                             
*                                                                               
PR30     MVI   RECREQ1+80,X'05'                                                 
*                                                                               
         GET   TAPEIN,RECIN        GET TCB/ELAPSED TIMES                        
         PUT   HISTOUT,RECIN                                                    
*                                                                               
         MVC   JOBLINE,RECIN                                                    
         MVC   RECWAIT,LSSTIME     WAIT TIME                                    
         MVI   RECWAIT+2,C'.'                                                   
         MVI   RECWAIT+5,C'.'                                                   
*                                                                               
         MVC   RECELAPS,LSETIME    ELAPSED TIME                                 
         MVI   RECELAPS+2,C'.'                                                  
         MVI   RECELAPS+5,C'.'                                                  
*                                                                               
         MVC   RECTCBTM,LSTTIME    TCB TIME                                     
         MVI   RECTCBTM+2,C'.'                                                  
         MVI   RECTCBTM+5,C'.'                                                  
*                                                                               
         MVC   RECTOTTM,LSTOTT     TOTAL TIME                                   
         MVI   RECTOTTM+2,C'.'                                                  
         MVI   RECTOTTM+5,C'.'                                                  
*                                                                               
         MVI   RECOUT+159,X'0D'    SET LINEFEED                                 
*                                                                               
         PUT   TAPEOUT,RECOUT                                                   
         B     PR10                                                             
*                                                                               
JOBLINE  DS    0XL80                                                            
         DC    C'WT='              WAIT TIME                                    
LSSTIME  DS    CL8' '                                                           
         DC    C',RT='             RUN TIME                                     
LSETIME  DS    CL8' '                                                           
         DC    C',TT='             TCB TIME                                     
LSTTIME  DS    CL8' '                                                           
         DC    C',MC='             MONSOON CPU                                  
LSMCPU   DS    CL12' '                                                          
         DC    C',JC='             JOBSTEP CPU                                  
LSJCPU   DS    CL12' '                                                          
         DC    C',TT='             TOTAL ELAPSED TIME                           
LSTOTT   DC    CL8' '                                                           
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
*                                                                               
         XC    WORK,WORK                                                        
         ST    RB,WORK                                                          
         L     RE,=A(WORKAREX)                                                  
         ST    RE,WORK+4                                                        
         OI    WORK+4,X'80'                                                     
         GOTO1 =V(STXITER),DMCB,WORK                                            
                                                                                
         OPEN  (TAPEOUT,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
* PUT COLUMN HEADINGS ONCE                                                      
         XC    RECOUT(160),RECOUT                                               
         MVC   RECOUT(COLHEADX-COLHEADS),COLHEADS                               
         MVI   RECOUT+159,X'0D'    PUT LINEFEED                                 
         PUT   TAPEOUT,RECOUT                                                   
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
SPOOFJOB DC    C'N'                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
* DCBS AND ADCONS                                                     *         
***********************************************************************         
         SPACE 1                                                                
TAPEIN   DCB   DSORG=PS,MACRF=GM,DDNAME=TAPEIN,RECFM=FB,LRECL=80,      +        
               EODAD=XBASE                                                      
*                                                                               
TAPEOUT  DCB   DSORG=PS,MACRF=PM,DDNAME=TAPEOUT,RECFM=FB,LRECL=160,    +        
               BLKSIZE=24000                                                    
*                                                                               
HISTOUT  DCB   DSORG=PS,MACRF=PM,DDNAME=HISTOUT,RECFM=FB,LRECL=80,     +        
               BLKSIZE=24000                                                    
*                                                                               
         DS    0D                                                               
         DC    CL8'**RECIN*'                                                    
RECIN    DS    CL80                                                             
         DS    CL24                                                             
*                                                                               
COLHEADS DC    C'USER-ID'                                                       
         DC    X'05'                                                            
         DC    C'JOB'                                                           
         DC    X'05'                                                            
         DC    C'DATE'                                                          
         DC    X'05'                                                            
         DC    C'BY'                                                            
         DC    X'05'                                                            
         DC    C'SUB'                                                           
         DC    X'05'                                                            
         DC    C'WAIT'                                                          
         DC    X'05'                                                            
         DC    C'ELAPS'                                                         
         DC    X'05'                                                            
         DC    C'CPU'                                                           
         DC    X'05'                                                            
         DC    C'TOTAL REAL'                                                    
         DC    X'05'                                                            
COLHEADX EQU   *                                                                
         DS    0D                                                               
         DC    CL8'*RECOUT*'                                                    
RECOUT   DS    0XL160                                                           
RECUSER  DS    CL8                                                              
         DS    CL1                                                              
RECJOB   DS    CL3                                                              
         DS    CL1                                                              
RECDATE  DS    CL5                 MMMDD                                        
         DS    CL1                                                              
RECMONST DS    CL2                                                              
         DS    CL1                                                              
RECSUBTM DS    CL5                 HH:MM                                        
         DS    CL1                                                              
RECWAIT  DS    CL8                 HH:MM:SS                                     
         DS    CL1                                                              
RECELAPS DS    CL8                 HH:MM:SS                                     
         DS    CL1                                                              
RECTCBTM DS    CL8                 HH:MM:SS                                     
         DS    CL1                                                              
RECTOTTM DS    CL8                 HH:MM:SS                                     
         DS    CL1                                                              
RECREQ1  DS    CL80                                                             
         DS    CL1                                                              
RECREQ2  DS    CL18                                                             
* FOLLOWING DATA NOT PART OF OUTPUT RECORDS                                     
         DS    CL52                                                             
RECREQ3  DS    CL80                                                             
RECREQ4  DS    CL80                                                             
RECREQ5  DS    CL80                                                             
RECREQ6  DS    CL80                                                             
RECREQ7  DS    CL80                                                             
RECREQ8  DS    CL80                                                             
RECREQ9  DS    CL80                                                             
RECREQA  DS    CL80                                                             
RECOUTX  EQU   *                                                                
         DS    9CL80               PROTECT AGAINST > 10 REQ CARDS               
*                                                                               
WORKAREA DS    0D                                                               
         DS    CL10000                                                          
WORKAREX EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
WORK     DS    XL64                                                             
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
SVFLDNUM DS    H                                                                
*                                                                               
DMCB     DS    6F                                                               
*                                                                               
CARD     DS    CL80                                                             
EOF      DS    X                                                                
WORKL    EQU   *-WORKD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038DDLONGJOBS05/10/04'                                      
         END                                                                    
