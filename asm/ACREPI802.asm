*          DATA SET ACREPI802  AT LEVEL 041 AS OF 05/01/02                      
*PHASE ACI802A                                                                  
         TITLE 'ACI802 - PRODUCTION VENDOR TAPE FOR OGILVY'                     
ACI802   CSECT                                                                  
         DS    CL4000                                                           
         ORG   ACI802                                                           
         PRINT NOGEN                                                            
         NMOD1 0,**ACI8**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACI8D,RC                                                         
         LA    R9,OUTAREA                                                       
         USING RECD,R9                                                          
         LA    R8,P                                                             
         USING PFIELDSD,R8                                                      
         EJECT                                                                  
*              ROUTINES EXECUTED AT FIRST FOR RUN                               
         SPACE 2                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   I810                                                             
         ZAP   COUNT,=P'0'           INIT # OF TAPES REQUESTED                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES EXECUTED AT FIRST FOR REQUEST                           
         SPACE 2                                                                
I810     CLI   MODE,REQFRST                                                     
         BNE   I820                                                             
         SPACE                                                                  
         MVI   RCSUBPRG,0                                                       
         ZAP   NUMREC,=P'0'        INIT NUMBER OF RECORDS PROCESSED             
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         SPACE                                                                  
         CLI   QOPT1,C'Y'                                                       
         BNE   XIT                                                              
         AP    COUNT,=P'1'                                                      
         CVB   R4,COUNT                                                         
         MVC   DSNAME+13(2),ALPHAID                                             
         GOTO1 DYNALLOC,DMCB,(0,=CL8'TAPE'),((R4),DSNAME)                       
         OPEN  (TAPE,OUTPUT)                                                    
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R3,DATADISP,ELCODE                                               
         EJECT                                                                  
*              ROUTINES EXECUTED AT PROCESS AN ACCOUNT RECORD                   
         SPACE 2                                                                
I820     CLI   MODE,PROCACC                                                     
         BNE   I830                                                             
         MVI   SPACING,X'02'       SET DOUBLE SPACING                           
         MVC   OUTAREA,SPACES      SET OUTAREA TO BLANKS                        
         AP    NUMREC,=P'1'        INCREMENT RECORD COUNTER                     
         L     R3,ADACC                                                         
         MVC   PFVNUM,4(R3)        SKIP P AT BEG OF ACCOUNT                     
         USING ACOTHERD,R3                                                      
         MVI   ELCODE,X'23'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   PFNUMBER,ACOTNUM    SS OR FED ID                                 
         L     R4,ADACCADD         22 ELEMENT                                   
         USING ACADDD,R4                                                        
         MVC   RCADD1,ACADADD      ADDRESS LINE 1                               
         MVC   PFADD,ACADADD                                                    
         CLI   ACADLNES,X'01'                                                   
         BE    I824                                                             
         CLI   ACADLNES,X'04'                                                   
         BE    I821                                                             
         CLI   ACADLNES,X'02'                                                   
         BE    I823                                                             
*              3 LINES OF ADDRESS, CHECK IF STATE AND ZIP IN 3RD LINE           
         CLC   ACADADD+61(17),SPACES STATE AND ZIP TOGETHER IS LENGTH 9         
         BNE   I822                                                             
         CLC   ACADADD+54(2),SPACES ALWAYS 2 SPACES BETW SATE AND ZIP           
         BNE   I822                                                             
         MVC   PFADD+132(25),ACADADD+26 ADDRESS LINE 2                          
         MVC   RCCITY,ACADADD+26                                                
         MVC   PFADD+264(25),ACADADD+52 ADDRESS LINE 3                          
         MVC   RCSTATE,ACADADD+52                                               
         MVC   RCZIP,ACADADD+56                                                 
         B     I824                                                             
I821     MVC   PFADD+396(25),ACADADD+78 ADDRESS LINE 4                          
         MVC   RCSTATE,ACADADD+78                                               
         MVC   RCZIP,ACADADD+82                                                 
I822     MVC   PFADD+264(25),ACADADD+52 ADDRESS LINE 3                          
         MVC   RCCITY,ACADADD+52                                                
I823     MVC   PFADD+132(25),ACADADD+26 ADDRESS LINE 2                          
         MVC   RCADD2,ACADADD+26                                                
I824     L     R4,ADACCSTA         30 ELEMENT                                   
         USING ACSTATD,R4                                                       
         TM    ACSTSTAT,X'20'                                                   
         BNO   *+8                 BRANCH IF NOT LOCKED                         
         MVI   PFDFLAG,C'D'                                                     
         TM    ACSTSTAT,X'02'                                                   
         BNO   *+8                 BRANCH IF NOT 1099                           
         MVI   PF1099,C'Y'                                                      
         L     R3,ADACCNAM         20 ELEMENT                                   
         USING ACNAMED,R3                                                       
         CLI   ACNMLEN,X'27'                                                    
         BNL   I826                LENGTH >= 25                                 
         ZIC   R4,ACNMLEN          LENGTH < 25                                  
         SH    R4,=H'3'                                                         
         B     *+8                                                              
I826     LA    R4,34               LENGTH >= 25, JUST MOVE 25                   
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   PFVNAME(0),ACNMNAME VENDOR NAME                                  
         CLI   QOPT1,C'Y'                                                       
         BNE   I828                                                             
         MVC   RCVNUM,PFVNUM                                                    
         MVC   RCDFLAG,PFDFLAG                                                  
         MVC   RCVNAME,PFVNAME                                                  
         MVC   RCNUMBER,PFNUMBER                                                
         MVC   RC1099,PF1099                                                    
         PUT   TAPE,OUTAREA                                                     
I828     GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES EXECUTED AT LAST FOR REQUEST                            
         SPACE 2                                                                
I830     CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         MVC   P+1(L'ENDING),ENDING                                             
         LA    R7,P+L'ENDING+2                                                  
         EDIT  NUMREC,(7,(R7)),COMMAS=YES,ALIGN=LEFT                            
         MVI   SPACING,X'02'                                                    
         GOTO1 ACREPORT                                                         
         CLI   QOPT1,C'Y'                                                       
         BNE   XIT                                                              
         CLOSE (TAPE)                                                           
         B     XIT                                                              
         EJECT                                                                  
*              CONSTANTS                                                        
         SPACE                                                                  
TAPE     DCB   DSORG=PS,MACRF=PM,DDNAME=TAPE,BLKSIZE=13200,LRECL=132            
         LTORG                                                                  
         SPACE 2                                                                
ENDING   DC    C'TOTAL NUMBER OF RECORDS ='                                     
DSNAME   DC    CL20'ACCTAPE.AC0I8  1'                                           
         EJECT                                                                  
*              DSECT FOR PROGRAM                                                
         SPACE                                                                  
ACI8D    DSECT                                                                  
OUTAREA  DS    CL132                                                            
NUMREC   DS    PL4                 NUMBER OF RECORDS PROCESSED                  
ELCODE   DS    XL1                 FOR GETELS, ETC.                             
         DS    0D                                                               
COUNT    DS    D                   # OF TAPES REQUESTED                         
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
         SPACE                                                                  
PFIELDSD DSECT                                                                  
         DS    C                                                                
PFVNUM   DS    CL6                                                              
         DS    CL4                                                              
PFDFLAG  DS    CL1                                                              
         DS    CL2                                                              
PFVNAME  DS    CL25                                                             
         DS    CL2                                                              
PFADD    DS    CL25                                                             
         DS    CL2                                                              
PF1099   DS    CL1                                                              
         DS    CL5                                                              
PFNUMBER DS    CL9                                                              
         SPACE 3                                                                
*              DSECT TO COVER OUTPUT RECORDS                                    
         SPACE                                                                  
RECD     DSECT                                                                  
         DS    CL2                                                              
RCVNUM   DS    CL6                                                              
RCDFLAG  DS    CL1                                                              
RCVNAME  DS    CL25                                                             
         DS    CL1                                                              
RCADD1   DS    CL25                                                             
         DS    CL1                                                              
RCADD2   DS    CL24                                                             
         DS    CL1                                                              
RCCITY   DS    CL24                                                             
         DS    CL1                                                              
RC1099   DS    CL1                                                              
         DS    CL1                                                              
RCSTATE  DS    CL2                                                              
RCZIP    DS    CL5                                                              
         DS    CL1                                                              
RCNUMBER DS    CL9                                                              
         DS    CL2                                                              
         EJECT                                                                  
*        ACGENBOTH                                                              
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041ACREPI802 05/01/02'                                      
         END                                                                    
