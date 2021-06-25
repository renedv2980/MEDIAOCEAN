*          DATA SET DDUNCATLG  AT LEVEL 002 AS OF 05/01/02                      
*PHASE UNCATLG,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE REGSAVE                                                                
         TITLE 'UNCATLG - UNCATALOG DATASETS'                                   
UNCATLG  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WRKX-WRKD,UNCATLG,=V(REGSAVE)                                    
         USING WRKD,RC                                                          
         OPEN  (SYSIN)                                                          
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         MVC   P,SPACES                                                         
         ZAP   COUNT,=P'0'                                                      
         EJECT                                                                  
*              ROUTINE READS FILE AND PRINTS REPORT                             
         SPACE 1                                                                
UNC2     GET   SYSIN,CARD                                                       
         CLC   CARD+5(4),=C'.SPF'  LOOKING FOR SPF                              
         BNE   UNC2                                                             
         CLC   CARD+12(8),=C'.OUTLIST' OUTLIST DATA SETS                        
         BNE   UNC2                                                             
         AP    COUNT,=P'1'      ADD TO N'DSNS FOUND                             
         BAS   RE,ALLOCATE      DYNAMICALLY ALLOCATE DD STMT TO UNCATLG         
         BZ    *+10                                                             
         MVC   P+25(19),=C'* NOT UNCATALOGED *'                                 
         MVC   P+1(19),CARD+1                                                   
         BAS   RE,PRNTIT        PRINT THE DSN                                   
         B     UNC2                                                             
         SPACE 1                                                                
UNCX     CLOSE SYSIN                                                            
         XBASE                                                                  
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE                                          
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         CP    LINE,=P'57'                                                      
         BL    *+8                                                              
         BAS   RE,HEADUP                                                        
         EDIT  (1,SPACING),(2,PARAS),FILL=0                                     
         AP    LINE,DUB                                                         
         MVC   DUB(2),=C'BL'                                                    
         MVC   DUB+2(2),PARAS                                                   
         GOTO1 =V(PRINT),PARAS,P-1,DUB                                          
         MVI   SPACING,1           RESET SPACING                                
         MVC   P,SPACES            CLEAR PRINT LINE                             
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO PRINT HEADINGS                                        
         SPACE 1                                                                
HEADUP   NTR1                                                                   
         GOTO1 =V(PRINT),PARAS,SPACES-1,=C'BC01'                                
         ZAP   LINE,=P'0'                                                       
         AP    PAGE,=P'1'                                                       
         SPACE 1                                                                
         MVC   PSAVE,P             SAVE CURRENT PRINT LINE                      
         MVC   P,SPACES                                                         
         LA    R2,P+((80-L'TITLE)/2)                                            
         MVC   0(L'TITLE,R2),TITLE     PRINT TITLE                              
         MVC   P+70(4),=C'PAGE'                                                 
         EDIT  PAGE,(4,P+75),ALIGN=LEFT                                         
         BAS   RE,PRNTIT                                                        
         MVC   0(L'TITLE,R2),TITLE2    UNDERLINE                                
         MVI   SPACING,3                                                        
         BAS   RE,PRNTIT                                                        
         MVC   P,PSAVE             RESTORE CURRENT PRINT LINE                   
         B     XIT                                                              
         SPACE 3                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE GENERATES DD STATEMENT FOR UNCATALOG                     
         SPACE 1                                                                
ALLOCATE NTR1                                                                   
         LA    R1,RBLK             SET UP REQ BLK POINTER                       
         ST    R1,ARBLK                                                         
         OI    ARBLK,X'80'                                                      
*                                                                               
ALLOC0   MVI   RBLKLEN,20          SET UP REQ BLK LEN                           
         MVI   RBLKVERB,1          SET ALLOCATE BY DSN VERB CODE                
         LA    R1,ATXT                                                          
         ST    R1,RBLKATXT         SET POINTER TO TEXT POINTER LIST             
*                                                                               
ALLOC1   LA    R1,TXTDD            SET UP POINTER TO DDNAME TEXT                
         ST    R1,ATXTDD                                                        
         MVC   0(2,R1),=X'0001'    SET DDNAME TEXT CODE                         
         MVC   2(2,R1),=X'0001'                                                 
         MVC   4(2,R1),=X'0005'                                                 
         MVC   6(2,R1),=C'DD'                                                   
         UNPK  8(3,R1),COUNT                                                    
         OI    10(R1),X'F0'                                                     
*                                                                               
ALLOC2   LA    R1,TXTDSN           SET UP POINTER TO DSN TEXT                   
         ST    R1,ATXTDSN                                                       
         MVC   0(2,R1),=X'0002'    SET DSN TEXT CODE                            
         MVC   2(2,R1),=X'0001'                                                 
         MVC   4(2,R1),=X'0013'    UUUU.SPF999.OUTLIST                          
         MVC   6(19,R1),CARD+1     MOVE DSN TO TEXT BLOCK                       
*                                                                               
ALLOC4   LA    R1,TXTDISP          SET UP POINTER TO DISP TEXT                  
         ST    R1,ATXTDISP                                                      
         MVC   0(2,R1),=X'0004'    SET DISP TEXT CODE                           
         MVC   2(2,R1),=X'0001'                                                 
         MVC   4(2,R1),=X'0001'                                                 
         MVI   6(R1),X'01'         SET DISP=OLD CODE                            
*                                                                               
ALLOC5   LA    R1,TXTNDISP         SET UP POINTER TO NORM DISP TEXT             
         ST    R1,ATXTNDSP                                                      
         MVC   0(2,R1),=X'0005'    SET NORMAL DISP TEXT CODE                    
         MVC   2(2,R1),=X'0001'                                                 
         MVC   4(2,R1),=X'0001'                                                 
         MVI   6(R1),X'01'         SET DISP=UNCATLG CODE                        
*                                                                               
         OI    ATXTLAST,X'80'      SET END OF LIST                              
*                                                                               
ALLOCLST LA    R1,ARBLK                                                         
         DYNALLOC                                                               
         LTR   RF,RF               TEST FOR ERRORS - RETURN CC                  
         B     XIT                                                              
         EJECT                                                                  
*              CONSTANTS                                                        
         SPACE 2                                                                
PAGE     DC    PL4'0'                                                           
LINE     DC    PL2'99'                                                          
SPACING  DC    X'01'                                                            
TITLE    DC    C'DDS UNCATALOG FACILITY'                                        
TITLE2   DC    C'----------------------'                                        
         SPACE 1                                                                
SYSIN    DCB   DDNAME=SYSIN,MACRF=(GM),DSORG=PS,RECFM=FB,              X        
               EODAD=UNCX,LRECL=121,BLKSIZE=121                                 
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER LOCAL STORAGE                                     
         SPACE 2                                                                
WRKD     DSECT                                                                  
DUB      DS    D                                                                
PARAS    DS    6F                                                               
COUNT    DS    PL2                                                              
CARD     DS    CL121                                                            
WORK     DS    CL132                                                            
         DS    C                                                                
P        DS    CL132                                                            
         DS    C                                                                
SPACES   DS    CL132                                                            
PSAVE    DS    CL132                                                            
         SPACE 1                                                                
ARBLK    DS    A                   REQUEST BLOCK POINTER                        
*                                                                               
RBLK     DS    0XL20               REQUEST BLOCK                                
RBLKLEN  DS    XL1                                                              
RBLKVERB DS    XL1                                                              
         DS    XL2                                                              
RBLKERR  DS    XL2                                                              
RBLKINFO DS    XL2                                                              
RBLKATXT DS    A                                                                
         DS    XL4                                                              
RBLKFLAG DS    XL4                                                              
*                                                                               
ATXT     DS    0A                  TEXT BLOCK POINTERS                          
ATXTDD   DS    A                                                                
ATXTDSN  DS    A                                                                
ATXTDISP DS    A                                                                
ATXTNDSP DS    A                                                                
         ORG   *-4                                                              
ATXTLAST DS    A                   LAST TEXT BLOCK POINTER                      
*                                                                               
TXTDD    DS    XL6,CL9             TEXT BLOCK DDNAME        0001                
TXTDSN   DS    XL6,CL30            TEXT BLOCK DSN           0002                
TXTDISP  DS    XL6,XL1             TEXT BLOCK DISP          0004                
TXTNDISP DS    XL6,XL1             TEXT BLOCK NORM DISP     0005                
*                                                                               
WRKX     EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DDUNCATLG 05/01/02'                                      
         END                                                                    
