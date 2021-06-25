*          DATA SET ACLDXAPR   AT LEVEL 001 AS OF 08/21/19                      
*PHASE ACXAPRA                                                                  
*INCLUDE ACRECTYP                                                               
*INCLUDE SORTER                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE CUREDIT                                                                
*INCLUDE HEXOUT                                                                 
         TITLE 'ADD EXISTING TIME BACK UP APPROVERS AS TIME OFF APPRS'          
***********************************************************************         
* THIS CONVERSION FINDS OUT ALL APPROVER RECORDS FOR A FILE & LOOK FOR*         
* THE LIDELD ELEMENT WHERE THE TYPE IS LIDTBACK AND THEN FOR ENTRIES  *         
* WHERE LIDLAPPL IS LIDLTIME.                                         *         
* FOR SUCH RECORDS SET LIDLAPP2 TO LIDLTOFF AS PER DSRD-23318         *         
***********************************************************************         
DMLDEXT  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         B     DMXCTL                                                           
         DC    C'*VERIFY*'                                                      
         DC    X'FB'                                                            
         DC    C'90101'                                                         
         DC    X'FB'                                                            
         DC    C'91231'                                                         
         DC    C'*',X'FF'          FILE = ALL                                   
DMXRTST  DS    0H                                                               
*        B     DMXPURGE            PURGE UNDER TEST                             
         B     DMXKEEP             KEEP WHEN LIVE                               
         EJECT                                                                  
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST(PLISTL),0(R1)                                              
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    DMXRET                                                           
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
         EJECT                                                                  
DMXEOF   DS    0H                                                               
         MVI   P,C' '              INIT PRINTLINE                               
         MVC   P+1(L'P-1),P                                                     
         MVC   P+1(42),=C'NO# OF BACK-UP APPR SET AS TIME-OFF APPR ='           
         CURED CHANGES,(12,P+45),0,MINUS=YES                                    
         GOTO1 VPRINTER                                                         
*                                                                               
DMXRET   DS    0H                                                               
         B     DMXIT               EXIT THE PROGRAM                             
*                                                                               
         USING PAPPRD,R4           PRINT LINE DSECT                             
DMXINIT  LA    R4,P                                                             
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P        INITIALIZE PRINT LINE                        
         MVC   P(PHEADQ),PDASH     PRINT DASHES FOR HEADERS                     
         GOTO1 VPRINTER                                                         
         MVC   P(PHEADQ),PHEAD     PRINT HEADER LINE                            
         GOTO1 VPRINTER                                                         
         MVC   P(PHEADQ),PDASH     PRINT DASHES FOR HEADERS                     
         GOTO1 VPRINTER                                                         
*                                                                               
DMXIT    XIT1  ,                                                                
         EJECT                                                                  
         USING APPRECD,R2          READ APPROVER RECORDS                        
DMXREC   L     R2,VREC                                                          
         GOTO1 VRECTYP,DMCB,(C'D',APPRECD)                                      
         MVC   RECTYPE,0(R1)                                                    
*        MVC   COMPCOD,1(R1)       GET COMPANY-CODE                             
*        CLI   COMPCOD,X'41'                                                    
*        BNE   DMXRTST             NO:EXIT THE PROGRAM                          
         CLI   RECTYPE,ACRTAPPR    APPROVER RECORD?                             
         BNE   DMXRTST             NO:EXIT THE PROGRAM                          
*                                                                               
DMXREC10 LA    R3,APPRFST          R3=A(1ST ELEMENT IN APPR RECORD)             
         USING LIDELD,R3           LIST DATA ELEMENT                            
         MVI   BYTE,0                                                           
*                                                                               
DMXREC20 CLI   LIDEL,0             E-O-R?                                       
         BE    DMXREC70            YES:CHECK FOR BYTE & KEEP DATA               
         CLI   LIDEL,LIDELQ        IS LIST DATA ELEMENT?                        
         BE    DMXREC40                                                         
*                                                                               
DMXREC30 LLC   R0,LIDLN            GET TO NEXT ELEMENT                          
         AR    R3,R0                                                            
         B     DMXREC20                                                         
*                                                                               
DMXREC40 CLI   LIDTYPE,LIDTBACK    BACK UP APPROVER LIST?                       
         BNE   DMXREC30            NO:CHECK NEXT ELEMENT                        
*                                                                               
         LLC   RF,LIDLN                                                         
         CHI   RF,LIDLNDQ          CHECK ELEMENT IS LONG ENOUGH                 
         BNH   DMXREC30            CHECK NEXT ELEMENT                           
         LA    R6,LIDDATA                                                       
         LR    R5,R3                                                            
         AR    R5,RF               R5=A(END OF LIDELD ELEMENT)                  
*                                                                               
SUB      USING LIDDATA,R6                                                       
DMXREC50 TM    SUB.LIDLAPPL,LIDLTIME TIMESHEETS?                                
         BZ    DMXREC60            NO:CHECK NEXT SUB-ELEMENT                    
*                                                                               
         OI    SUB.LIDLAPP2,LIDLTOFF SET AS TIME-OFF APPROVERS                  
         OI    BYTE,X'80'          CHECK FOR BYTE WHILE KEEPING UPD REC         
*                                                                               
         LA    R4,P                                                             
         AP    CHANGES,=P'1'       INCREMENT UPDATED LIDELD COUNETR             
         GOTO1 VHEXOUT,DMCB,APPKCPY,PCMPCD,L'APPKCPY COMP CODE                  
         GOTO1 VHEXOUT,DMCB,LIDLPID,PAPPRPID,L'LIDLPID  APPROVER PID            
         MVC   P(PAPPRLQ),PAPPRD   PRINT DETAIL LINES                           
         GOTO1 VPRINTER                                                         
*                                                                               
DMXREC60 LHI   RF,LIDLLN6Q         PID - FOR LIDTBACK                           
         AR    R6,RF                                                            
         CR    R6,R5                                                            
         BL    DMXREC50                                                         
         B     DMXREC30                                                         
*                                                                               
DMXREC70 TM    BYTE,X'80'          IS RECORD UPDATED?                           
         BZ    DMXRTST             NO:EXIT THE PROGRAM                          
*                                                                               
         B     DMXKEEP             YES: KEEP UPDATED EXPENSE RECORD             
         DROP  R2,R3,R4,SUB                                                     
*                                                                               
         LTORG                                                                  
VRECTYP  DC    V(ACRECTYP)                                                      
VBINSRCH DC    V(BINSRCH)                                                       
VSORTER  DC    V(SORTER)                                                        
VHELLO   DC    V(HELLO)                                                         
VLDDEF   DC    V(LDDEFN)                                                        
CUREDIT  DC    V(CUREDIT)                                                       
VHEXOUT  DC    V(HEXOUT)                                                        
ACCMST   DC    C'ACCMST  '                                                      
ACCDIR   DC    C'ACCDIR  '                                                      
PDASH    DC    (PHEADQ)C'-'                                                     
CHANGES  DC    PL6'0'              COUNTER FOR UPDATED RECORDS                  
*                                                                               
PHEAD    DS    0C                                                               
         DC    C'|'                                                             
         DC    C'COMPANY-CODE'     COMPANY CODE                                 
         DC    C'|'                                                             
         DC    C'APPROVER-PID'     UNIQUE CONTROL BINARY PERSON PID             
         DC    C'|'                                                             
PHEADQ   EQU   *-PHEAD                                                          
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        DSECTS                    WORKING-STORAGE                    *         
***********************************************************************         
WORKD    DSECT                                                                  
APARM    DS    A                                                                
COMPCOD  DS    X                   COMPANY-CODE                                 
DUB      DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    XL64                                                             
BYTE     DS    XL1                                                              
*                                                                               
         DS    0D                                                               
PLIST    DS    0X                                                               
VREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
VCARDS   DS    V                                                                
VPEELDT  DS    A                                                                
VISREC   DS    A                                                                
PLISTL   EQU   *-PLIST                                                          
*                                                                               
RECTYPE  DS    X                                                                
WORKX    EQU   *                                                                
*                                                                               
***********************************************************************         
*        DSECTS                    DETAIL LINE                        *         
***********************************************************************         
PAPPRD   DSECT ,                                                                
         DS    CL5                 FILLER                                       
PCMPCD   DS    CL2                 COMPANY CODE                                 
         DS    CL11                FILLER                                       
PAPPRPID DS    CL4                 APPROVER BINARY PID                          
         DS    CL5                 FILLER                                       
PAPPRLQ  EQU   *-PAPPRD                                                         
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE ACOPTEQUS                                                      
         PRINT ON                                                               
         SPACE                                                                  
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENRAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENRAC                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACLDXAPR  08/21/19'                                      
         END                                                                    
