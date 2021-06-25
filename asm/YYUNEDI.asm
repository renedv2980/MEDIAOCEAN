*          DATA SET YYUNEDI    AT LEVEL 059 AS OF 08/16/00                      
*PHASE YYUNEDIA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
         TITLE 'EASYLINK RECORD REPORT'                                         
YYUNEDI  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,YYUNEDI,=V(REGSAVE),R7                                         
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
*                                                                               
******** GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
*                                                                               
         OPEN  (FILE,OUTPUT)                                                    
*                                                                               
         MVC   P+2(7),=C'USER ID'                                               
         MVC   P+20(11),=C'ADV MAILBOX'                                         
         MVC   P+40(11),=C'REP MAILBOX'                                         
         MVC   P+60(9),=C'ACCOUNT #'                                            
         PUT   FILE,P+2                                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P+2(7),=C'-------'                                               
         MVC   P+20(11),=C'-----------'                                         
         MVC   P+40(11),=C'-----------'                                         
         MVC   P+60(9),=C'---------'                                            
         PUT   FILE,P+2                                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NCTFILE X',IO,0                                               
*                                                                               
         USING EDIKEYD,R2                                                       
         LA    R2,KEY                                                           
         XC    KEY,KEY             GET THE FIRST EDIFILE REC                    
         MVI   EDIKSYS,EDIKSYSQ                                                 
         MVI   EDITYPE,EDITYPEQ                                                 
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IO                
*                                                                               
         LA    R2,IO                                                            
RCT10    CLI   EDITYPE,EDITYPEQ    STILL READING EDIFILE REC?                   
         BNE   RCTX                NO - DONE                                    
*                                                                               
         MVC   P+2(L'EDINAME),EDINAME     USER ID                               
*                                                                               
         LR    R6,R2                                                            
         MVI   ELCODE,EDILNKEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING EDILNKD,R6                                                       
*                                                                               
         CLI   EDIMETHS,EDIEASYQ   EASYLINK?                                    
         BNE   RCT20                                                            
*                                                                               
         OC    EDIADVNO,EDIADVNO                                                
         BZ    *+14                                                             
         CLC   EDIREPNO,=10C' '                                                 
         BNE   RCT15                                                            
         OC    EDIREPNO,EDIREPNO                                                
         BZ    RCT12               DON'T HAVE BOTH MAILBOXES                    
         CLC   EDIREPNO,=10C' '                                                 
         BNE   RCT15                                                            
*                                                                               
RCT12    MVC   P+21(L'EDIADVNO),=10C'*'     ADV MAILBOX                         
         MVC   P+41(L'EDIREPNO),=10C'*'     REP MAILBOX                         
         MVC   P+60(L'EDIEACC),EDIEACC      ACC #                               
         B     RCT18                                                            
*                                                                               
RCT15    MVC   P+21(L'EDIADVNO),EDIADVNO    ADV MAILBOX                         
         MVC   P+41(L'EDIREPNO),EDIREPNO    REP MAILBOX                         
         MVC   P+60(L'EDIEACC),EDIEACC      ACC #                               
         DROP  R6                                                               
*                                                                               
RCT18    OC    P,=132C' '                                                       
         PUT   FILE,P+2                                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
RCT20    GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRSEQ'),=C'CTFILE',KEY,IO                
         B     RCT10                                                            
*                                                                               
RCTX     CLOSE FILE                                                             
         XBASE                                                                  
         LTORG                                                                  
*                                                                               
***********************************************************************         
         DS    0D                                                               
*FASSBOFF                                                                       
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         SPACE 3                                                                
UTL      DC    F'0',X'0A'                                                       
***********************************************************************         
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(YYUNEDI)                                                       
         DC    X'80'                                                            
         DC    VL3(DUMMY)                                                       
*                                                                               
ELCODE   DS    X                                                                
DATADISP DC    Y(EDIELDQ)                                                       
DMCB     DS    6F                                                               
KEY      DS    CL25                KEY INTO CTFILE                              
COUNTER  DS    F                                                                
LSTEND   DS    A                                                                
FILE     DCB   DDNAME=OUTFILE,DSORG=PS,MACRF=PM                                 
***********************************************************************         
IO       DS    2000C                                                            
***********************************************************************         
*DDDPRINT                                                                       
*CTGENEDICT                                                                     
*                                                                               
*        PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENEDICT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'059YYUNEDI   08/16/00'                                      
         END                                                                    
