*          DATA SET ACMAC02    AT LEVEL 044 AS OF 03/09/92                      
*PHASE T61102A,*                                                                
*                                                                               
         TITLE 'T61102 - MULTIPLE A/C LOCK '                                    
T61102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 000,*MAC02**                                                     
         L     RA,0(R1)                    A(TWA)                               
         L     RC,4(R1)                    A(W/S)                               
         L     R8,8(R1)                    A(RECORD)                            
         USING MACWRKD,RC                                                       
         USING T611FFD,RA                                                       
         USING ACKEYD,R8                                                        
         CLI   MODE,EOF                                                         
         BE    BLDEND                                                           
         ZIC   R1,MODE                                                          
         SLL   R1,2                                                             
         B     *+4(R1)                                                          
         B     OKEXIT                      NO MODE 0                            
         B     BILDKEY                     NEW SCREEN                           
         B     MARKIT                      SCREEN AVAIL. FOR MARKING            
         B     BLDLINE                     RECORD FOR DISPLAY                   
OKEXIT   CR    RB,RB                                                            
         B     EXIT                                                             
ERREXIT  LTR   RB,RB                                                            
         B     EXIT                                                             
FASTEXIT L     RD,SAVED                                                         
EXIT     XMOD1                                                                  
         EJECT                                                                  
*              BUILD KEY FOR FIRST READ THIS PASS                               
BILDKEY  MVC   KEY,SPACES                                                       
         LA    R8,KEY                                                           
         MVI   IOMODE,SKSEQ1                                                    
         TWAXC MALSTA1H,MALTABH,PROT=Y                                          
         SR    RE,RE                                                            
         LA    R7,MALSTA1H                                                      
         LA    RF,MALTABH-1                                                     
         OI    1(R7),X'20'                                                      
         NI    6(R7),X'FE'                                                      
         IC    RE,0(R7)                                                         
         BXLE  R7,RE,*-12                                                       
         SR    R0,R0                                                            
         LR    R3,RA                                                            
         CLI   0(R3),0                                                          
         BE    *+14                                                             
         IC    R0,0(R3)                                                         
         AR    R3,R0                                                            
         B     *-14                                                             
         MVI   1(R3),1                     SPECIAL BITS FOR FAKPAK              
         MVI   2(R3),1                                                          
         CLI   PASS,0                                                           
         BNE   BILD100                                                          
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),WUNIT                                                   
         CLC   ACCFILT,SPACES              ANY START VALUE ?                    
         BE    *+14                        NO, USE FILTERS IF ANY               
         MVC   KEY+3(12),ACCFILT                                                
         B     BILD50                                                           
         LA    RF,KEY+3                    RF = START OF ACCOUNT                
         ZIC   RE,SLVLALEN                                                      
         BCTR  RE,0                                                             
         C     RE,=F'0'                                                         
         BL    BILD05                                                           
         LA    R1,CLIENT                   CLIENT HAS LEV A FILTER              
         EX    RE,KEYMOVE                                                       
*                                                                               
         LA    RF,1(RE,RF)                                                      
         IC    RE,SLVLBLEN                                                      
         BCTR  RE,0                                                             
         C     RE,=F'0'                                                         
         BL    BILD05                                                           
         LA    R1,PRODUCT                  PRODUCT "    " B    "                
         EX    RE,KEYMOVE                                                       
*                                                                               
         LA    RF,1(RE,RF)                                                      
         IC    RE,SLVLCLEN                                                      
         BCTR  RE,0                                                             
         C     RE,=F'0'                                                         
         BL    BILD05                                                           
         LA    R1,JOB                      JOB     "    " C    "                
         EX    RE,KEYMOVE                                                       
*                                                                               
         LA    RF,1(RE,RF)                                                      
         IC    RE,SLVLDLEN                                                      
         BCTR  RE,0                                                             
         C     RE,=F'0'                                                         
         BL    BILD05                                                           
         LA    R1,LEVEL4                   LEVEL4  "    " D    "                
         EX    RE,KEYMOVE                                                       
*                                                                               
BILD05   CLC   KEY+3(12),SPACES            IF NO FILTERS                        
         BE    BILD50                      THEN NO STOP POINT                   
         MVC   ENDKEY(15),KEY                                                   
         CLI   ENDKEY+14,C' '                                                   
         BNE   BILD50                                                           
         LA    RE,ENDKEY+13                                                     
         LA    RF,11                                                            
BILD10   CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         MVI   1(RE),X'FF'                                                      
         B     BILD50                                                           
         BCTR  RE,0                                                             
         BCT   RF,BILD10                                                        
         DC    H'0'                        NO KEY THERE AT ALL                  
BILD50   MVI   LASTLYN,0                                                        
         CLC   KEY+3(12),SPACES                                                 
         BNE   OKEXIT                                                           
         ZIC   R1,SKEYEND                  FORCE READ HIGH TO                   
         LA    R1,KEY(R1)                  SKIP OVER LEDGER RECORD              
         MVI   0(R1),X'41'                                                      
         B     OKEXIT                                                           
BILD100  MVC   ACKEYACC,LASTACCT           CONTINUATION SCREEN                  
         MVI   LASTLYN,0                                                        
         B     OKEXIT                                                           
KEYMOVE  MVC   0(0,RF),0(R1)               EXECUTED INSTRUCTION                 
         EJECT                                                                  
BLDLINE  ICM   R7,15,ADACBAL               ONLY NEED LOW-LEVEL ACCTS.           
         BZ    OKEXIT                                                           
         MVI   IOMODE,SKSEQ1               RE-SET READ MODE                     
         MVC   LASTACCT,ACKEYACC           SAVE FOR NEXT PASS                   
         MVC   MACHED1,LOCHED1             MOVE HEADER FELDS                    
         MVC   MACHED2,LOCHED2             TO SCREEN                            
         OI    MACHED1H+6,X'80'                                                 
         OI    MACHED2H+6,X'80'                                                 
         TM    ACSTATUS,X'80'                                                   
         BO    OKEXIT                      NO DELETED ACCOUNTS                  
         CLI   KEY,X'FE'                   I/O COUNT EXCEEDED                   
         BE    BLDEND                                                           
         USING LOCLYND,R7                                                       
         ZIC   R7,LASTLYN                                                       
         LA    R6,LOCNLNQ                                                       
         MR    R6,R6                                                            
         LA    R7,MALSTA1H(R7)                                                  
         NI    LOCNSTAH+1,X'DF'            UNPROTECT MARK FIELD                 
         OI    LOCNSTAH+6,X'80'            TRANSMIT                             
*                                                                               
         USING ACNAMED,R6                                                       
BLDNAM   ICM   R6,15,ADACNAM                                                    
         BZ    BLDSTAT                                                          
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     BLDSTAT                                                          
         MVC   LOCNNAME(0),2(R6)                                                
*                                                                               
         USING ACSTATD,R6                                                       
BLDSTAT  ICM   R6,15,ADACSTAT                                                   
         BZ    BLDSTAT4                                                         
         GOTO1 DATCON,DMCB,(1,ACSTLAST),(8,LOCNACT)                             
         CLI   ACTION,C'U'                                                      
         BE    BLDSTAT2                                                         
         TM    ACSTSTAT,X'20'              LOCKED ALREADY?                      
         BNO   BLDSTAT4                                                         
         MVC   LOCNWHY(6),=C'LOCKED'                                            
         OI    LOCNSTAH+1,X'20'            YES, LOCK THE LINE                   
         B     BLDSTAT4                                                         
BLDSTAT2 TM    ACSTSTAT,X'20'                                                   
         BO    BLDSTAT4                                                         
         MVC   LOCNWHY(4),=C'OPEN'         CAN'T REOPEN IF NOT LOCKED           
         OI    LOCNSTAH+1,X'20'                                                 
*                                                                               
         USING ASTELD,R6                                                        
BLDSTAT4 ICM   R6,15,ADACAST                                                    
         BZ    BLDCLOS                                                          
         CLI   ACTION,C'U'                                                      
         BE    BLDCLOS                                                          
         TM    LOCNSTAH+1,X'20'                                                 
         BO    BLDCLOS                                                          
         OC    ASTDRAFT,ASTDRAFT                                                
         BZ    BLDCLOS                                                          
         OI    LOCNSTAH+1,X'20'                                                 
         MVC   LOCNWHY(7),=C'DFT ITM'                                           
*                                                                               
         USING ACJOBD,R6                                                        
BLDCLOS  ICM   R6,15,ADACJOB                                                    
         BZ    BLDCODE                                                          
         GOTO1 DATCON,DMCB,(1,ACJBCLOS),(8,LOCNCLOS)                            
*                                                                               
BLDCODE  MVC   LOCNACC,ACKEYACC+3          ACCOUNT CODE                         
         ZIC   R7,LASTLYN                                                       
         LA    R7,1(R7)                                                         
         STC   R7,LASTLYN                  BUMP LINE #                          
         CLI   LASTLYN,LOCMAX              BOTTOM OF THE SCREEN?                
         BNE   OKEXIT                                                           
BLDEND   ZIC   R7,PASS                     YES, BUMP PASS #                     
         LA    R7,1(R7)                                                         
         STC   R7,PASS                                                          
         LA    RF,MALTABH                                                       
         LA    R7,MALSTA1H                                                      
BLDEND2  TM    1(R7),X'20'                 AND POSITION CURSOR TO               
         BNZ   BLDEND4                     FIRST UNPROT FIELD                   
         TM    6(R7),X'20'                                                      
         BO    BLDEND4                                                          
         CLI   8(R7),0                                                          
         BE    BLDEND6                                                          
BLDEND4  ZIC   RE,0(R7)                                                         
         BXLE  R7,RE,BLDEND2                                                    
BLDEND6  OI    6(R7),X'40'                                                      
         OI    MACACTH+6,X'81'             RE-TRANSMIT AND MODIFY               
         MVI   IOMODE,FINISHED             ACTION FIELD                         
         SR    R0,R0                                                            
         LR    R3,RA                                                            
         CLI   0(R3),0                     FIND BOTTOM OF THE TWA               
         BE    *+14                                                             
         IC    R0,0(R3)                                                         
         AR    R3,R0                                                            
         B     *-14                                                             
         MVI   1(R3),1                     AND PUT IN SPECIAL BITS              
         MVI   2(R3),1                                                          
         B     OKEXIT                                                           
         EJECT                                                                  
*              LOCK MARKED ACCOUNTS                                             
MARKIT   LA    R7,MALSTA1H                 A(FIRST SCREEN LINE)                 
         LA    R0,LOCMAX                   MAX LINES                            
MARK005  CLI   LOCNSTA,0                                                        
         BE    MARK100                     NO MARK                              
         CLI   LOCNSTA,C'N'                                                     
         BE    MARK100                     DON'T MARK                           
         CLI   LOCNSTA,C'Y'                                                     
         BE    MARK010                     MARK                                 
         LA    RF,LOCNSTAH                 IF NONE OF THESE                     
         ST    RF,FADR                     THEN IT'S NOT ALLOWED                
         MVI   FERN,INVALID                                                     
         MVI   FNDX,0                                                           
         B     ERREXIT                                                          
MARK010  MVI   IOMODE,READ2                READ ACCOUNT INTO IO2                
         LA    R8,IO2                                                           
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY              BUILD KEY FOR READ                   
         MVC   KEY+1(2),WUNIT                                                   
         MVC   KEY+3(12),LOCNACC                                                
         GOTO1 READ                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   KEY,X'FE'                   MAX COUNT EXCEEDED                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R6,ACRECORD                                                      
MARK020  CLI   0(R6),X'30'                 STATUS ELEMENT                       
         BE    MARK030                                                          
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     MARK020                                                          
         USING ACSTATD,R6                                                       
MARK030  XI    ACSTSTAT,X'20'              FLIP THE BIT OVER                    
         MVI   IOMODE,WRITE2               AND RWITE THE ACCOUNT BACK           
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   KEY,X'FE'                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
MARK100  LA    R7,LOCNLNQ(R7)              NEXT LINE                            
         BCT   R0,MARK005                                                       
         B     OKEXIT                                                           
LOCHED1  DC    CL16'M    ACCOUNT',16C'-',C'NAME',16C'-'                         
         DC    C' ACTIVITY CLOSING  ACCOUNT'                                    
LOCHED2  DC    C'K  ',12C'-',40C' ',CL9'DATE',CL7'DATE',C'STATUS  '             
LOCLYND  DSECT                                                                  
LOCNSTAH DS    CL8                                                              
LOCNSTA  DS    C                           MARK                                 
LOCNACCH DS    CL8                                                              
         DS    0CL75                                                            
LOCNACC  DS    CL12                        ACCOUNT KEY EX. CUL                  
         DS    C                                                                
LOCNNAME DS    CL36                        ACCOUNT NAME                         
         DS    C                                                                
LOCNACT  DS    CL9                         ACTIVITY DATE                        
LOCNCLOS DS    CL9                                                              
LOCNWHY  DS    CL7                         REASON NOT LOCKABLE                  
LOCNLNQ  EQU   *-LOCLYND                                                        
         SPACE 2                                                                
       ++INCLUDE ACMACWRK                                                       
       ++INCLUDE ACMACEQU                                                       
         PRINT  OFF                                                             
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDACCFACS                                                      
       ++INCLUDE FATWA                                                          
       ++INCLUDE FAFACTS                                                        
         PRINT  ON                                                              
LOCMAX   EQU   17                          MAX. ON SCREEN                       
CLOSMAX  EQU   17                          MAX. ON SCREEN                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044ACMAC02   03/09/92'                                      
         END                                                                    
