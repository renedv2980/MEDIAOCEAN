*          DATA SET TAREP0F    AT LEVEL 049 AS OF 07/17/14                      
*PHASE T7030FC,*                                                                
*                                                                               
         TITLE 'T7030F - PROD INT REPORT- SCREEN VALIDATION'                    
T7030F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7030F                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING T703FFD,RA                                                       
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING TPIND,R7            PRODUCTION INTERFACE DSECT                   
         EJECT                                                                  
*                                                                               
         GOTO1 INITIAL,DMCB,0                                                   
         CLI   MODE,VALKEY                                                      
         BNE   XIT                                                              
         MVC   PNUSER,TGUSER                                                    
         LA    R2,SPNAGYH          CHECK IF REQUESTING A SPECIFIC AGY           
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         XC    PROCAGY,PROCAGY     CLEAR                                        
         CLC   8(3,R2),=C'ALL'                                                  
         BE    VREC30                                                           
         GOTO1 USERVAL,DMCB,(X'40',0(R2)),SPNAGYNH                              
         MVC   PROCAGY,TGUSER                                                   
*                                                                               
         LA    R2,SPNSEQH          SPECIFIC SEQUENCE NUMBER?                    
         CLI   5(R2),0                                                          
         BE    VREC30                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         MVC   WORK(4),=4X'F0'     INSURE VALID NUMERIC                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),8(R2)                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),=4X'F0'                                                  
         BNE   INVERR                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)         RETURN AMOUNT IN SEQNO                       
         CVB   R1,DUB                                                           
         STH   R1,SEQNO                                                         
*                                                                               
VREC30   LA    R2,SPNEMUH          VALIDATE EMULATED FILES                      
         MVI   CNTREAD,C'N'        SET CONTROL FILE NOT READ                    
         LA    R4,EMUTAB           POINT TO EMULATION TABLE                     
         XC    EMUTAB(EMULNQ),EMUTAB     & CLEAR IT                             
         CLI   5(R2),0                                                          
         BE    VREC50                                                           
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
*                                                                               
VREC40   CLC   =C'ACC',SCDATA1     INPUT WILL BE ACCX                           
         BNE   INVERR                                                           
         MVC   BYTE,SCDATA1+3                                                   
         BAS   RE,VALACC           VALIDATE THE ACC FILE                        
         LA    R4,1(R4)            BUMP EMULATION TABLE                         
         LA    R3,SCANNEXT         BUMP TO IT                                   
         BCT   R0,VREC40           AND CONTINUE                                 
*                                                                               
VREC50   MVI   0(R4),X'FF'         MARK END OF TABLE                            
         LA    R2,SPNOPTH          CHECK OPTIONS                                
         CLI   5(R2),0                                                          
         BE    VRECX                                                            
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
*                                                                               
VREC60   CLC   =C'TRACE',SCDATA1  TRACE                                         
         BNE   INVERR                                                           
         CLI   SCDATA2,C'Y'                                                     
         BE    VREC70                                                           
         CLI   SCDATA2,C'N'                                                     
         BE    VOPTNEXT                                                         
         B     INVERR                                                           
VREC70   OI    PNOPTION,PNTRACE    SET TRACE ON                                 
         B     VOPTNEXT                                                         
*                                                                               
VOPTNEXT LA    R3,SCANNEXT         BUMP TO IT                                   
         BCT   R0,VREC60           AND CONTINUE                                 
*                                                                               
VRECX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        VALIDATE ACC FILE & GET SE NUMBER                                      
*        BYTE = ACC SYSTEM                                                      
*        R4   = NEXT ENTRY IN EMUTAB                                            
*                                                                               
VALACC   NTR1                                                                   
         CLI   CNTREAD,C'Y'        IF CONTROL FILE WAS READ                     
         BE    VACC05              JUST VALIDATE ACC FILE                       
         MVC   FILENAME,=CL8'CTFILE' SET TO READ CONTROL FILE                   
         MVI   USEIO,C'Y'          ELSE READ IT                                 
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING CTWREC,R3                                                        
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'S'                                                     
         GOTO1 HIGH                READ RECORD ON CONTROL FILE                  
         XC    FILENAME,FILENAME                                                
         MVI   USEIO,C'N'                                                       
         CLC   CTWKEY,KEYSAVE      DID WE GET THE RECORD                        
         BE    *+6                                                              
         DC    H'0'                IF NOT - DIE                                 
         MVI   CNTREAD,C'Y'                                                     
*                                                                               
VACC05   L     R3,AIO                                                           
         LA    R1,CTWDATA                                                       
*                                                                               
VACC10   CLI   0(R1),X'A4'         SYSTEM LIST ELEMENT                          
         BE    VACC30                                                           
         CLI   0(R1),0             END OF RECORD                                
         BNE   *+6                 NO SE NUMBER ?                               
         DC    H'0'                                                             
*                                                                               
VACC20   ZIC   R2,1(R1)                                                         
         AR    R1,R2                                                            
         B     VACC10                                                           
*                                                                               
         USING SYSELD,R1                                                        
VACC30   LA    RE,SYSNAME+L'SYSNAME-1                                           
         CLI   0(RE),C' '          LOCATE LAST CHARACTER OF NAME                
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLC   BYTE,0(RE)          MATCH LOGICAL SYSTEM NUMBER                  
         BNE   VACC20              GET ANOTHER                                  
         MVC   0(1,R4),SYSSEN      SET SE NUMBER                                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERXIT                                                            
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERXIT                                                            
*                                                                               
ERXIT    DS    0H                                                               
         GOTO1 ERREX                                                            
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE TAPIND                                                         
         EJECT                                                                  
*                                                                               
*        OTHER DSECTS ARE HIDDEN IN HERE                                        
*                                                                               
         SPACE 1                                                                
*TAREPFFD                                                                       
*TAREPF0D                                                                       
*DDGENTWA                                                                       
*DDTWADCOND                                                                     
*DDSYSELD                                                                       
*DDMASTD                                                                        
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*CTGENFILE                                                                      
*ACGENBOTH                                                                      
*ACGENPOST                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAREPFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPF0D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DDSYSELD                                                       
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049TAREP0F   07/17/14'                                      
         END                                                                    
