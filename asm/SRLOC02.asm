*          DATA SET SRLOC02    AT LEVEL 003 AS OF 05/01/02                      
*PHASE T15C02A                                                                  
         PRINT NOGEN                                                            
         TITLE '$LOCK - CONTROL SYSTEM LOCKOUTS '                               
LOCKCON  CSECT                                                                  
         NMOD1 000,**$LK2**,R9,CLEAR=YES,RR=RE                                  
         LR    RC,R1                                                            
         USING WRKD,RC             RC=A(W/S)                                    
         ST    RE,RELO                                                          
         L     R6,UPDENTRY                                                      
         USING UPDTABD,R6                                                       
         L     R7,ASYSFACS                                                      
         USING SYSFACD,R7                                                       
*                                                                               
         CLI   CALL,C'L'           SET LOCK KEY                                 
         BE    CLOCK000                                                         
         CLI   CALL,C'D'           DISPLAY LOCK KEY                             
         BE    CDISP000                                                         
         DC    H'0'                UNKNOWN CALL                                 
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*************************************************************                   
*  THE FOLLOWING ROUTINES RETURN THE FOLLOWING:-            *                   
*  UPDSENUM,UPDRULE,UPDKEY                                  *                   
*                                                           *                   
*  ASCANBLK = CURRENT POS'N IN LOCK=CARD                    *                   
*  REQUEST  = REPORTS REQUEST CARD                          *                   
*************************************************************                   
         SPACE 1                                                                
CLOCK000 L     RF,ASCANBLK                                                      
         LA    R2,RULETAB          SEARCH FOR LOCK RULE                         
         SR    R1,R1                                                            
         IC    R1,0(RF)                                                         
         BCTR  R1,0                                                             
CLOCK010 EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,RF),0(R2)      TEST KEYWORD                                 
         BE    CLOCK020                                                         
         LA    R2,L'RULETAB(,R2)                                                
         CLI   0(R2),0                                                          
         BNE   CLOCK010                                                         
         DC    H'0'                INVALID RULE                                 
*                                                                               
CLOCK020 MVC   UPDRULE,10(R2)      SAVE RULE NUMBER                             
         MVC   UPDSENUM,LKSYS      CONTROL SESYS=OVSYS                          
         LR    R1,RF               PASS SCANNER ENTRY TO RULE                   
         L     RF,12(R2)                                                        
         A     RF,RELO                                                          
         BASR  RE,RF               PROCESS RULE                                 
CLOCKX   B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*  THE FOLLOWING ROUTINES BUILD A 40 BYTE MESSAGE IN MSG    *                   
*  R6 = UPDTAB ENTRY                                        *                   
*************************************************************                   
         SPACE 1                                                                
CDISP000 LA    R2,RULETAB          SEARCH FOR LOCK RULE                         
         XC    EXT,EXT                                                          
CDISP010 CLC   UPDRULE,10(R2)      TEST RULE                                    
         BE    CDISP020                                                         
         LA    R2,L'RULETAB(,R2)                                                
         CLI   0(R2),0                                                          
         BNE   CDISP010                                                         
         DC    H'0'                INVALID RULE                                 
*                                                                               
CDISP020 L     RF,16(,R2)                                                       
         A     RF,RELO                                                          
         BR    RF                                                               
*************************************************************                   
*                 LOCK RULES                                *                   
*************************************************************                   
         SPACE 1                                                                
AGYRUL   ST    RE,SAVERE           TOTAL AGENCY LOCKOUT                         
*                                                                               
         CLI   1(R1),4                                                          
         BNE   AGYR010                                                          
         CLC   22(4,R1),=C'NONE'                                                
         BE    AGYR100                                                          
         B     CARDERR                                                          
*                                                                               
AGYR010  CLI   1(R1),2             TEST AGENCY CODE                             
         BNE   CARDERR                                                          
         MVC   UPDKEY+1(2),22(R1)                                               
*                                                                               
AGYR100  LA    R1,32(R1)           NEXT SCANNER ENTRY                           
         CLI   0(R1),0                                                          
         BE    RULEXX              IF ANY                                       
         CLI   0(R1),5                                                          
         BNE   CARDERR                                                          
         CLC   12(5,R1),=C'RTYPE'  MUST BE RTYPE                                
         BNE   CARDERR                                                          
         SR    RF,RF                                                            
         ICM   RF,1,1(R1)                                                       
         BZ    CARDERR                                                          
         BCTR  RF,0                                                             
         LA    RE,TYPETAB          MATCH RECORD TYPE TO TABLE                   
AGYR110  EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),22(R1)                                                   
         BE    AGYR120                                                          
         LA    RE,L'TYPETAB(RE)                                                 
         CLI   0(RE),0                                                          
         BNE   AGYR110                                                          
         B     CARDERR                                                          
*                                                                               
AGYR120  MVC   UPDKEY(1),8(RE)     SAVE RTYPE                                   
*                                                                               
RULEXX   L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
CARDERR  DC    H'0'                INVALID LOCK= CARD                           
         EJECT                                                                  
*************************************************************                   
*       DISPLAY RULES                                       *                   
*************************************************************                   
         SPACE 1                                                                
AGYDIS   TM    DDSFLAG,DDSTRM                                                   
         BNO   AGYD100                                                          
AGYD100  MVC   MSG(4),=C'AGY='     TEST AGY=NONE                                
         LA    R1,MSG+4                                                         
         OC    UPDKEY+1(2),UPDKEY+1                                             
         BNZ   AGYD110                                                          
         MVC   0(4,R1),=C'NONE'                                                 
         LA    R1,4(R1)                                                         
         B     AGYD200                                                          
*                                                                               
AGYD110  MVC   0(2,R1),UPDKEY+1    OR AGY=XX                                    
         LA    R1,2(R1)                                                         
*                                                                               
AGYD200  LA    RE,TYPETAB          FIND RTYPE                                   
*                                                                               
AGYD210  CLC   8(1,RE),UPDKEY                                                   
         BE    AGYD220                                                          
         LA    RE,L'TYPETAB(RE)                                                 
         CLI   0(RE),0                                                          
         BNE   AGYD210                                                          
         B     EXIT                                                             
*                                                                               
AGYD220  MVC   0(7,R1),=C',RTYPE=' DISPLAY RTYPE                                
         MVC   7(8,R1),0(RE)                                                    
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*       MESSAGE NUMBERS FOR RULE DESCRIPTION                *                   
*************************************************************                   
         SPACE 1                                                                
*        PUT EQUATES HERE                                                       
         EJECT                                                                  
*************************************************************                   
*                       CONSTANTS                           *                   
*************************************************************                   
*                                                                               
*        CL8'KEYWORD',X'0000',H'RULENUM',A(ROUTINE)                             
*                                                                               
RULETAB  DS    0CL20                                                            
         DC    CL8'AGY     ',X'0000',H'0001',AL4(AGYRUL),AL4(AGYDIS)            
         DC    X'0000'                                                          
*                                                                               
*        CL8'KEYWORD',RECORD-TYPE,FLAGS                                         
*                                                                               
TYPETAB  DS    0CL10                                                            
         DC    CL8'EXCHANGE',C'E',X'00'                                         
         DC    X'0000'                                                          
*                                                                               
DC@DDS   DC    CL8'DDS'                                                         
DC@USR   DC    CL8'U'                                                           
*                                                                               
BUFFER   DC    CL8'BUFFER'                                                      
GFILE    DC    CL8'GFILE'                                                       
RANDOM   DC    CL8'RANDOM'                                                      
PRTQUE   DC    CL8'PRTQUE'                                                      
READ     DC    CL8'READ'                                                        
ACTI     DC    CL8'ACTI'                                                        
PURGE    DC    CL8'PURGE'                                                       
INDEX    DC    CL8'INDEX'                                                       
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
GLIST    DC    CL8'GLIST'                                                       
CTFILE   DC    CL8'CTFILE'                                                      
SPACES   DC    CL32' '                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SRLOCWRK                                                       
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SRLOC02   05/01/02'                                      
         END                                                                    
