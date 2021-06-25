*          DATA SET YYUNUSS    AT LEVEL 001 AS OF 03/20/01                      
*PHASE YYUNUSSA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*                                                                               
YYUNUSS  CSECT                                                                  
         TITLE ' DO I/O TO USS HFS  '                                           
*                                                                               
         PRINT NOGEN                                                            
         NBASE 0,*YYUNUSS*,=V(REGSAVE)                                          
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         MVC   CONFILE(60),=CL60' '                                             
         MVC   EMAILADR(55),=CL55'"UNKNOWN"'                                    
         MVC   FILENAME(62),=CL62'"UNKNOWN"'                                    
         MVC   SUBJECT(71),=CL71'"UNKNOWN"'                                     
*                                                                               
RC10     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BE    RCX                                                              
*                                                                               
         CLC   =C'CONFILE=',CARD                                                
         BNE   RC20                                                             
         MVC   CONFILE(60),CARD+8                                               
         B     RC10                                                             
*                                                                               
RC20     CLC   =C'EMAIL=',CARD                                                  
         BNE   RC30                                                             
         MVC   EMAILADR(55),CARD+6                                              
         B     RC10                                                             
*                                                                               
RC30     CLC   =C'FILE=',CARD                                                   
         BNE   RC40                                                             
         MVC   FILENAME(62),CARD+5                                              
         B     RC10                                                             
*                                                                               
RC40     CLC   =C'SUB=',CARD                                                    
         BNE   RC10                                                             
         MVC   SUBJECT(71),CARD+4                                               
         B     RC10                                                             
*                                                                               
RCX      DS    0H                                                               
*                                                                               
*                                                                               
** ROUTINE TO OPEN A UNIX SYSTEM SERVICES HFS FILE                              
**       MVI   O_FLAGS4,O_RDWR           EN FOR READ AND WRITE                  
**       MVI   O_FLAGS4,O_RDONLY         EN FOR READ AND WRITE                  
BPX1OPN  EQU   156                                                              
BPX1RED  EQU   176                                                              
BPX1CLO  EQU   72                                                               
BPX1WRT  EQU   220                                                              
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         LA    R3,CONFILE                                                       
         ST    R3,BUFADDR                                                       
         LA    RE,L'CONFILE(R3)                                                 
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         B     *-10                                                             
         LA    RE,1(RE)                                                         
         SR    RE,R3                                                            
         ST    RE,BUFLEN                                                        
*                                                                               
         XC    S_MODE,S_MODE                                                    
         MVI   S_MODE2,S_IRUSR       USER READ/WRITE, GROUP READ,               
         MVI   S_MODE3,S_IWUSR+S_IRGRP+S_IROTH           OTHER READ             
         XC    O_FLAGS(OPNF#LENGTH),O_FLAGS                                     
         MVI   O_FLAGS4,O_CREAT+O_RDWR CREATE, OPEN FOR READ AND WRITE          
         L     R4,16           CVT - COMMON VECTOR TABLE                        
         L     R4,544(R4)      CSRTABLE                                         
         L     R4,24(R4)       CSR SLOT                                         
         L     RF,BPX1OPN(R4)  ADDRESS OF THE SERVICE BPX1OPN                   
         CALL  (15),                 OPEN A FILE  BPX1OPN              X        
               (BUFLEN,              INPUT: PATHNAME LENGTH            X        
               CONFILE,              INPUT: PATHNAME                   X        
               O_FLAGS,              INPUT: ACCESS            BPXYOPNF X        
               S_MODE,               INPUT: MODE    BPXYMODE, BPXYFTYP X        
               RETVAL,               RETURN VALUE:-1 OR FILE DESCRIPTO X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL    TEST RETVAL           BL    PSEUD           
         BL    OPENNG                                                           
         ST    RF,FILEDESC          SAVE FILE DESCRIPTOR                        
*                                                                               
*                                                                               
*                                                                               
         MVC   BUFLEN,=F'81'                                                    
         MVC   ALET,=F'0'                                                       
         LA    R3,BUFFER2                                                       
*                                                                               
WRTA     CLI   0(R3),X'FF'                                                      
         BE    WRTX                                                             
         MVC   P(81),0(R3)                                                      
         GOTO1 =V(PRINTER)                                                      
         ST    R3,BUFADDR                                                       
         LA    R3,81(R3)                                                        
         L     RF,BPX1WRT(R4)  ADDRESS OF THE SERVICE BPX1CLO                   
         CALL  (15),                 Write to a file                   x        
               (FILEDESC,            Input: File descriptor            x        
               BUFADDR,              Input: ->Buffer                   x        
               ALET,                 Input: Buffer ALET                x        
               BUFLEN,               Input: Number of bytes to writ    x        
               RETVAL,               Return value: -1 or bytes writ    x        
               RETCODE,              Return code                       x        
               RSNCODE),             Reason code                       x        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL    TEST RETVAL           BL    PSEUD           
         BL    WRTNG                                                            
         B     WRTA                                                             
*                                                                               
*                                                                               
WRTX     DS    0H                                                               
*                                                                               
*                                                                               
         L     RF,BPX1CLO(R4)  ADDRESS OF THE SERVICE BPX1CLO                   
         CALL  (15),                 Close a file                      x        
               (FILEDESC,            Input: File descriptor            x        
               RETVAL,               Return value: 0 or -1             x        
               RETCODE,              Return code                       x        
               RSNCODE),             Reason code                       x        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL    TEST RETVAL           BL    PSEUD           
         BL    CLOSENG                                                          
*                                                                               
         B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
**  READ HFS DATA                                                               
         MVC   BUFLEN,=F'80'        READ BUFFER LENGTH                          
         SPACE ,                                                                
         L     RF,BPX1RED(R4)  ADDRESS OF THE SERVICE BPX1OPN                   
         CALL  (15),                 READ FROM A FILE                  X        
               (FILEDESC,           INPUT: FILE DESCRIPTOR             X        
               BUFADDR,              ->BUFFER TO READ INTO             X        
               BUFFALET,             INPUT: BUFFER ALET                X        
               BUFLEN,               INPUT: NUMBER OF BYTES TO READ    X        
               RETVAL,               RETURN VALUE: 0, -1, OR CHAR COUN X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL    TEST RETVAL           BL    PSEUD           
         BL    READNG                                                           
         L     R7,BUFADDR                                                       
         ABEND 3,DUMP                                                           
*  NEED CODES FOR NO MORE DATA                                                  
         EJECT                                                                  
EXIT     XBASE                                                                  
         EJECT                                                                  
*                                                                               
OPENNG   DS    0H                                                               
         ICM   R7,B'1111',RETCODE    RETURN CODE                                
         ICM   R8,B'1111',RSNCODE    REASON CODE                                
         ABEND 1,DUMP                                                           
WRTNG    DS    0H                                                               
         ICM   R7,B'1111',RETCODE    RETURN CODE                                
         ICM   R8,B'1111',RSNCODE    REASON CODE                                
         ABEND 2,DUMP                                                           
CLOSENG  DS    0H                                                               
         ICM   R7,B'1111',RETCODE    RETURN CODE                                
         ICM   R8,B'1111',RSNCODE    REASON CODE                                
         ABEND 3,DUMP                                                           
READNG   DS    0H                                                               
         ABEND 4,DUMP                                                           
SIZE     DC    F'2000'                                                          
SAVE     DS    20F                                                              
DMCB     DS    6F                                                               
ALET     DS    F                                                                
RETCODE  DS    F                RETURN CODE (ERRNO)                             
RSNCODE  DS    F                REASON CODE                                     
RETVAL   DS    F                RETURN VALUE (0, -1 OR OTHER)                   
FILEDESC DS    F                FILE DESCRIPTOR                                 
PLIST    DS    13A              CALL PARMLIST WORK AREA                         
BUFADDR  DS    A                                                                
BUFLEN   DS    F                                                                
CARD     DS    CL80                                                             
CONFILE  DS    CL60                                                             
         DC    C'**BUFFER**'                                                    
BUFFER   DS    CL80                                                             
         DC    C'**BUFFE2**'                                                    
BUFFER2  DC    CL80'//*****creation of a user type variable */'                 
         DC    X'15'                                                            
         DC    CL24'THE_USER=SHO USR EMAIL '                                    
EMAILADR DC    CL55' '                                                          
         DC    C';'                                                             
         DC    X'15'                                                            
         DC    CL18'FILE_TO_SEND=FILE '                                         
FILENAME DC    CL62' '                                                          
         DC    X'15'                                                            
         DC    CL80'PATH "/u/bde1/ ";'                                          
         DC    X'15'                                                            
         DC    CL80'IF (THE_USER IS NOT EMPTY) THEN'                            
         DC    X'15'                                                            
         DC    CL80'  DOCLIST=SND TO THE_USER DOC FILE_TO_SEND '                
         DC    X'15'                                                            
         DC    CL8'  SBJ   '                                                    
SUBJECT  DC    CL71' '                                                          
         DC    C';'                                                             
         DC    X'15'                                                            
         DC    CL80'  COMPLETED_LIST=DOCLIST{DOC COMPLETED};'                   
         DC    X'15'                                                            
         DC    CL80'  IF (COMPLETED_LIST IS NOT EMPTY) THEN'                    
         DC    X'15'                                                            
         DC    CL80'    display COMPLETED_LIST;'                                
         DC    X'15'                                                            
         DC    CL80'    sho snd recipient "*" last 1 completed;'                
         DC    X'15'                                                            
         DC    CL80'    EXEC PROGRAM "/u/bde1/email-bdeexec1.rex C @@CO+        
               MPLETED_LIST @THE_USER";'                                        
         DC    X'15'                                                            
         DC    CL80'  ELSE'                                                     
         DC    X'15'                                                            
         DC    CL80'    FAILED_LIST=DOCLIST{DOC FAILED};'                       
         DC    X'15'                                                            
         DC    CL80'    display FAILED_LIST;'                                   
         DC    X'15'                                                            
         DC    CL80'  ENDIF;'                                                   
         DC    X'15'                                                            
         DC    CL80'ELSE'                                                       
         DC    X'15'                                                            
         DC    CL80'  exec program "echo user NOT IN BDE ";'                    
         DC    X'15'                                                            
         DC    CL80'  EXEC PROGRAM "/u/bde1/email-bdeexec1.rex E @THE_U+        
               SER";'                                                           
         DC    X'15'                                                            
         DC    CL80'ENDIF;'                                                     
         DC    X'15'                                                            
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
* FLAGS                                                                         
         BPXYOPNF DSECT=NO                                                      
         BPXYMODE DSECT=NO                                                      
       ++INCLUDE DDDPRINT                                                       
YYUNUSS  CSECT                                                                  
OFLGSLEN DC    A(OPNF#LENGTH)                                                   
SMODELEN DC    A(S_MODE#LENGTH)                                                 
BUFFALET DC    F'0'                                                             
         BPXYFTYP                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001YYUNUSS   03/20/01'                                      
         END                                                                    
