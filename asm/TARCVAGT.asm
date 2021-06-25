*          DATA SET TARCVAGT   AT LEVEL 030 AS OF 03/09/10                      
*PHASE TALOOKA                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PRNTBL                                                                 
         TITLE 'TARCVLOOK - SEARCH RECOVERY FILE AND PRINT'                     
RCVLOOK  CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NBASE 0,RCVLOOK,VREGSAVE                                               
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING RCVLOOK+4096,RC                                                  
*                                                                               
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         MVC   TITLE(20),=C'TALENT RECOVERY LOOK'                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         B     INIT2                                                            
*                                                                               
VREGSAVE DC    V(REGSAVE)                                                       
         EJECT                                                                  
INIT2    DS    0H                                                               
         LA    RE,RCVLOOK          SET FOR STXITER                              
         L     RF,=V(STXITER)                                                   
         STM   RE,RF,DUB                                                        
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
*                                                                               
         OPEN  (RECVIN,(INPUT))                                                 
         OPEN  (RECVOUT,(OUTPUT))                                               
*                                                                               
         MVC   TITLE(30),=CL30'RECOVERY LOOK PROGRAM'                           
         B     IN2                                                              
         EJECT                                                                  
*** PROCESS INPUT FILE ***                                                      
*                                                                               
IN2      GET   RECVIN,RCVREC                                                    
*                                                                               
         CLI   RKEY,TLCACDQ        LOOKING FOR CAST RECORDS ONLY                
         BNE   IN2                                                              
*                                                                               
         CLI   RRECTY,1            TEST FOR COPY                                
         BNE   *+12                                                             
         BAS   RE,COPY             SAVE COPY                                    
         B     IN2                                                              
*                                                                               
         CLI   RRECTY,2            TEST FOR CHANGE                              
         BNE   IN2                                                              
         BAS   RE,CHANGE           HANDLE CHANGE                                
         B     IN2                                                              
*                                                                               
MYKEY1   DC    X'A0',C'8015  ',X'1991030051000A000000000D'                      
         DC    C'071428006W  ',X'FF'                                            
MYKEY1LQ EQU   *-MYKEY1                                                         
         DC    X'FF'                                                            
         EJECT                                                                  
COPY     NTR1                                                                   
         XC    SVNCDE,SVNCDE                                                    
         LA    R4,RKEY                                                          
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TACAD,R4                                                         
         MVC   SVNCDE,TACANCDE     SAVE AGENT CODE                              
         SPACE 1                                                                
         LA    R0,SAVREC           COPY IT TO SAVREC                            
         LA    R1,2128                                                          
         LA    RE,RCVREC                                                        
         LH    RF,RCVREC                                                        
         MVCL  R0,RE                                                            
         B     XIT                                                              
         SPACE 3                                                                
CHANGE   NTR1                                                                   
         LA    R4,RKEY                                                          
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TACAD,R4                                                         
         CLC   SVNCDE,TACANCDE     DID AGENT CODE CHANGE                        
         BE    XIT                                                              
         CLC   TACANCDE,=Y(0373)   WAS IT CHANGED TO 0373                       
         BNE   XIT                                                              
         MVI   RRECTY,1            SET THIS RECORD IS COPY                      
         PUT   RECVOUT,RCVREC      WRITE TO OUTPUT TAPE                         
         SPACE 1                                                                
         MVI   SHDR+RRECTY-RECVHDR,2  SET SAVED RECORD IS CHANGE                
         PUT   RECVOUT,SAVREC         WRITE TO OUTPUT TAPE                      
         SPACE 1                                                                
         AP    OUTCNT,=P'1'        ADD TO OUTPUT COUNT                          
         GOTO1 TRNSAGT,DMCB,(X'40',SVNCDE),P+1    PRINT OLD CODE                
         GOTO1 TRNSAGT,DMCB,(X'40',TACANCDE),P+6  NEW CODE                      
         LA    R3,RKEY                                                          
         USING TLCAD,R3                                                         
         MVC   P+11(9),TLCASSN     PRINT S/S NUMBER                             
         SPACE 1                                                                
         LA    R4,RKEY                                                          
         MVI   ELCODE,TAACELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   CHA40                                                            
         USING TAACD,R4                                                         
         MVC   P+21(8),TAACSTAF    PRINT STAFF NAME                             
         GOTO1 =V(DATCON),DMCB,(1,TAACCDTE),(8,P+30)                            
         SPACE 1                                                                
CHA40    CLC   PSAVE,P             IS THIS SAME AS LAST PRINTED LINE            
         BE    XIT                 YES, SO DON'T BOTHER                         
         MVC   PSAVE,P             ELSE SAVE THIS LINE                          
         GOTO1 =V(PRINTER)         AND PRINT IT                                 
         B     XIT                                                              
         SPACE 1                                                                
PSAVE    DC    CL80' '                                                          
         EJECT                                                                  
PRINTIT  DS    0H                                                               
         SR    R0,R0                                                            
         ICM   R0,3,RECVHDR-4      GET RECORD LENGTH                            
         GOTO1 =V(PRNTBL),DMCB,0,RECVHDR-4,C'DUMP',(R0),=C'1D'                  
         B     IN2                                                              
*                                                                               
ENDIN    DS    0H                                                               
         CLOSE RECVIN                                                           
         CLOSE RECVOUT                                                          
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         EDIT  (P4,OUTCNT),(10,P+1),ZERO=NOBLANK,COMMAS=YES                     
         MVC   P+12(7),=C'CHANGES'                                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
EOJ      DS    0H                                                               
         XBASE                                                                  
*                                                                               
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
***********************************************************************         
*        ROUTINE TO TRANSLATE AGENT CODE                              *         
*        INCLUDED BY TASYSVAL AND TALNK01                             *         
*        ON ENTRY ... P1 BYTE 0 = X'80' CHARACTER TO NUMERIC          *         
*                                 X'40' NUMERIC TO CHARACTER          *         
*                     P1        = A(INCOMING AGENT CODE)              *         
*                     P2        = A(TRANSLATED AGENT CODE)            *         
***********************************************************************         
                                                                                
TRNSAGT  NTR1                                                                   
       ++INCLUDE TATRNSAGT                                                      
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,LRECL=2100,             X        
               MACRF=GM,EODAD=ENDIN                                             
         SPACE 1                                                                
RECVOUT  DCB   DDNAME=RECVOUT,DSORG=PS,RECFM=VB,LRECL=2048,            X        
               MACRF=PM,BLKSIZE=8200,BUFNO=2                                    
         EJECT                                                                  
DMCB     DS    6F                                                               
MYSEQ    DC    F'0'                                                             
OUTCNT   DC    PL4'0'                                                           
DUB      DS    D                                                                
WORK     DS    XL64                                                             
DATADISP DC    Y(TLRCELEM-TLRCD)                                                
ELCODE   DS    XL1                                                              
SVNCDE   DS    H                   SAVED AGENT CODE                             
*                                                                               
         DS    0D                                                               
         DC    C'*RCVREC*'                                                      
RCVREC   DC    F'0'                VAR REC LEN                                  
       ++INCLUDE DMRCVRHDR                                                      
         SPACE 1                                                                
RKEY     DS    0CL32                                                            
         DS    2100C                                                            
*                                                                               
         DS    0D                                                               
         DC    C'*SAVREC*'                                                      
SAVREC   DC    F'0'                VAR REC LEN                                  
SHDR     DS    CL24                RCV HDR                                      
         SPACE 1                                                                
SKEY     DS    0CL32                                                            
         DS    2100C                                                            
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
* TAGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030TARCVAGT  03/09/10'                                      
         END                                                                    
