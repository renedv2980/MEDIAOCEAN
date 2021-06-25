*          DATA SET SPREPFXBS1 AT LEVEL 017 AS OF 01/09/97                      
*PHASE SPFX02V                                                                  
         TITLE 'SPFX02 - EXTRACT MISSING BS BUY RECORDS'                        
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
         EJECT                                                                  
FX       DS    0H                                                               
         XC    COUNT,COUNT                                                      
         OPEN  (FILEOUT,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(10),=X'52B23D1305045CAA6104'  R/MR4/PB/1284/KKIJF            
         GOTO1 HIGH                                                             
         B     FX20                                                             
                                                                                
FX10     GOTO1 SEQ                                                              
*                                                                               
FX20     CLC   KEY(10),KEYSAVE                                                  
         BNE   FX100                                                            
         CLI   KEY+12,7            LINES 1-7 ONLY                               
         BH    FX100                                                            
         BAS   RE,PUTIT                                                         
         B     FX10                                                             
*                                                                               
PUTIT    NTR1                                                                   
         GOTO1 GETBUY                                                           
         L     RE,ADBUY                                                         
* INCREMENT LINE NUMBER                                                         
         ZIC   R0,10(RE)           GET LINE NUMBER                              
         AH    R0,=H'51'                                                        
         STC   R0,10(RE)                                                        
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LA    R0,4(RF)         GET RECLEN+4                                    
         SLL   R0,16               LEFT ALIGN                                   
         SH    RE,=H'4'            BACK UP TO REC-4                             
         ST    R0,0(RE)                                                         
         LR    R0,RE                                                            
         PUT   FILEOUT,(R0)                                                     
*                                                                               
         L     R1,COUNT                                                         
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
*                                                                               
         L     R6,ADBUY                                                         
         SH    R6,=H'4'                                                         
         GOTO1 HEXOUT,DMCB,(R6),P,20,=C'TOG'                                    
         GOTO1 REPORT                                                           
         B     EXIT                                                             
                                                                                
                                                                                
FX100    MVC   P(5),=C'COUNT'                                                   
         EDIT  (4,COUNT),(8,P+10)                                               
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
*                                                                               
         GETEL R6,24,ELCODE                                                     
*                                                                               
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=VB,LRECL=2008,            X        
               MACRF=PM,BLKSIZE=16000                                           
*                                                                               
         LTORG                                                                  
*                                                                               
         DS    0F                                                               
COUNT    DS    F                                                                
SAVEKEY  DS    XL18                                                             
ELCODE   DS    X                                                                
STAWORK  DS    XL31                                                             
BAGYTAB  DS    16XL4                                                            
         DS    F                                                                
RECORD   DS    CL50                                                             
SPACE    DS    XL2000                                                           
                                                                                
*                                                                               
AGENCYD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENSTAB                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017SPREPFXBS101/09/97'                                      
         END                                                                    
