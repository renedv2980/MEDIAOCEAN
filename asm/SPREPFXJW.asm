*          DATA SET SPREPFXJW  AT LEVEL 030 AS OF 08/28/00                      
*PHASE SPFX02J                                                                  
*===============================================================*               
* THIS PROGRAM READS LISTS OF RADIO/TV/CABLE STATIONS           *               
* FOR THE JWT MINDSHARE STATION FILE CONVERSION                 *               
*===============================================================*               
         TITLE 'SPFX02 - UNSCREW DPI FOR T,MS,DPI'                              
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC                                                      
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX02                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* READ INPUT FILES AND BUILD A LIST OF STATIONS USING BINSRCH         *         
***********************************************************************         
         SPACE 1                                                                
FX02     MVC   KEY(4),=X'00318DE8' MS/T/DPI CLIENT HEADER                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETCLT                                                           
*                                                                               
         L     R6,ADCLT                                                         
         USING CLTHDRD,R6                                                       
         LA    R1,CLIST                                                         
*                                                                               
FX04     SR    RE,RE                                                            
         IC    RE,3(R1)                                                         
         BCTR  RE,0                                                             
         MHI   RE,8                                                             
         LA    RE,TAB(RE)                                                       
         MVC   0(4,RE),0(R1)                                                    
*                                                                               
         LA    R1,4(R1)                                                         
         CLI   0(R1),C'A'                                                       
         BNL   FX04                                                             
*                                                                               
* NOW READ PRDHDRS                                                              
*                                                                               
FX10     GOTO1 SEQ                                                              
         CLC   KEY(4),KEYSAVE                                                   
         BNE   FX20                                                             
         OC    KEY+7(6),KEY+7      TEST ESTIMATE                                
         BNZ   FX10                                                             
*                                                                               
         GOTO1 GETPRD                                                           
*                                                                               
         L     R6,ADPRD                                                         
         USING PRDHDR,R6                                                        
*                                                                               
         SR    RE,RE                                                            
         IC    RE,PCODE+1                                                       
         BCTR  RE,0                                                             
         MHI   RE,8                                                             
         LA    RE,TAB(RE)                                                       
         MVC   4(3,RE),KEY+4       MOVE PRD CODE                                
         MVC   7(1,RE),PCODE+1                                                  
         B     FX10                                                             
*                                                                               
FX20     LA    R4,TAB                                                           
         LA    R5,1                                                             
*                                                                               
FX22     OC    0(8,R4),0(R4)       TEST NULL ENTRY                              
         BZ    FX24                                                             
*                                                                               
         CLC   0(4,R4),4(R4)                                                    
         BE    *+10                                                             
         MVC   P+25(5),=C'<<<<<'                                                
*                                                                               
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P(3),DUB                                                         
*                                                                               
         MVC   P+6(3),0(R4)                                                     
         MVC   P+10(3),4(R4)                                                    
         GOTO1 HEXOUT,DMCB,3(R4),P+15,1,=C'TOG'                                 
         GOTO1 (RF),(R1),7(R4),P+19,1                                           
         GOTO1 REPORT                                                           
*                                                                               
FX24     LA    R4,8(R4)                                                         
         AHI   R5,1                                                             
         CHI   R5,220                                                           
         BL    FX22                                                             
         GOTO1 AENDREQ                                                          
         LTORG                                                                  
*                                                                               
TAB      DC    880X'00'                                                         
*                                                                               
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
*                                                                               
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030SPREPFXJW 08/28/00'                                      
         END                                                                    
