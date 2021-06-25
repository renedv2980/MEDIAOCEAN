*          DATA SET SPREPFXANN AT LEVEL 052 AS OF 03/26/99                      
*PHASE SPFX02C                                                                  
SPFX02C  TITLE 'SPFX02C - COPY CAMPAIGNS FROM WILA TO WIAP'                     
         SPACE 1                                                                
SPFX02   CSECT                                                                  
         DS    6000C                                                            
         ORG   SPFX02                                                           
         SPACE 1                                                                
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02                                                         
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02,RB,RC                                                     
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX2                                                              
*                                                                               
YES      CR    RB,RB               SET CC EQUAL                                 
         B     XIT                                                              
*                                                                               
NO       LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
*=========================================================*                     
* REQFRST PROCESSING                                      *                     
*=========================================================*                     
         SPACE 1                                                                
FX2      DS    0H                                                               
*                                                                               
         OPEN  (FILEIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,CAMPTAB          TABLE=3-BUYER,1-CODE,5*2SEQ,2-CAMP           
*                                                                               
FX20     LA    R0,IOLEN                                                         
         GET   FILEIN,(0)                                                       
*                                                                               
         OC    IO+4(80),SPACES                                                  
         L     RF,ACOMFACS                                                      
         L     RF,CSCANNER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(C'C',IO),SCANOUT                                      
         LA    R2,SCANOUT                                                       
         MVC   0(3,R3),12(R2)      BUYER                                        
         XC    3(11,R3),3(R3)       BUYER CODE AND SEQ                          
         MVC   14(2,R3),38(R2)      CAMPAIGN NUM                                
         XC    14(2,R3),=X'FFFF'    CAMPAIGN NUMBER COMPLIMENT                  
         LA    R3,16(R3)                                                        
         B     FX20                                                             
FX99     MVI   0(R3),X'FF'                                                      
         CLOSE FILEIN                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FX100    LA    R1,IO               GET BUYER CODES FROM BUYER RECS              
         ST    R1,AREC                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(3),=X'0D6511'    NWS BUYER RECS                              
*        MVC   KEY(3),=X'0D6591'    NWS BUYER RECS                              
         GOTO1 HIGH                                                             
         B     FX110                                                            
FX110SEQ GOTO1 SEQ                                                              
FX110    CLC   KEY(3),KEYSAVE                                                   
         BNE   FX199                                                            
*                                                                               
         GOTO1 GET                 GET REC INTO IO                              
         LA    R2,IO                                                            
         LA    R2,24(R2)                                                        
         CLI   0(R2),X'01'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUYBYTE,2(R2)       SAVE BUYER CODE                              
*                                                                               
         LA    R3,CAMPTAB                                                       
FX120    CLI   0(R3),X'FF'                                                      
         BE    FX110SEQ                                                         
         CLC   0(3,R3),KEY+3       MATCH ON BUYER                               
         BH    FX110SEQ            TABLE IS SORTED                              
         BNE   *+10                                                             
         MVC   3(1,R3),BUYBYTE                                                  
         LA    R3,16(R3)                                                        
         B     FX120                                                            
                                                                                
*                                                                               
FX199    DS    0H                                                               
         OPEN  (FILEOUT,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,CAMPTAB                                                       
*                                                                               
FX200    XC    KEY,KEY                                                          
         MVC   KEY(3),=X'0D6611'   NWS CAMPAIGN RECS                            
         MVC   KEY+3(1),3(R3)      BUYER CODE                                   
         MVC   KEY+4(2),14(R3)      CAMP NUM                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   FX299                                                            
         GOTO1 GET                 GET REC INTO IO                              
         L     R4,AREC                                                          
         MVC   BYTE,2(R4)          A/M                                          
         NI    BYTE,X'0F'                                                       
         OI    BYTE,X'90'          CHANGE TO X'9'                               
         MVC   2(1,R4),BYTE                                                     
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,13(R4)         LEN FOR PUT                                  
         AHI   R1,4                                                             
         STH   R1,IOLEN                                                         
         LA    R0,IOLEN                                                         
         PUT   FILEOUT,(0)                                                      
*                                                                               
FX299    LA    R3,16(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BNE   FX200                                                            
*                                                                               
*                                                                               
         LA    R3,CAMPTAB                                                       
FX300    XC    KEY,KEY                                                          
         MVC   KEY(3),=X'0D6711'   NWS CAMPAIGN HDR RECS - GET SEQ #            
         MVC   KEY+3(1),3(R3)      BUYER CODE                                   
         MVC   KEY+4(2),14(R3)      CAMP NUM                                    
         GOTO1 HIGH                                                             
         B     FX310                                                            
FX310SEQ GOTO1 SEQ                                                              
FX310    CLC   KEY(6),KEYSAVE                                                   
         BNE   FX399               GET NEXT TABLE ENTRY                         
*                                                                               
         OC    4(2,R3),4(R3)       ROOM FOR 2 SEQ NUMS                          
         BNZ   FX315                                                            
         MVC   4(2,R3),KEY+8       SAVE SEQ NUM IN TABLE                        
         B     FX320                                                            
FX315    OC    6(2,R3),6(R3)                                                    
         BNZ   FX316                                                            
         MVC   6(2,R3),KEY+8       SAVE SEQ NUM IN TABLE                        
         B     FX320                                                            
FX316    OC    8(2,R3),8(R3)                                                    
         BNZ   FX317                                                            
         MVC   8(2,R3),KEY+8       SAVE SEQ NUM IN TABLE                        
         B     FX320                                                            
FX317    OC    10(2,R3),10(R3)                                                  
         BNZ   FX318                                                            
         MVC   10(2,R3),KEY+8       SAVE SEQ NUM IN TABLE                       
         B     FX320                                                            
FX318    OC    12(2,R3),12(R3)                                                  
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   12(2,R3),KEY+8       SAVE SEQ NUM IN TABLE                       
*                                                                               
FX320    GOTO1 GET                 GET REC INTO IO                              
         L     R4,AREC                                                          
         MVC   BYTE,2(R4)          A/M                                          
         NI    BYTE,X'0F'                                                       
         OI    BYTE,X'90'          CHANGE TO X'9'                               
         MVC   2(1,R4),BYTE                                                     
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,13(R4)         LEN FOR PUT                                  
         AHI   R1,4                                                             
         STH   R1,IOLEN                                                         
         LA    R0,IOLEN                                                         
         PUT   FILEOUT,(0)                                                      
         B     FX310SEQ            CHECK FOR MORE FOR THIS CAMP                 
*                                                                               
FX399    LA    R3,16(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BNE   FX300                                                            
*                                                                               
*                                                                               
         LA    R3,CAMPTAB                                                       
FX400    XC    KEY,KEY                                                          
         MVC   KEY(3),=X'0D6911'   NWS COMMENT  RECS                            
         MVC   KEY+3(1),3(R3)      BUYER CODE                                   
         MVC   KEY+4(2),14(R3)      CAMP NUM                                    
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BNE   FX499                                                            
         GOTO1 GET                 GET REC INTO IO                              
         L     R4,AREC                                                          
         MVC   BYTE,2(R4)          A/M                                          
         NI    BYTE,X'0F'                                                       
         OI    BYTE,X'90'          CHANGE TO X'9'                               
         MVC   2(1,R4),BYTE                                                     
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,13(R4)         LEN FOR PUT                                  
         AHI   R1,4                                                             
         STH   R1,IOLEN                                                         
         LA    R0,IOLEN                                                         
         PUT   FILEOUT,(0)                                                      
*                                                                               
FX499    LA    R3,16(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BNE   FX400                                                            
*                                                                               
         LA    R3,CAMPTAB                                                       
FX500    XC    KEY,KEY                                                          
         MVC   KEY(3),=X'0D6A11'   NWS GOAL RECS                                
         MVC   KEY+3(1),3(R3)      BUYER CODE                                   
         MVC   KEY+4(2),14(R3)      CAMP NUM                                    
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BNE   FX599                                                            
         GOTO1 GET                 GET REC INTO IO                              
         L     R4,AREC                                                          
         MVC   BYTE,2(R4)          A/M                                          
         NI    BYTE,X'0F'                                                       
         OI    BYTE,X'90'          CHANGE TO X'9'                               
         MVC   2(1,R4),BYTE                                                     
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,13(R4)         LEN FOR PUT                                  
         AHI   R1,4                                                             
         STH   R1,IOLEN                                                         
         LA    R0,IOLEN                                                         
         PUT   FILEOUT,(0)                                                      
*                                                                               
FX599    LA    R3,16(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BNE   FX500                                                            
*                                                                               
         LA    R3,CAMPTAB                                                       
FX600    XC    KEY,KEY                                                          
         MVC   KEY(3),=X'0D6811'   NWS DETAIL RECS                              
         MVC   KEY+3(1),3(R3)      BUYER CODE                                   
         OC    4(2,R3),4(R3)       ANY SEQ NUMBER                               
         BZ    FX699                                                            
         MVC   KEY+4(2),4(R3)      FIRST SEQ NUMBER                             
         GOTO1 HIGH                                                             
         B     FX610                                                            
FX610SEQ GOTO1 SEQ                                                              
FX610    CLC   KEY(4),KEYSAVE                                                   
         BNE   FX699               GET NEXT TABLE ENTRY                         
         CLC   KEY+4(2),4(R3)      MATCH SEQ NUMBER                             
         BE    FX615                                                            
         CLC   KEY+4(2),6(R3)      MATCH SEQ NUMBER                             
         BE    FX615                                                            
         CLC   KEY+4(2),8(R3)      MATCH SEQ NUMBER                             
         BE    FX615                                                            
         CLC   KEY+4(2),10(R3)     MATCH SEQ NUMBER                             
         BE    FX615                                                            
         CLC   KEY+4(2),12(R3)     MATCH SEQ NUMBER                             
         BNE   FX610SEQ                                                         
FX615    GOTO1 GET                 GET REC INTO IO                              
         L     R4,AREC                                                          
         MVC   BYTE,2(R4)          A/M                                          
         NI    BYTE,X'0F'                                                       
         OI    BYTE,X'90'          CHANGE TO X'9'                               
         MVC   2(1,R4),BYTE                                                     
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,13(R4)         LEN FOR PUT                                  
         AHI   R1,4                                                             
         STH   R1,IOLEN                                                         
         LA    R0,IOLEN                                                         
         PUT   FILEOUT,(0)                                                      
         B     FX610SEQ                                                         
*                                                                               
FX699    LA    R3,16(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BNE   FX600                                                            
*                                                                               
*                                                                               
*                                                                               
         LA    R3,CAMPTAB                                                       
FX700    MVC   P(3),0(R3)                                                       
         GOTO1 HEXOUT,DMCB,3(R3),P+6,1,=C'TOG'                                  
         GOTO1 HEXOUT,DMCB,4(R3),P+9,2,=C'TOG'                                  
         GOTO1 HEXOUT,DMCB,6(R3),P+14,2,=C'TOG'                                 
         GOTO1 HEXOUT,DMCB,8(R3),P+19,2,=C'TOG'                                 
         GOTO1 HEXOUT,DMCB,10(R3),P+24,2,=C'TOG'                                
         GOTO1 HEXOUT,DMCB,12(R3),P+29,2,=C'TOG'                                
         GOTO1 HEXOUT,DMCB,14(R3),P+35,2,=C'TOG'                                
         GOTO1 REPORT                                                           
         LA    R3,16(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BNE   FX700                                                            
*                                                                               
*                                                                               
         EJECT                                                                  
FX999    MVC   P(14),=C'DONE          '                                         
         GOTO1 REPORT                                                           
*                                                                               
         CLOSE FILEOUT                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
*                                                                               
         GETEL R6,24,ELCODE                                                     
         LTORG                                                                  
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=VB,LRECL=2048,            X        
               BLKSIZE=25000,MACRF=PM                                           
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=VB,LRECL=255,              X        
               MACRF=GM,EODAD=FX99                                              
*                                                                               
MYCOUNT  DC    PL4'0'                                                           
MAXCOUNT DC    PL4'9999999'                                                     
SEQNUM   DC    PL4'0'                                                           
SCANOUT  DS    XL100                                                            
         DS    CL9                                                              
         DS    D                                                                
IOLEN    DS    XL4                                                              
IO       DS    CL2000                                                           
ELCODE   DS    CL1                                                              
BUYBYTE  DS    XL1                                                              
CAMPSEQ  DS    XL2                                                              
*                                                                               
CAMPTAB  DS    XL12000                                                          
CAMPTABX DS    X                                                                
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE DDCOMFACSD                                                     
         PRINT ON                                                               
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
AGYRECD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052SPREPFXANN03/26/99'                                      
         END                                                                    
