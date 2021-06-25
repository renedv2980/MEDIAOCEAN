*          DATA SET SPREPSX03  AT LEVEL 014 AS OF 02/20/01                      
*PHASE SPSX03T,*,NOAUTO                                                         
         TITLE 'MARKET FIX PROGRAM - SUBCONTROLLER'                             
*                                                                               
*  LEV  7    MAR07/88 FIX CLTFRST (WAS BYPASSING)                               
*  LEV  8-10 SEP07/89 FIX QSTA MEDIA C (MSPACK NO LONGER PACKS IT)              
*  LEV 11    JUL01/93 ADD CABLE                                                 
*  LEV 12    FEB05/98 EXPAND CANADIAN NETWORKS TO FULL BYTE FROM 5 BITS         
*  LEV 13    FEB16/01 TRANSFER NEW MEDIA - BUY COPIES X'08'                     
*                                                                               
SPSX03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPSX03                                                         
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         CLI   QSTA+4,C'/'                                                      
         BE    *+12                                                             
         CLI   QSTA+4,C'C'                                                      
         BNE   *+8                                                              
         MVI   QSTA+4,C'T'         SET MEDIA TO T SO MSPACK WILL DO IT          
*                                                                               
         GOTO1 MSPACK,DMCB,QMKT,QSTA,SVMKTSTA                                   
         CLI   QMED,C'C'                                                        
         BNE   *+8                                                              
         NI    SVMKTSTA+4,X'00'    SET MEDIA 8 BITS TO ZERO                     
*                                                                               
         CLI   QSTA,C'0'                                                        
         BL    *+8                                                              
         NI    SVMKTSTA+4,X'80'    SET CABLE 7 BITS TO ZERO                     
*                                                                               
SPCTL00  GOTO1 FCNXTCLT            GET NEXT CLIENT                              
         BE    SPCTL10                                                          
         XIT1                                                                   
*                                                                               
*PCTL10  CLI   USRSW2,C'Y'                                                      
*        BNE   SPCTL20                                                          
SPCTL10  MVI   MODE,CLTFRST                                                     
         GOTO1 GO                                                               
         MVI   USRSW2,C'N'                                                      
*                                                                               
SPCTL20  MVC   KEY(1),SVAGYMD                                                   
         MVC   KEY+1(2),SVCLT                                                   
         MVI   KEY+3,1                                                          
         MVC   KEY+4(5),SVMKTSTA                                                
*                                                                               
SPCTL30  XC    KEY+9(8),KEY+9                                                   
         GOTO1 HIGH                                                             
         B     SPCTL50                                                          
*                                                                               
SPCTL40  GOTO1 SEQ                                                              
*                                                                               
SPCTL50  CLC   KEYSAVE(9),KEY      SAME A-M/CLT/PRD/MKT/STA?                    
         BE    SPCTL60                                                          
         CLC   KEYSAVE(3),KEY      SAME A-M/CLT?                                
         BE    SPCTL52                                                          
         SPACE                                                                  
SPCTL51  DS   0H                                                                
         TM    KEYSAVE,X'08'       DONE COPIED BUYS (SFM)                       
         BO    SPCTL90              YES                                         
         XC    KEY,KEY                                                          
         MVC   KEY(1),SVAGYMD                                                   
         OI    KEY,X'08'                                                        
         MVC   KEY+1(2),SVCLT                                                   
         MVI   KEY+3,1                                                          
         MVC   KEY+4(5),SVMKTSTA                                                
         SPACE                                                                  
         GOTO1 HIGH                                                             
         B     SPCTL50                                                          
         SPACE                                                                  
SPCTL52  DS    0H                                                               
         CLC   KEYSAVE(4),KEY      SAME A-M/CLT/PRD?                            
         BNE   SPCTL54                                                          
         SPACE                                                                  
         CLI   QSTA,C'0'           IS THIS CABLE                                
         BL    SPCTL70                                                          
         MVC   DUB(5),KEY+4        SAVE MARKET/STATION                          
         NI    DUB+4,X'80'                                                      
         CLC   SVMKTSTA(5),DUB                                                  
         BE    SPCTL60                                                          
         B     SPCTL70                                                          
         SPACE                                                                  
SPCTL54  MVC   KEY+4(5),KEYSAVE+4  RESTORE MKT/STA                              
         CLI   QMED,C'C'                                                        
         BNE   SPCTL56                                                          
         NI    KEY+8,X'00'         SET MEDIA 8 BITS TO ZERO                     
         B     SPCTL30                                                          
SPCTL56  CLI   QSTA,C'0'           IS THIS CABLE                                
         BL    SPCTL30                                                          
         NI    KEY+8,X'80'         SET CABLE 7 BITS TO ZERO                     
         B     SPCTL30                                                          
         EJECT                                                                  
SPCTL60  MVI   MODE,PROCBUY        UPDATE THE SPTDIR RECORD                     
         GOTO1 GO                                                               
         CLI   USRSW1,C'N'         MORE THAN 255 BUY LINES FOR STA/EST          
         BE    SPCTL40              PASSIVE ELEMENT - READ SEQ                  
         MVI   USRSW1,C'N'                                                      
*                                                                               
* BYPASS MORE THAN 255 LINES FOR STATION *                                      
*                                                                               
SPCTL64  GOTO1 SEQ                                                              
*                                                                               
         CLC   KEYSAVE(10),KEY    SAME A-M/CLT/PRD/MKT/STA/EST?                 
         BE    SPCTL64                                                          
*                                                                               
         B     SPCTL50            RESUME PROCESSING                             
*                                                                               
SPCTL70  CLC   KEYSAVE(8),KEY                                                   
         BNE   SPCTL80                                                          
         MVC   DUB(1),KEY                                                       
         NI    DUB,X'0F'                                                        
         CLI   DUB,8                IS THIS A 'COMBINED' BUY                    
         BNE   SPCTL80              NO.                                         
         SR    R2,R2                                                            
         ICM   R2,7,KEY+6                                                       
         SRL   R2,8                FOR NEW MSPACK - USES 8 BITS                 
         SR    R3,R3                                                            
         ICM   R3,7,SVMKTSTA+2                                                  
         SRL   R3,8                                                             
         CR    R2,R3               IS THIS THE SAME STATION?                    
         BE    SPCTL60              YES.                                        
*                                                                               
SPCTL80  MVC   KEY(13),KEYSAVE                                                  
         CLI   KEYSAVE+3,X'FF'     END OF PRODUCTS FOR THIS CLIENT?             
         BE    SPCTL51              YES, CHECK COPIES                           
         SPACE                                                                  
         ZIC   RE,KEYSAVE+3         NO, TRY NEXT PRODUCT NUMBER                 
         LA    RE,1(,RE)                                                        
         STC   RE,KEY+3                                                         
         CLI   QMED,C'C'                                                        
         BNE   SPCTL30                                                          
         NI    KEY+8,X'00'         SET MEDIA 8 BITS TO ZERO                     
         B     SPCTL30                                                          
*                                                                               
SPCTL90  DS   0H                                                                
         MVI   MODE,PROCREC        PROCESS NON-BUY RECORDS                      
         GOTO1 GO                                                               
         B     SPCTL00                                                          
         SPACE 5                                                                
         DS    0D                                                               
SVMKTSTA DS    D                                                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SPREPSX03 02/20/01'                                      
         END                                                                    
