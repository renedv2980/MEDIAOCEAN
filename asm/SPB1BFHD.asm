*          DATA SET SPB1BFHD   AT LEVEL 013 AS OF 12/07/89                      
*PHASE SPB1BFHD,+0                                                              
         TITLE 'HARRIS/DONOVAN - SPECIAL BILL FORMULA'                          
         PRINT NOGEN                                                            
SPB1BFHD CSECT                                                                  
         NMOD1 0,**B1BF**                                                       
         USING SPWORKD,RA,R8                                                    
         USING BILWRKD,RC                                                       
         LA    RC,SPACEND                                                       
         SPACE 2                                                                
         CLC   CLT,=C'LAB'         LABATTS                                      
         BNE   BFX                                                              
         CLI   QMGR,C'C'           MGR SCHEME C                                 
         BNE   BF04                                                             
         L     RF,AMGR1                                                         
         CLC   0(2,RF),=C'02'      02 IS ONTARIO                                
         BNE   BFX                                                              
         B     BF06                                                             
*                                                                               
BF04     DS    0H                                                               
         CLI   QMGR,C'B'           MGR SCHEME B                                 
         BNE   BFX                                                              
         L     RF,AMGR1                                                         
         CLC   0(2,RF),=C'04'      04 IS ONTARIO                                
         BNE   BFX                                                              
*                                                                               
BF06     DS    0H                                                               
         CLI   BFMODE,C'F'                                                      
         BNE   BF20                                                             
*                                  FORMULA OVERRIDE                             
         L     RF,AEST                                                          
         CLI   0(RF),3                                                          
         BE    BF08                                                             
         CLI   0(RF),4                                                          
         BNE   BFX                                                              
*                                                                               
BF08     DS    0H                                                               
         MVC   W(5),BFM                                                         
         B     BFX                                                              
*                                                                               
BFM      DC    X'50',FL4'-860000'                                               
*                                                                               
BF20     DS    0H                  AOR OVERRIDE                                 
         L     RF,APRD                                                          
         ZIC   R4,0(RF)            GET PROD CODE FROM PROD SEQ                  
         SLL   R4,2                                                             
         L     RF,ADCLT                                                         
         LA    R4,CLIST-CLTHDR(RF,R4)                                           
         CLC   =C'BL ',0(R4)       SCALI PRODUCTS ONLY                          
         BE    BF21                                                             
         CLC   =C'CA ',0(R4)                                                    
         BNE   BFX                                                              
*                                                                               
BF21     DS    0H                                                               
         L     RF,AEST                                                          
         CLI   0(RF),1                                                          
         BE    BF22                                                             
         CLI   0(RF),2                                                          
         BE    BF22                                                             
         CLI   0(RF),3                                                          
         BE    BF24                                                             
         CLI   0(RF),4                                                          
         BE    BF24                                                             
         B     BFX                                                              
*                                                                               
BF22     DS    0H                                                               
         XC    SVAOREFF,SVAOREFF     CLEAR EFFECTIVE DATE                       
         MVC   SVAORPCT,=F'40000'    SET NEW PCT                                
         B     BFX                                                              
*                                                                               
BF24     DS    0H                                                               
         XC    SVAOREFF,SVAOREFF     CLEAR EFFECTIVE DATE                       
         MVC   SVAORPCT,=F'35000'    SET NEW PCT                                
*                                    (AND LEAVE BASIS AS IS)                    
*                                                                               
BFX      XIT1                                                                   
*                                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPB102WRK                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013SPB1BFHD  12/07/89'                                      
         END                                                                    
