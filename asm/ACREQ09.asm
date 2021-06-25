*          DATA SET ACREQ09    AT LEVEL 005 AS OF 02/28/12                      
*PHASE T60409A                                                                  
*INCLUDE RECUP                                                                  
*INCLUDE SQUASHER                                                               
         TITLE 'ACREQ09 - RPF MODULE'                                           
T60409   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX,T60409,RA,RR=R5                                             
         USING LWS,RC                                                           
         ST    R5,RELO                                                          
         USING GWS,R9              R9 = A(W/S)                                  
         L     R9,0(R1)                                                         
         USING ACQD,R8             R8 = A(REQUEST CARDS)                        
         LA    R8,RCARDS                                                        
         USING TWAD,R3             R3 = A(TWA)                                  
         L     R3,ASAVE                                                         
*                                                                               
         L     RF,=V(RECUP)                                                     
         A     RF,RELO                                                          
         ST    RF,VRECUP                                                        
         L     RF,=V(SQUASHER)                                                  
         A     RF,RELO                                                          
         ST    RF,VSQUASH                                                       
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'E3000A67',0                                      
         MVC   ARFPX,0(R1)                                                      
*                                                                               
         LA    R0,ACTTABN          LOCATE ROUTINE IN BRANCH TABLE               
         LA    R1,ACTTAB                                                        
         CLC   QRFPMODE,0(R1)                                                   
         BE    *+14                                                             
         LA    R1,L'ACTTAB(R1)                                                  
         BCT   R0,*-14                                                          
         DC    H'0'                INVALID MODE                                 
*                                                                               
         MVI   QRFPMODE,QRFPOK     DEFAULT STATUS = NO ERROR                    
         ICM   RF,15,1(R1)                                                      
         A     RF,RELO                                                          
         BASR  RE,RF               BRANCH TO ROUTINE                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*              INITIALIZE $RFP INTERFACE                              *         
***********************************************************************         
*                                                                               
INIT     NTR1                                                                   
         L     R0,AIORFP           CLEAR $RFP INTERFACE BLOCK                   
         SR    R1,R1                                                            
         ICM   R1,3,=Y(RFPBLKLN)                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING RFPBLK,R4                                                        
         L     R4,AIORFP                                                        
         MVI   RFPINIT,0           INITIALIZE RFP BLOCK                         
         MVC   RFPACOMF,ACOMFACS   - A(COMFACS)                                 
         MVC   RFPARECP,VRECUP     - A(RECUP)                                   
         MVC   RFPAMIN,AIOMINIO    - A(MINIO IO BUFFER)                         
         MVC   RFPAMINR,ARFPTAB    - A(RFP REQUEST TABLE)                       
         MVC   RFPMINRL,=Y(L'RFPTAB)                                            
         MVC   RFPFUID,TWAUSRID    - USER ID                                    
         MVC   RFPFAGY,TWAAGY      - AGENCY                                     
         MVI   RFPFSYS,C'A'        - ACCOUNTING SYSTEM                          
         GOTO1 ARFPX,DMCB,(R4)                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE GROUP                                         *         
***********************************************************************         
*                                                                               
GROUP    NTR1                                                                   
         USING RFPBLK,R4                                                        
         L     R4,AIORFP                                                        
         MVC   RFPFGRP,QRFPWORK    GROUP NAME                                   
         MVI   RFPMODE,RFPVALGP    VALIDATE GROUP                               
         OI    RFPFFLAG,RFPFSYMS   RETURN ALL SYMBOLIC EQUATES                  
         GOTO1 ARFPX,DMCB,(R4)                                                  
         CLI   RFPERROR,RFPNOERR                                                
         BE    *+8                                                              
         MVI   QRFPMODE,QRFPIGRP   INVALID GROUP                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE SYMBOL & RETURN CORRESPONDING ESCAPE SEQUENCE *         
***********************************************************************         
*                                                                               
SYMBOL   NTR1                                                                   
         USING RFPBLK,R4                                                        
         L     R4,AIORFP                                                        
*                                                                               
         ZIC   R0,RFPVNUMS         # OF SYMBOLS IN RFP TABLE                    
         CLC   QRFPWORK,RFPVSYMB                                                
         BE    SYMB100                                                          
         LA    R4,RFPVSYML(R4)                                                  
         BCT   R0,*-14                                                          
         MVI   QRFPMODE,QRFPISYM   INVALID SYMBOL NAME                          
         B     SYMBX                                                            
*                                                                               
SYMB100  XC    QRFPWORK,QRFPWORK                                                
         MVC   QRFPWORK(L'RFPVSYME),RFPVSYME                                    
*                                                                               
SYMBX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              ADD REQUEST TO GROUP                                   *         
***********************************************************************         
*                                                                               
REQADD   NTR1                                                                   
         USING RFPBLK,R4                                                        
         L     R4,AIORFP                                                        
         MVC   RFPFRQID,ACQPROG    REQUEST #                                    
         ZIC   R1,REQFLAG          (#REQCRDS-1) IN HIGH ORDER NIBBLE            
         SRA   R1,4                ISOLATE HIGH ORDER NIBBLE                    
         LA    R1,2(R1)            ADJUST COUNT & ADD 1 FOR HEADER              
         STC   R1,RFPFNUMR         #REQUEST CARDS + HEADER CARD                 
         MVI   RFPMODE,RFPADDRQ    ADD REQUEST                                  
*                                                                               
         LA    R0,REQREC                                                        
         MH    R1,=Y(L'RFPVREQC)                                                
         LR    RF,R1                                                            
         LA    RE,RFPVREQH                                                      
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 ARFPX,DMCB,(R4)                                                  
         CLI   RFPERROR,RFPNOERR                                                
         BE    REQADD2                                                          
         MVI   QRFPMODE,QRFPRADD                                                
         CLI   RFPERROR,RFPNOROO                                                
         BNE   EXIT                                                             
         MVI   QRFPMODE,QRFPNORO                                                
         B     EXIT                                                             
*                                                                               
REQADD2  MVC   BVRHDR,=CL60'Request     added to '                              
         EDIT  (B1,RFPFSEQN),(3,BVRHDR+8),ALIGN=LEFT,WRK=TEMP                   
         MVC   BVRHDR+21(L'RFPFGRP),RFPFGRP                                     
         GOTO1 VSQUASH,DMCB,BVRHDR,L'BVRHDR                                     
         MVC   FERN,=AL2(FE)                                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              INPUT BRANCH TABLE                                     *         
***********************************************************************         
*                                                                               
ACTTAB   DS    0XL5                                                             
         DC    AL1(QRFPINIT),AL4(INIT)        INITIALIZE $RFP INTERFACE         
         DC    AL1(QRFPGVAL),AL4(GROUP)       VALIDATE GROUP                    
         DC    AL1(QRFPSYMB),AL4(SYMBOL)      VALIDATE SYMBOL NAME              
         DC    AL1(QRFPRADD),AL4(REQADD)      ADD REQUEST TO GROUP              
ACTTABN  EQU   (*-ACTTAB)/L'ACTTAB                                              
         EJECT                                                                  
***********************************************************************         
*              LITERAL DECLARATIONS                                   *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              LOCAL WORKING STORAGE                                  *         
***********************************************************************         
*                                                                               
LWS      DSECT                                                                  
WRKBLCK  DS    0F                                                               
RELO     DS    F                                                                
VRECUP   DS    A                                                                
VSQUASH  DS    A                                                                
ARFPX    DS    A                                                                
LWSX     EQU   *-LWS                                                            
       ++INCLUDE GERFPIOD                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE ACREQWORK                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREQ09   02/28/12'                                      
         END                                                                    
