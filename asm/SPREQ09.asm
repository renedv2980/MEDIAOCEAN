*          DATA SET SPREQ09    AT LEVEL 010 AS OF 05/01/02                      
*PHASE T20809,+0                                                                
*INCLUDE RECUP                                                                  
*INCLUDE SQUASHER                                                               
         TITLE 'SPREQ09 - REQUEST - $RFP INIT MODULE'                           
T20809   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX,T20809,RR=R9                                               
*                                                                               
         USING WORKD,RC                                                         
         ST    R9,RELO                                                          
         L     R9,0(R1)                                                         
         USING REQTEMP,R9                    R9=A(W/S)                          
         L     R3,ASAVE                                                         
         USING TWAD,R3                    R3=A(TWA)                             
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
         MVI   RFPFSYS,C'S'        - SPOT SYSTEM                                
* - ARE WE SPOT OR NET                                                          
         L     RF,APARM                                                         
         L     RF,16(RF)           GET A(COMFACS)                               
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0)                                              
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3           2=SPOT,3=NET                                 
         BNE   *+8                                                              
         MVI   RFPFSYS,C'N'                                                     
*                                                                               
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
         LR    R2,R3                                                            
         USING T208FFD,R2                                                       
         L     R4,AIORFP                                                        
         MVC   RFPFRQID,RNUM       REQUEST #                                    
         ZIC   R1,RHDR+15          (#REQCRDS-1) IN HIGH ORDER NIBBLE            
         SRA   R1,4                ISOLATE HIGH ORDER NIBBLE                    
         LA    R1,2(R1)            ADJUST COUNT & ADD 1 FOR HEADER              
         STC   R1,RFPFNUMR         #REQUEST CARDS + HEADER CARD                 
         MVI   RFPMODE,RFPADDRQ    ADD REQUEST                                  
         MVC   RFPVREQH(L'REQREC),REQREC                                        
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
         DROP  R2                                                               
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
WORKD    DSECT                                                                  
WRKBLCK  DS    0F                                                               
RELO     DS    F                                                                
VRECUP   DS    A                                                                
VSQUASH  DS    A                                                                
ARFPX    DS    A                                                                
WORKX    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
*              ++INCLUDES                                             *         
***********************************************************************         
*                                                                               
         PRINT ON                                                               
       ++INCLUDE GERFPIOD                                                       
       ++INCLUDE SPREQSAVE                                                      
       ++INCLUDE SPREQTEMP                                                      
       ++INCLUDE SPREQFFBD                                                      
       ++INCLUDE FLDIND                                                         
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASSB                                                          
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDSPOOK                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPREQ09   05/01/02'                                      
         END                                                                    
