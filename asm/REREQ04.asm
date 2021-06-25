*          DATA SET REREQ04    AT LEVEL 124 AS OF 05/01/02                      
*PHASE T80704A,*                                                                
*INCLUDE SQUASHER                                                               
         TITLE 'T80704 - REREQ04 - REP REQUEST FILE PROGRAM ROUTINES'           
***********************************************************************         
*                                                                     *         
*  REREQ04 -- PHASE T80704 -- REP REQUEST FILE PROGRAM ROUTINES       *         
*                                                                     *         
*  18JUN93 (SKU) --- ORIGINAL ENTRY DATE                              *         
*                                                                     *         
*  04JAN95 (SKU) --- CHANGE TO SUPPORT > 2 REQUEST CARDS              *         
*                                                                     *         
*                    ***  END TOMBSTONE  ***                          *         
***********************************************************************         
         PRINT NOGEN                                                            
T80704   CSECT                                                                  
         NMOD1 0,T80704,R7,RR=R2                                                
*                                                                               
         LA    RC,2048(R7)                                                      
         LA    RC,2048(RC)                                                      
         USING T80704+8192,RC                                                   
*                                                                               
         L     R9,0(R1)            A(WORK AREA FROM CALLER)                     
         USING REQWRK,R9                                                        
*                                                                               
         ST    R2,RELO             SUB-OVERLAY RELOCATION FACTOR                
*                                                                               
         L     RA,ASAVE            A(TWA)                                       
         LA    R8,2048(RA)                                                      
         LA    R8,2048(R8)         SECOND TWA REQISTER                          
         USING TWAD,RA,R8                                                       
*                                                                               
         L     RF,=V(SQUASHER)                                                  
         A     RF,RELO                                                          
         ST    RF,VSQUASH                                                       
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'E3000A67',0                                      
         MVC   ARFPX,0(R1)                                                      
*                                                                               
         LA    RE,ROUTINES                                                      
MAIN100  CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                ACTION NOT PROCESSED BY THE PHASE            
         CLC   QRFPMODE,0(RE)                                                   
         BE    MAIN120                                                          
         LA    RE,4(RE)                                                         
         B     MAIN100                                                          
*                                                                               
MAIN120  EQU   *                   BRANCH TO PROCESS ROUTINE                    
         MVI   QRFPMODE,QRFPOK     DEFAULT STATUS = NO ERROR                    
         SR    RF,RF                                                            
         ICM   RF,7,1(RE)          A(RTN)                                       
         A     RF,RELO             RELOCATE                                     
         BASR  RE,RF               AND GO                                       
         SPACE 2                                                                
EXXIT    XIT1                      GENERIC EXIT                                 
         SPACE 2                                                                
ROUTINES DS    0F                                                               
         DC    AL1(QRFPINIT),AL3(INIT)        INIT RFP INTERFACE                
         DC    AL1(QRFPGVAL),AL3(GROUP)       VALIDATE GROUP                    
         DC    AL1(QRFPSYMB),AL3(SYMBOL)      VALIDATE SYMBOL NAME              
         DC    AL1(QRFPRADD),AL3(REQADD)      ADD REQUEST TO GROUP              
         DC    H'0'                EOT                                          
         EJECT                                                                  
**********************************************************************          
* RFP INITIZALIZATION                                                           
**********************************************************************          
INIT     NTR1                                                                   
         L     R0,AIORFP            CLEAR $RFP INTERFACE BLOCK                  
         SR    R1,R1                                                            
         ICM   R1,3,=Y(RFPBLTLN)   EXTENDED VERSION                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R4,AIORFP                                                        
         USING RFPBLK,R4                                                        
         MVI   RFPINIT,0           INITIALIZE RFP BLOCK                         
         MVC   RFPACOMF,ACOMFACS   - A(COMFACS)                                 
         MVC   RFPARECP,RECUP      - A(RECUP)                                   
         MVC   RFPAMIN,AIOMINIO    - A(MINIO IO BUFFER)                         
         MVC   RFPAMINR,ARFPTAB    - A(RFP REQUEST TABLE)                       
         MVC   RFPMINRL,=Y(L'RFPTAB)                                            
         MVC   RFPFUID,TWAUSRID    - USER ID                                    
         MVC   RFPFAGY,TWAAGY      - AGENCY                                     
         MVI   RFPFSYS,C'R'        - REP SYSTEM                                 
         OI    RFPFLAGS,RFPXSYMS   - USING RFPBLK EXTENSION                     
         GOTO1 ARFPX,DMCB,(R4)                                                  
         B     EXXIT                                                            
         EJECT                                                                  
**********************************************************************          
* RFP GROUP VALIDATION                                                          
**********************************************************************          
GROUP    NTR1                                                                   
         L     R4,AIORFP                                                        
         USING RFPBLK,R4                                                        
         MVC   RFPFGRP,QRFPWORK    GROUP NAME                                   
         MVI   RFPMODE,RFPVALGP    VALIDATE GROUP                               
         OI    RFPFFLAG,RFPFSYMS   RETURN ALL SYMBOLIC EQUATES                  
         GOTO1 ARFPX,DMCB,(R4)                                                  
         CLI   RFPERROR,RFPNOERR                                                
         BE    *+8                                                              
         MVI   QRFPMODE,QRFPIGRP   INVALID GROUP                                
         B     EXXIT                                                            
         EJECT                                                                  
**********************************************************************          
* RFP SYMBOL VALIDATION                                                         
**********************************************************************          
SYMBOL   NTR1                                                                   
         L     R4,AIORFP                                                        
         USING RFPBLK,R4                                                        
*                                                                               
         LH    R5,=Y(RFPXTNSN-RFPBLK)  SYMBOL TABLE POINTER                     
         LA    R5,RFPBLK(R5)                                                    
*                                                                               
         ZIC   R0,RFPVNUMS         # OF SYMBOLS IN RFP TABLE                    
*                                                                               
         CLC   QRFPWORK,RFPVSYMB-RFPVSYMS(R5)                                   
         BE    SYMB100                                                          
         LA    R5,RFPVSYML(R5)                                                  
         BCT   R0,*-14                                                          
*                                                                               
         MVI   QRFPMODE,QRFPISYM   INVALID SYMBOL NAME                          
         B     SYMBX                                                            
*                                                                               
SYMB100  XC    QRFPWORK,QRFPWORK                                                
         MVC   QRFPWORK(L'RFPVSYME),RFPVSYME-RFPVSYMS(R5)                       
*                                                                               
SYMBX    DS    0H                                                               
         B     EXXIT                                                            
         EJECT                                                                  
**********************************************************************          
* RFP REQUEST ADD                                                               
**********************************************************************          
REQADD   NTR1                                                                   
         L     R4,AIORFP                                                        
         USING RFPBLK,R4                                                        
         MVC   RFPFRQID,RNUM       REQUEST #                                    
         ZIC   R1,REQFLAG          (#REQCRDS-1) IN HIGH ORDER NIBBLE            
         SRA   R1,4                ISOLATE HIGH ORDER NIBBLE                    
         LA    R1,2(R1)            ADJUST COUNT & ADD 1 FOR HEADER              
         STC   R1,RFPFNUMR         #REQUEST CARDS + HEADER CARD                 
                                                                                
         LA    R0,RFPVREQH         MOVE SPECIFIED NUMBER OF CARDS               
         LA    RE,REQREC                                                        
         MH    R1,=H'80'           EACH CARD IS 80 IN LENGTH                    
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVI   RFPMODE,RFPADDRQ    ADD REQUEST                                  
         GOTO1 ARFPX,DMCB,(R4)                                                  
         CLI   RFPERROR,RFPNOERR                                                
         BE    REQADD2                                                          
         MVI   QRFPMODE,QRFPRADD                                                
         CLI   RFPERROR,RFPNOROO                                                
         BNE   REQADDX                                                          
         MVI   QRFPMODE,QRFPNORO                                                
         B     REQADDX                                                          
*                                                                               
REQADD2  MVC   RQSMSG,=CL60'REQUEST     ADDED TO '                              
         EDIT  (B1,RFPFSEQN),(3,RQSMSG+8),ALIGN=LEFT                            
         MVC   RQSMSG+21(L'RFPFGRP),RFPFGRP                                     
         GOTO1 VSQUASH,DMCB,RQSMSG,L'RQSMSG                                     
*                                                                               
REQADDX  DS    0H                                                               
         B     EXXIT                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         TITLE 'WORK AREA'                                                      
*                                                                               
*- LOCAL WORK AREA                                                              
*                                                                               
VSQUASH  DS    A                                                                
ARFPX    DS    A                                                                
*                                                                               
*- REQTWA AND REQWRK                                                            
*        PRINT OFF                                                              
       ++INCLUDE REREQTWA                                                       
       ++INCLUDE REREQWRK                                                       
       ++INCLUDE GERFPIOD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'124REREQ04   05/01/02'                                      
         END                                                                    
