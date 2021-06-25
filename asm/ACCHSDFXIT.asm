*          DATA SET ACCHSDFXIT AT LEVEL 001 AS OF 08/20/15                      
*PROCESS USING(WARN(15))                                                        
*PHASE CHASXITA                                                                 
         TITLE 'CHASE CHECK FILE DFSORT EXIT'                                   
***********************************************************************         
*                                                                     *         
* THIS IS A DFSORT USER EXIT PROGRAM.                                 *         
* SEE IBM'S DFSORT: APPLICATION PROGRAMMING GUIDE FOR DETAILS.        *         
*                                                                     *         
* THIS EXIT IS INVOKED BY ICETOOL.                                    *         
* SEE 'DDS.ACC.PARMS(AC55CH*)'                                        *         
*                                                                     *         
***********************************************************************         
CHASXIT  CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ENTRY E15                 MUST BE "E15" (FOR DFSORT)                   
*                                                                               
         REQUS                                                                  
*                                                                               
         USING E15,RC              RC = PROGRAM BASE REGISTER                   
E15      SAVE  (14,12),,CHASXIT                                                 
         LA    RC,0(RF)            SET PROGRAM BASE REGISTER                    
         STMH  GR0,GRF,DFSORT_HIGH_HALVES                                       
         ST    RD,SAVE15+4         SAVE BACKWARD POINTER                        
         LA    RE,SAVE15           SET FORWARD POINTER...                       
         ST    RE,8(RD)            ...IN SAVE AREA                              
         LR    RD,RE               SET OUR SAVE AREA                            
         LR    R2,R1               SAVE PARAMETER LIST POINTER                  
*                                                                               
         L     R3,0(R2)            LOAD A(RECORD)                               
         LTR   R3,R3               EOF?                                         
         BZ    EOF                 YES                                          
*                                                                               
         USING JPMCHASE,R3                                                      
*                                                                               
         LA    R5,WORKREC          BUILD FORMATTED CHECK RECORD HERE            
WRK      USING CHECK_PORTION,R5                                                 
*                                                                               
         CLC   =C'ALL INVOICES PROCESSED FOR CHECK',0(R3)  TRAILER?             
         BE    PROCTLR             YES: RETURN THE MERGED RECORD                
*                                                                               
         OC    INV_CNTR,INV_CNTR   CHECK HEADER RECORD?                         
         BNZ   PROCDET             NO                                           
*                                                                               
* ===========================                                                   
* PROCESS CHECK HEADER RECORD                                                   
* ===========================                                                   
*                                                                               
         MVC   INV_CNTR,=F'1'      START THE INVOICE COUNTER                    
*                                                                               
* PUT THE COMMON CHECK FIELDS INTO THE WORK RECORD. NOTE: BECAUSE THE           
* INPUT RECORD IS BLANK-PADDED, THE WORK RECORD WILL BE ALSO.                   
*                                                                               
         LA    R0,WORKREC                                                       
         L     R1,=A(WORK_RECORD_LENGTH)                                        
         LR    RE,R3                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE               PUT HEADER FIELDS INTO WORK RECORD           
         JO    *+2                 DESTRUCTIVE MOVE!                            
         B     DELREC                                                           
*                                                                               
* =============================                                                 
* PROCESS INVOICE DETAIL RECORD                                                 
* =============================                                                 
*                                                                               
PROCDET  DS    0H                                                               
         L     R1,INV_CNTR         CURRENT INVOICE NUMBER                       
         BCTR  R1,0                ARRAY INDEX IS 0-BASED                       
         MHI   R1,L'ORDER_ITEM_DESCRIPTION_ARRAY                                
         LAY   R4,WRK.ORDER_ITEM_DESCRIPTION_ARRAY(R1)                          
         MVC   0(L'INVNUM,R4),INVNUM                                            
         MVI   L'INVNUM(R4),C'|'                                                
*                                                                               
         L     R1,INV_CNTR                                                      
         BCTR  R1,0                                                             
         MHI   R1,L'ORDER_ITEM_CONTROL_CODE_ARRAY                               
         LAY   R4,WRK.ORDER_ITEM_CONTROL_CODE_ARRAY(R1)                         
         MVC   0(4,R4),=C'Add|'                                                 
*                                                                               
         L     R1,INV_CNTR                                                      
         BCTR  R1,0                                                             
         MHI   R1,L'ORDER_ITEM_UNIT_PRICE_ARRAY                                 
         LAY   R4,WRK.ORDER_ITEM_UNIT_PRICE_ARRAY(R1)                           
         MVC   0(L'NETAMT,R4),NETAMT                                            
         MVI   L'NETAMT(R4),C'|'                                                
*                                                                               
         L     R1,INV_CNTR                                                      
         BCTR  R1,0                                                             
         MHI   R1,L'ORDER_ITEM_NUM_OF_UNITS_ARRAY                               
         LAY   R4,WRK.ORDER_ITEM_NUM_OF_UNITS_ARRAY(R1)                         
         MVC   0(2,R4),=C'1|'                                                   
*                                                                               
         L     R1,INV_CNTR                                                      
         BCTR  R1,0                                                             
         MHI   R1,L'ORDER_ITEM_UNIT_OF_MEASURE_ARRAY                            
         LAY   R4,WRK.ORDER_ITEM_UNIT_OF_MEASURE_ARRAY(R1)                      
         MVC   0(5,R4),=C'EACH|'                                                
*                                                                               
         L     R1,INV_CNTR         INCREMENT THE INVOICE NUMBER                 
         AHI   R1,1                                                             
         ST    R1,INV_CNTR                                                      
*                                                                               
         B     DELREC                                                           
         DROP  R3                                                               
*                                                                               
* ============================                                                  
* PROCESS CHECK TRAILER RECORD                                                  
* ============================                                                  
*                                                                               
PROCTLR  DS    0H                                                               
         XC    INV_CNTR,INV_CNTR   CLEAR INV. COUNTER (FOR NEXT CHECK)          
*                                                                               
         LAY   R4,WRK.ORDER_ITEM_CONTROL_CODE_ARRAY                             
         BCTR  R4,0                                                             
         CLI   0(R4),C'|'                                                       
         BNE   *-6                                                              
         MVI   0(R4),C' '          BLANK OUT LAST PIPE IN ARRAY                 
*                                                                               
         LAY   R4,WRK.ORDER_ITEM_UNIT_PRICE_ARRAY                               
         BCTR  R4,0                                                             
         CLI   0(R4),C'|'                                                       
         BNE   *-6                                                              
         MVI   0(R4),C' '          BLANK OUT LAST PIPE IN ARRAY                 
*                                                                               
         LAY   R4,WRK.ORDER_ITEM_NUM_OF_UNITS_ARRAY                             
         BCTR  R4,0                                                             
         CLI   0(R4),C'|'                                                       
         BNE   *-6                                                              
         MVI   0(R4),C' '          BLANK OUT LAST PIPE IN ARRAY                 
*                                                                               
         LAY   R4,WRK.ORDER_ITEM_UNIT_OF_MEASURE_ARRAY                          
         BCTR  R4,0                                                             
         CLI   0(R4),C'|'                                                       
         BNE   *-6                                                              
         MVI   0(R4),C' '          BLANK OUT LAST PIPE IN ARRAY                 
*                                                                               
         LAY   R4,WRK.ORDER_ITEM_ARRAY_END                                      
         BCTR  R4,0                                                             
         CLI   0(R4),C'|'                                                       
         BNE   *-6                                                              
         MVI   0(R4),C' '          BLANK OUT LAST PIPE IN ARRAY                 
*                                                                               
         LA    R3,WORKREC          THE ENTIRE WORK RECORD IS NOW BUILT          
         B     KEEPREC             PASS IT BACK TO DFSORT                       
*                                                                               
         DROP  WRK                                                              
         EJECT                                                                  
*                                                                               
* ================                                                              
* RETURN TO DFSORT                                                              
* ================                                                              
*                                                                               
KEEPREC  DS    0H                                                               
         SGR   GRF,GRF             SET RC=0: KEEP RECORD                        
         SGR   GR1,GR1                                                          
         LR    R1,R3               SET RECORD POINTER                           
         B     GOBACK                                                           
*                                                                               
DELREC   DS    0H                                                               
         LGHI  GRF,4               SET RC=4: DELETE RECORD                      
         B     GOBACK                                                           
*                                                                               
ADDREC   DS    0H                                                               
         LGHI  GRF,12              SET RC=12: INSERT RECORD                     
         SGR   GR1,GR1                                                          
         LA    R1,WORKREC          SET RECORD POINTER                           
         B     GOBACK                                                           
*                                                                               
ERROR    DS    0H                                                               
         LGHI  GRF,16              DFSORT WILL TERMINATE WITH RC=16             
         B     GOBACK                                                           
*                                                                               
EOF      DS    0H                                                               
         LGHI  GRF,8               SET RC=8:EOF                                 
*                                                                               
GOBACK   DS    0H                                                               
         LMH   GR0,GR0,DFSORT_HIGH_HALVES                                       
         LMH   GR2,GRE,DFSORT_HIGH_HALVES+8                                     
         L     RD,4(,RD)                                                        
         L     RE,12(,RD)                                                       
         LM    R2,RC,28(RD)        RESTORE REGS                                 
         BSM   0,RE                RETURN                                       
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         ORG   CHASXIT+(((*-CHASXIT)/256)+1)*256  FOR I-CACHE PIPELINE          
         SPACE 2                                                                
SAVE15   DS    18F                 SAVE DFSORT'S REGISTERS                      
DFSORT_HIGH_HALVES DS 16F                                                       
*                                                                               
INV_CNTR DC    F'0'                USED AS BOTH A FLAG AND ARRAY INDEX          
*                                                                               
         DS    D                                                                
         DC    CL16'****WORKREC*****'                                           
WORKREC  DS    (WORK_RECORD_LENGTH)C                                            
         DC    CL16'**END WORKREC***'                                           
*                                                                               
         EJECT                                                                  
       ++INCLUDE ACCHSDSECT                                                     
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACCHSDFXIT08/20/15'                                      
         END                                                                    
