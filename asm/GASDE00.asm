*          DATA SET GASDE00    AT LEVEL 004 AS OF 08/22/00                      
*PHASE TB1B00A                                                                  
         TITLE  'DAVID''S TEST'                                                 
SQUARE   CSECT                                                                  
         PRINT  NOGEN                                                           
         NMOD1  SDEWRKX-SDEWRK,GASDE00,RR=R2                                    
         USING  SDEWRK,RC          WORKSPACE BASE ADDRESS                       
         L      RA,4(R1)                                                        
         USING  TB1BFFD,RA         TWA BASE ADDRESS                             
         ST     R2,RELO            RELOCATION FACTOR                            
         EJECT                                                                  
         XC     SDEHEAD,SDEHEAD    CLEAR HEADER FIELD                           
*                                                                               
         CLI    SDEVALH+5,0        DID USER ENTER A VALUE?                      
         BNE    *+14               YES                                          
         MVC    SDEHEAD(19),=C'MISSING INPUT FIELD'                             
         B      GOODBYE                                                         
*                                                                               
         TM     SDEVALH+4,X'08'    IS VALUE NUMERIC?                            
         BO     *+14                                                            
         MVC    SDEHEAD(17),=C'VALUE NOT NUMERIC'                               
         B      GOODBYE                                                         
*                                                                               
         ZIC    R1,SDEVALH+5       INPUT LENGTH OF VALUE                        
         BCTR   R1,0                                                            
         EX     R1,*+8                                                          
         B      *+10                                                            
         PACK   DUB,SDEVAL(0)                                                   
         CVB    R1,DUB             R0 = BINARY VALUE                            
         STH    R1,HALF                                                         
*                                                                               
         MH     R1,HALF            RO = SQUARED VALUE                           
         EDIT   (R1),(8,SDERSLT),ZERO=NOBLANK,ALIGN=LEFT                        
         OI     SDERSLTH+6,X'80'   XMIT RESULT                                  
         MVC    SDEHEAD(16),=C'ACTION COMPLETED'                                
*                                                                               
GOODBYE  OI     SDEHEADH+6,X'80'   TRANSMIT MESSAGE FIELD                       
         OI     SDEVALH+6,X'40'    FORCE CURSOR TO VALUE FIELD                  
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*                        CONSTANTS                          *                   
*************************************************************                   
RELO     DS     F                     RELOCATION CONSTANT                       
         EJECT                                                                  
*************************************************************                   
*                        WORKSPACE                          *                   
*************************************************************                   
SDEWRK   DSECT                                                                  
DUB      DS     D                                                               
HALF     DS     H                                                               
WORK     DS     CL17                                                            
SDEWRKX  EQU    *                                                               
         EJECT                                                                  
*************************************************************                   
*                           TWA                             *                   
*************************************************************                   
       ++INCLUDE GASDEFFD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004GASDE00   08/22/00'                                      
         END                                                                    
