*          DATA SET SPSFM6E    AT LEVEL 078 AS OF 10/05/06                      
*PHASE T2176EB                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21782A -- EQLEN RECORD DISPLAY                      *         
*                                                                     *         
*  COMMENTS:     DISPLAYS SPSLENTAB                                   *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SCSFM3E (MAINT)                               *         
*                                                                     *         
*  OUTPUTS:                                                           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T2176E - EQLEN DISPLAY'                                         
T2176E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**176E**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
*                                                                               
XIT      XIT1                                                                   
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       DS    0H                                                               
         LA    R2,DISMEDSH          R2 POINTER TO MEDIA FIELD HEADER            
         GOTO1 VALIMED              VALIDATE MEDIA                              
*                                                                               
         BAS   RE,CLRSCRN           CLEAR THE SCREEN                            
*                                                                               
         MVC   DMCB+4(4),=X'D9000A57'                                           
         GOTO1 CALLOV,DMCB,0       GET SPSLENTAB                                
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R1,DMCB             POINT TO START OF PHASE                      
         LH    RE,0(R1)            ENTRY LENGTH                                 
         LR    R5,RE                                                            
         L     RF,2(R1)            EOT DSPL                                     
         AR    RF,R1               RELOCATE EOT ADDRESS                         
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
         MVI   HALF,C'T'                                                        
         CLI   QMED,C'T'                                                        
         BE    GETEQU1                                                          
         CLI   QMED,C'N'                                                        
         BE    GETEQU1                                                          
         CLI   QMED,C'C'                                                        
         BE    GETEQU1                                                          
*                                                                               
         MVI   HALF,C'R'                                                        
         CLI   QMED,C'R'                                                        
         BE    GETEQU1                                                          
         CLI   QMED,C'X'                                                        
         BE    GETEQU1                                                          
         DC    H'0'                                                             
*                                                                               
GETEQU1  CLC   =C'00',0(R1)        TEST DEFAULT TABLE                           
         BE    GETEQU2                                                          
         CLC   0(2,R1),AGENCY      ELSE MATCH AGY                               
         BNE   *+14                                                             
GETEQU2  CLC   HALF(1),2(R1)       AND MEDIA                                    
         BE    GETEQU3                                                          
*                                                                               
         BXLE  R1,RE,GETEQU1                                                    
         DC    H'0'                                                             
*                                                                               
GETEQU3  AHI   R1,6                POINT BEYOND HEADER + "0" ENTRY              
         LA    R2,DISSLN1H         FIRST FIELD HEADER                           
         LA    R3,1                START AT SPOT LENGTH 1                       
         SHI   R5,6                SUBTRACT HEADER FROM ENTRY LENGTH            
         SRL   R5,1                DIVIDE ENTRY LENGTH BY 2                     
*                                                                               
GETEQU5  CLI   1(R1),0             SLN VALID?                                   
         BE    *+8                 NO                                           
         BAS   RE,DISPSLN          YES - DISPLAY IT                             
*                                                                               
         LA    R1,2(R1)            BUMP SPSLENTAB POINTER                       
         LA    R3,1(R3)            BUMP SPOT LENGTH COUNTER                     
         BCT   R5,GETEQU5                                                       
*                                                                               
VKX      B     XIT                                                              
***********************************************************************         
*                    DISPLAY EQU AND SPOT LENGTH                      *         
***********************************************************************         
DISPSLN  NTR1                                                                   
*                                                                               
         ZIC   R4,1(R1)                                                         
         EDIT  (R3),(3,8(R2)),0,ZERO=NOBLANK                                    
         OI    6(R2),X'80'          TRANSMIT                                    
         MVI   11(R2),C'='                                                      
         EDIT  (R4),(3,12(R2)),0,ZERO=NOBLANK,ALIGN=LEFT                        
         OI    6(R2),X'80'          TRANSMIT                                    
*                                                                               
         LA    R3,DISTAGH           LAST FIELD ON THE SCREEN                    
         ZIC   R5,0(R2)             BUMP TO NEXT PROTECTED FIELD                
         AR    R2,R5                                                            
         CR    R2,R3                IS END OF SCREEN?                           
         BE    ERRSPACE             YES                                         
         XIT1  REGS=(R2)            DO NOT RESTORE R2                           
***********************************************************************         
*                           CLRSCRN                                   *         
***********************************************************************         
CLRSCRN  NTR1                                                                   
         LA    R2,DISSLN1H          FIRST DATA ON SCREEN                        
         LA    R3,DISTAGH           CLEAR UP TO THIS FIELD                      
*                                                                               
CLR10    MVC   8(7,R2),=C'       '                                              
         OI    6(R2),X'80'          TRANSMIT                                    
         ZIC   R0,0(R2)             BUMP TO NEXT FIELD                          
         AR    R2,R0                                                            
         CR    R2,R3                IS END OF SCREEN?                           
         BL    CLR10                NO                                          
*                                                                               
         B     XIT                                                              
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
ERRSPACE MVC   ERRNUM,=AL2(1290)    ERROR - NEED TO MAKE SCREEN BIGGER          
         B     SPERREX                                                          
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
         MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
***********************************************************************         
*        DSECTS                                                       *         
***********************************************************************         
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*SPGENEQU   (EQUIVALENCY RECORDS)                                               
*SPSFMFFD                                                                       
*SCSFM3ED   (M SCREEN)                                                          
*DDGENTWA                                                                       
*FAGETTXTD  (ERROR MESSAGES)                                                    
*SPSFMWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM3ED          MAINTENACE SCREEN                            
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
         ORG   SYSSPARE                                                         
ERRNUM   DS    XL2                  FOR ERRORS                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'078SPSFM6E   10/05/06'                                      
         END                                                                    
