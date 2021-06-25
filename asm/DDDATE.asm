*          DATA SET DDDATE     AT LEVEL 001 AS OF 10/22/20                      
*PHASE DDDATEA                                                                  
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE PERVERT                                                                
         TITLE 'DDDATE - MODULE TO LINK TO DATE MODULES'                        
         MACRO                                                                  
&ENTRY   MODENTRY                                                               
         ENTRY &ENTRY                                                           
         USING *,RF                                                             
         USING MLFIXEDD,MLFIX                                                   
&ENTRY   STM   R1,R3,24(RD)                                                     
         ICM   RF,7,ML&ENTRY+1                                                  
         J     CALLMOD                                                          
         MEND                                                                   
***********************************************************************         
* LOADED BY DDMODLINK SUBROUTINE LINKED INTO CALLERS PROGRAM          *         
* CALLED BY DDMODLINK SUBROUTINE WITH PARMS IN R1 (DSECT DDMODLINKD)  *         
*                                                                     *         
* WARNING:                                                            *         
* THIS MODULE *DOES NOT* SAVE REGISTERS APART FROM R1 TO R3. IT       *         
* RELIES ON ITS CALLER'S SAVE (MODLINK)                              *          
***********************************************************************         
         PRINT NOGEN                                                            
DDDT24   CSECT                                                                  
         REQUS                                                                  
         ENTRY UTL                 V(UTL) FOR DATAMGR ROUTINES                  
         ENTRY SSB                 V(SSB) FOR DATAMGR ROUTINES                  
         ENTRY ADWAIT              V(ADWAIT) FOR DATAMGR ROUTINES               
                                                                                
         USING *,RF                                                             
         ST    RE,SAVERE           SAVE CALLERS RETURN ADDRESS                  
         J     *+12                SKIP OVER SENTINAL                           
         DC    C'*DDDATE*'                                                      
*                                                                               
         LR    R2,R1                                                            
         USING MLPARMS,R2                                                       
*                                                                               
         CLI   MLFIX,0             ALREADY SAVED FIXED MODLINK DATA?            
         JNE   SETADDRS            SKIP IF YES - NOT FIRST TIME IN              
*                                                                               
*        FIRST TIME ONLY CODE                                                   
*                                                                               
SETFIRST L     R3,MLAFIXED                                                      
         MVC   MLFIX,0(R3)         SAVE FIXED MODLINK DATA                      
         USING MLFIXEDD,MLFIX                                                   
*                                                                               
*        END OF FIRST TIME ONLY CODE                                            
*                                                                               
SETADDRS LT    R3,MLVUTL           TEST IF UTL PROVIDED                         
         JZ    SETADD10            NO USE DEFAULT                               
         MVC   UTL,0(R3)                                                        
         J     SETADD20                                                         
SETADD10 XC    UTL(255),UTL        SET DEFAULT IF NO UTL PROVIDED               
*                                                                               
SETADD20 LT    R3,MLVSSB           COPY CALLERS SSB TO MY SSB                   
         JZ    CALLDATE                                                         
         MVC   SSB,0(R3)                                                        
*                                                                               
CALLDATE L     R1,MLAPARM          CALLERS PARMS                                
         LLH   RE,MLNTRYNO                                                      
         BCTR  RE,0                                                             
         SLL   RE,2                RE=INDEX INTO ROUTINE TABLE                  
         L     RF,VDDDT24(RE)                                                   
         BASR  RE,RF                                                            
                                                                                
RETNDATE SAM24 ,                   MAY BE LEFT IN 31 BIT AND/OR AR MODE         
         SAC   0                                                                
         LARL  RF,DDDT24           RESET RF BASE                                
         IPM   RE                  SAVE CC                                      
         LT    R3,MLVSSB           COPY BACK SSB                                
         JZ    RETNDT10                                                         
         CLI   2(R3),X'FF'         ONLY IF EXTENDED SSB                         
         JNE   RETNDT10                                                         
         MVC   0(L'SSB,R3),SSB                                                  
*                                                                               
*        COPY BACK UTL? ARE WE SURE WE KNOW LENGTH?                             
*                                                                               
RETNDT10 SPM   RE                  RESTORE CC                                   
         L     RE,SAVERE           RETURN TO DDMODLINK                          
         BR    RE                                                               
         DROP  RF                                                               
                                                                                
         USING *,RF                                                             
         USING MLFIXEDD,MLFIX                                                   
ADWAIT   LT    RF,MLVADWT      ADWAIT ENTRY                                     
         BNZR  RF                                                               
         BR    RE                                                               
         DROP  RF                                                               
                                                                                
*        MODULES CALLED FROM WITHIN THIS LOAD MODULE BUT NOT LINKED IN          
*        THESE WILL ROUTE THROUGH MODLINK VIA ENTRY POINTS IN MLFIXEDD          
*                                                                               
DATAMGR  MODENTRY ,                                                             
LOCKSPC  MODENTRY ,                                                             
*                                                                               
         DROP  RF                                                               
CALLMOD  LARL  R1,MLFIX                                                         
         USING MLFIXEDD,R1                                                      
         LT    R2,MLVSSB                                                        
         JZ    CALMOD10                                                         
         CLI   2(R2),X'FF'         ONLY IF EXTENDED SSB                         
         JNE   CALMOD10                                                         
         LARL  R3,SSB                                                           
         MVC   0(255,R2),0(R3)                                                  
CALMOD10 LM    R1,R3,24(RD)                                                     
         BR    RF                                                               
         DROP  R1                                                               
*                                                                               
*        MODLINK MODULES IN THIS LOAD MODULE                                    
*                                                                               
VDDDT24  DC    (MLNTRYSN)A(0)                                                   
         ORG   VDDDT24+(4*(ADDAYQ-1))                                           
         DC    V(ADDAY)                                                         
         ORG   VDDDT24+(4*(GETDAYQ-1))                                          
         DC    V(GETDAY)                                                        
         ORG   VDDDT24+(4*(DATCONQ-1))                                          
         DC    V(DATCON)                                                        
         ORG   VDDDT24+(4*(DATCONXQ-1))                                         
         DC    V(DATCONX)                                                       
         ORG   VDDDT24+(4*(DATVALQ-1))                                          
         DC    V(DATVAL)                                                        
         ORG   VDDDT24+(4*(PERVALQ-1))                                          
         DC    V(PERVAL)                                                        
         ORG   VDDDT24+(4*(PERVERTQ-1))                                         
         DC    V(PERVERT)                                                       
         ORG   ,                                                                
*                                                                               
SAVERE   DS    A                   ALWAYS SAME ADDRESS IN MODLINK FOR           
*                                  ANY GIVEN CALL TO MODLINK                    
*                                                                               
         DS    0L                                                               
*                                                                               
         DC    C'DATESSB '                                                      
SSB      DC    XL(SSOOFFX-SSBOFFD)'00'                                          
         ORG   SSB                                                              
         DC    XL2'0000',X'FF'                                                  
*&&UK*&& DC    AL1(SSOSNRCV)       NO OFFLINE RECOVERY                          
         ORG                                                                    
*                                                                               
         DC    C'DATEUTL '                                                      
UTL      DC    XL255'00'                                                        
*                                                                               
         DS    0L                                                               
MLFIX    DC    XL(MLFIXEDL)'00'    SEE DDMODLINKD                               
         EJECT ,                                                                
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
                                                                                
MSG      DC    AL2(MSGEND-*-2)                                                  
MSGHDR   DC    CL15' '                                                          
         DC    C' '                                                             
MSGINFO  DC    CL10' '                                                          
         DC    C'.'                                                             
MSGEND   DS    0X                                                               
                                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* DSECTS                                                                        
***********************************************************************         
*                                                                               
MLFIXEDD DSECT                                                                  
       ++INCLUDE DDMODLINKD                                                     
*FACIDTABD                                                                      
         PRINT OFF                                                              
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
*DDMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*FASSBOFF                                                                       
SSBOFFD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
*DMDYNDDD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDYNDDD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DDDATE    10/22/20'                                      
         END                                                                    
