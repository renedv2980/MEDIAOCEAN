*          DATA SET DMDDSIO31  AT LEVEL 001 AS OF 01/15/13                      
*PHASE DDSIO31P                                                                 
*INCLUDE DMRCVUSS                                                               
*INCLUDE DMISGENQ                                                               
*INCLUDE DMSHMUSS                                                               
         TITLE 'DDSIO31 - 31 bit modules for offline applications'              
***********************************************************************         
* LOADED BY DMDMGRL SUBROUTINE LINKED INTO CALLERS PROGRAM            *         
* CALLED BY DMDMGRL SUBROUTINE WITH PARAMETERS IN R2,R3,R4            *         
*                                                                     *         
* R2 AL4(CALLERS UTL CSECT/ENTRY POINT)                               *         
*   IF HOB OF R2 = X'FF' USE R5 INSTEAD                               *         
* R3 AL4(CALLERS SSB CSECT/ENTRY POINT)                               *         
* R4 AL1 SUBROUTINE NUMBER AND AL3(CALLERS ADWAIT SOUBROUTINE)        *         
***********************************************************************         
         PRINT NOGEN                                                            
DDSIO31  CSECT                                                                  
DDSIO31  AMODE 31                  Set 31 bit addressing mode                   
DDSIO31  RMODE ANY                 Set any residency                            
         REQUS                                                                  
         ENTRY UTL                 V(UTL) FOR DATAMGR ROUTINES                  
*        ENTRY AMSOON              JOB IS A SOON JOB                            
         ENTRY SSB                 V(SSB) FOR DATAMGR ROUTINES                  
         ENTRY ADWAIT              V(ADWAIT) FOR DATAMGR ROUTINES               
         USING *,RF                                                             
         SAM31                                                                  
         ST    RE,SAVERE           SAVE CALLERS RETURN ADDRESS                  
SETUTL   LTR   R2,R2               COPY CALLERS UTL TO MY UTL                   
         BZ    SETUTL4                                                          
         CLM   R2,8,=X'FF'         TEST IF R2 SET BY CALLER                     
         BNE   SETUTL2             NO USE R2 AS NORMAL                          
         MVC   UTL,0(R5)           YES USE R5 INSTEAD OF R2                     
         NILH  GR2,X'00FF'         SO 31-BIT MODULES DON'T COMPLAIN             
         B     SETSSB                                                           
*                                                                               
SETUTL2  MVC   UTL,0(R2)                                                        
         B     SETSSB                                                           
*                                                                               
SETUTL4  MVC   UTL,DEFUTL          SET DEFAULT IF NO UTL PROVIDED               
*                                                                               
SETSSB   LTR   R3,R3               COPY CALLERS SSB TO MY SSB                   
         BZ    SETADW                                                           
         MVC   SSB,0(R3)                                                        
*                                                                               
SETADW   ST    R4,AADWAIT          SAVE A(ADWAIT) AND ROUTINE NUMBER            
         LR    RE,R4                                                            
         SRL   RE,22               HOB AS ROUTINE NUMBER, SRL THEN X4           
                                                                                
         L     RF,VDMGRMOD(RE)                                                  
         BASR  RE,RF                                                            
         DROP  RF                                                               
                                                                                
         USING *,RE                                                             
EXIT     IPM   RF                  SAVE CC                                      
         LTR   R3,R3               COPY BACK SSB                                
         BZ    EXITX                                                            
         MVC   0(L'SSB,R3),SSB                                                  
*                                                                               
EXITX    SPM   RF                  RESTORE CC                                   
         L     RE,SAVERE           RETURN TO CALLER                             
         BSM   0,RE                                                             
         DROP  RE                                                               
***********************************************************************         
* ADWAIT is for any included modules that need to call ADWAIT         *         
* This will in turn call the one which is linked with DMDMGRL         *         
***********************************************************************         
         USING *,RF                                                             
ADWAIT   LT    RF,AADWAIT          ADWAIT ENTRY                                 
         SAM24                                                                  
         BNZR  RF                  GO TO ADWAIT                                 
         LR    RF,RE               RETURN TO CALLER                             
         BSM   0,RF                WITH CORRECT ADDRESSING MODE                 
         EJECT                                                                  
*                                                                               
VDMGRMOD DS    0A                                                               
         DC    A(0)                ROUTINE NUM 00                               
         DC    A(0)                ROUTINE NUM 01                               
         DC    A(0)                ROUTINE NUM 02                               
         DC    A(0)                ROUTINE NUM 03                               
         DC    A(0)                ROUTINE NUM 04                               
         DC    A(0)                ROUTINE NUM 05                               
         DC    A(0)                ROUTINE NUM 06                               
         DC    A(0)                ROUTINE NUM 07                               
         DC    A(0)                ROUTINE NUM 08                               
         DC    A(0)                ROUTINE NUM 09                               
         DC    A(0)                ROUTINE NUM 10                               
         DC    A(0)                ROUTINE NUM 11                               
         DC    A(0)                ROUTINE NUM 12                               
         DC    A(0)                ROUTINE NUM 13                               
         DC    A(0)                ROUTINE NUM 14                               
         DC    A(0)                ROUTINE NUM 15                               
         DC    A(0)                ROUTINE NUM 16                               
         DC    V(DMRCVUSS)         ROUTINE NUM 17                               
         DC    V(DMISGENQ)         ROUTINE NUM 18                               
         DC    V(DMSHMUSS)         ROUTINE NUM 19                               
*                                                                               
SAVERE   DS    A                                                                
SVRER1   DS    4A                                                               
AADWAIT  DS    A                                                                
*                                                                               
         DC    C'***DDSIO31SSB***'                                              
SSB      DS    XL(SSOOFFX-SSBOFFD)'00'                                          
         ORG   SSB                                                              
         DC    XL2'0000',X'FF'                                                  
*&&UK*&& DC    AL1(SSOSNRCV)       NO OFFLINE RECOVERY                          
         ORG                                                                    
*                                                                               
         DC    C'***DDSIO31UTL***'                                              
UTL      DC    XL255'00'                                                        
*                                                                               
DEFUTL   DS    0X                                                               
         DC    XL255'00'                                                        
         EJECT ,                                                                
***********************************************************************         
* DDS DSECTS                                                                    
***********************************************************************         
*FASSBOFF                                                                       
SSBOFFD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DMDDSIO31 01/15/13'                                      
         END                                                                    
