*          DATA SET DMDDSIOS   AT LEVEL 020 AS OF 05/01/02                      
*PHASE DDSIOT                                                                   
*INCLUDE DMDMGRA        <=== MERGED DEMDIRM                                     
*INCLUDE DMDTFS                                                                 
*INCLUDE DMDADDS                                                                
*INCLUDE DMISDDS                                                                
*INCLUDE DMDDNAME                                                               
*INCLUDE DMDYNDD                                                                
*INCLUDE DMDALINK                                                               
*INCLUDE DMDAPTRS                                                               
*INCLUDE DMPRTQ                                                                 
*INCLUDE DMPRTQO                                                                
*INCLUDE DMRCVR                                                                 
*INCLUDE DMWRKFA        <==NEW 16BIT                                            
*INCLUDE DMWRKR                                                                 
*INCLUDE DMACCEMU                                                               
*INCLUDE DMDABUFF                                                               
*INCLUDE DMDANDX                                                                
*INCLUDE DMENQDEQ                                                               
*&&US                                                                           
*INCLUDE DMENQCTL                                                               
*&&                                                                             
*INCLUDE LOCKSPC                                                                
*INCLUDE LOCKUP                                                                 
*INCLUDE DYNALLOC                                                               
*INCLUDE GETRET                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE ARREDIT                                                                
*INCLUDE DMPQGCI                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'DDSIO - MODULE TO LINK TO INDEPENDENT DATA MANAGER'             
***********************************************************************         
*LOADED BY DMDMGRL SUBROUTINE LINKED INTO CALLERS PROGRAM             *         
*CALLED BY DMDMGRL SUBROUTINE WITH PARAMETERS IN R2,R3,R4             *         
*                                                                     *         
*R2 AL4(CALLERS UTL CSECT/ENTRY POINT)                                *         
*   IF HOB OF R2 = X'FF' USE R5 INSTEAD                                         
*R3 AL4(CALLERS SSB CSECT/ENTRY POINT)                                *         
*R4 AL1 SUBROUTINE NUMBER AND AL3(CALLERS ADWAIT SOUBROUTINE)         *         
***********************************************************************         
         SPACE 1                                                                
         PRINT NOGEN                                                            
DDSIO    CSECT                                                                  
         REQUS                                                                  
         SPACE 1                                                                
         ENTRY UTL                 V(UTL) FOR DATAMGR ROUTINES                  
         ENTRY XPEDITER            RUNNING UNDER XPEDITER                       
         ENTRY AMSOON              JOB IS A SOON JOB                            
         ENTRY SSB                 V(SSB) FOR DATAMGR ROUTINES                  
         ENTRY ADWAIT              V(ADWAIT) FOR DATAMGR ROUTINES               
         SPACE 1                                                                
         USING *,RF                                                             
         ST    RE,SAVERE           SAVE CALLERS RETURN ADDRESS                  
         B     SETUTL                                                           
         DC    C'DDSIODDS'                                                      
         SPACE 1                                                                
SETUTL   LTR   R2,R2               COPY CALLERS UTL TO MY UTL                   
         BZ    SETUTL2                                                          
         CLM   R2,8,=X'FF'         TEST IF R2 SET BY CALLER                     
         BNE   *+18                NO USE R2 AS NORMAL                          
         MVC   UTL,0(R5)           YES USE R5 INSTEAD OF R2                     
         ICM   R2,8,=X'00'         SO 31-BIT MODULES DON'T COMPLAIN             
         B     SETSSB                                                           
         MVC   UTL,0(R2)                                                        
         B     SETSSB                                                           
SETUTL2  MVC   UTL,DEFUTL          SET DEFAULT IF NO UTL PROVIDED               
         SPACE 1                                                                
SETSSB   LTR   R3,R3               COPY CALLERS SSB TO MY SSB                   
         BZ    SETADW                                                           
*********TM    5(R3),X'20'         TEST SSOFLAG1,SSOFXCPY                       
*********BO    SETADW              NO MVC IF XCOPY IS SET                       
         MVC   SSB,0(R3)                                                        
         B     SETADW                                                           
*                                                                               
SETADW   ST    R4,AADWAIT          SAVE A(ADWAIT) AND ROUTINE NUMBER            
*                                                                               
SETMST   CLM   R7,15,=CL4'ABEA'    SPECIAL XPEDITER CALL FOR LOCKSPC            
         BNE   SETMST1             NO - NOT ANNIES FAULT THEN                   
         LTR   R6,R6               YOU HAD BETTER HAVE A MASTC - IDIOT          
         BZ    SETMST1                                                          
         USING MASTD,R6                                                         
         MVC   XPEDITER,MCXPDTR    SET XPEDITER STATUS                          
         MVI   AMSOON,C'N'                                                      
         OC    MCREMPQK,MCREMPQK   TEST SOON JOB OR OVERNIGHT                   
         BZ    *+8                                                              
         MVI   AMSOON,C'Y'                                                      
         TM    MCRFLAG1,MCRPROF    OR PROFILES FROM DISK SPECIFIED              
         BZ    *+8                                                              
         MVI   AMSOON,C'Y'                                                      
         MVI   AMSOON,C'N'         ONLY ONE LPAR NOW: NO PROFILE=DISK           
         DROP  R6                                                               
*                                                                               
SETMST1  SLR   RE,RE                                                            
         IC    RE,AADWAIT                                                       
         SLL   RE,2                RE=INDEX INTO ROUTINE TABLE                  
         SPACE 1                                                                
SETADR   MVI   AADWAIT,0           PASS CONTROL TO DATAMGR MODULE               
         L     RF,VDMGRMOD(RE)                                                  
         BASR  RE,RF                                                            
         DROP  RF                                                               
         SPACE 1                                                                
         USING *,RE                                                             
EXIT     IPM   RF                  SAVE CC                                      
         LTR   R3,R3               COPY BACK SSB                                
         BZ    EXITX                                                            
         CLI   2(R3),X'FF'         ONLY IF EXTENDED SSB                         
         BNE   EXITX                                                            
         MVC   0(L'SSB,R3),SSB                                                  
*                                                                               
EXITX    SPM   RF                  RESTORE CC                                   
         L     RE,SAVERE           RETURN TO CALLER                             
         BR    RE                                                               
         DROP  RE                                                               
         SPACE 1                                                                
         USING *,RF                                                             
ADWAIT   L     RF,AADWAIT          ADWAIT ENTRY                                 
         LTR   RF,RF                                                            
         BNZR  RF                                                               
         LR    RF,RE                                                            
         BR    RF                                                               
         EJECT                                                                  
VDMGRMOD DS    0A                                                               
         DC    V(DATAMGR)          ROUTINE NUM 00                               
         DC    V(DADDS)            ROUTINE NUM 01                               
         DC    V(ISDDS)            ROUTINE NUM 02                               
         DC    V(DMDANDX)          ROUTINE NUM 03                               
         DC    V(DYNALLOC)         ROUTINE NUM 04                               
         DC    V(WORKER)           ROUTINE NUM 05                               
         DC    V(DMDABUFF)         ROUTINE NUM 06                               
         DC    V(DMDALINK)         ROUTINE NUM 07                               
         DC    V(DMDAPTRS)         ROUTINE NUM 08                               
         DC    V(PQOPEN)           ROUTINE NUM 09                               
         DC    V(DMENQDEQ)         ROUTINE NUM 10                               
         DC    V(DMOD000)          ROUTINE NUM 11                               
         DC    V(DMACCEMU)         ROUTINE NUM 12                               
         DC    V(LOCKSPC)          ROUTINE NUM 13                               
         DC    V(DMDDNAME)         ROUTINE NUM 14                               
         DC    V(LOCKUP)           ROUTINE NUM 15                               
         SPACE 1                                                                
SAVERE   DS    A                                                                
AADWAIT  DS    A                                                                
         SPACE 1                                                                
         DS    0L                                                               
         DC    C'XPEDITER'                                                      
XPEDITER DC    C'N'                                                             
         DC    XL7'00'                                                          
         DC    C'AMSOON**'                                                      
AMSOON   DC    C'N'                                                             
         DC    XL7'00'                                                          
*                                                                               
         DC    C'DDSIOSSB'                                                      
SSB      DS    0XL256                                                           
         DC    XL2'0000',X'FF'                                                  
*&&US*&& DC    X'00'               OFFLINE RECOVERY REQUIRED                    
*&&UK*&& DC    X'02'               NO OFFLINE RECOVERY                          
         DC    XL252'00'                                                        
*                                                                               
         DC    C'DDSIOUTL'                                                      
UTL      DC    XL08'00'                                                         
*                                                                               
DEFUTL   DS    0X                                                               
         DS    XL08'00'                                                         
         SPACE 1                                                                
*DDMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020DMDDSIOS  05/01/02'                                      
         END                                                                    
