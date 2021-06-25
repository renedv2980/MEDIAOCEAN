*          DATA SET GELNK13    AT LEVEL 002 AS OF 11/29/12                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 041173.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE TA0613A                                                                  
GELNK13  TITLE '- CFM client/product download'                                  
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=US,REQUEST=*,SERVERLIST=SERVERS,WORKERKEY=CFMC,    +        
               SEGMENT=Y,SYSTEM=CTLSYSQ                                         
                                                                                
SERVERS  DC    AL1(TSTSTEW,TSTSPOT,TSTADBY,TSTAPAL,0)                           
                                                                                
REQCFMC  LKMAP H,I#CFMCDL,NEWREC=Y                                              
                                                                                
System   LKMAP F,01,CHAR,(*,SYS1LIT),OLEN=1,ARRAY=S                             
Agency   LKMAP F,02,CHAR,(*,AGY1LIT),OLEN=2                                     
LimAccs  LKMAP F,03,HEXD,(*,LIMALIT),OLEN=4                                     
UserId#  LKMAP F,04,HEXD,(*,USIDLIT),OLEN=2                                     
PID#     LKMAP F,05,HEXD,(*,PIDNLIT),OLEN=2                                     
AccsGrp  LKMAP F,06,HEXD,(*,AGRPLIT),OLEN=2                                     
PerAgy   LKMAP F,07,CHAR,(*,PAGYLIT),OLEN=2                                     
SecSys   LKMAP F,08,HEXD,(*,SSYSLIT),OLEN=1                                     
SecPrg   LKMAP F,09,HEXD,(*,SPRGLIT),OLEN=1,ARRAY=E                             
                                                                                
System   LKMAP F,10,CHAR,(*,SYS2LIT),OLEN=1,ARRAY=S                             
Agency   LKMAP F,11,CHAR,(*,AGY2LIT),OLEN=2                                     
Media    LKMAP F,12,CHAR,(*,MEDCLIT),OLEN=1                                     
Client   LKMAP F,13,CHAR,(*,CLTCLIT),OLEN=5                                     
Product  LKMAP F,14,CHAR,(*,PRDCLIT),OLEN=5                                     
PCToken  LKMAP F,30,CHAR,(*,TOKNLIT),OLEN=8,ARRAY=E                             
                                                                                
Option01 LKMAP F,40,CHAR,(*,OPT1LIT),OLEN=1                                     
Option02 LKMAP F,41,CHAR,(*,OPT2LIT),OLEN=1                                     
Option03 LKMAP F,42,CHAR,(*,OPT3LIT),OLEN=1                                     
Option04 LKMAP F,43,CHAR,(*,OPT4LIT),OLEN=1                                     
Option05 LKMAP F,44,CHAR,(*,OPT5LIT),OLEN=1                                     
Option06 LKMAP F,45,CHAR,(*,OPT6LIT),OLEN=1                                     
Option07 LKMAP F,46,CHAR,(*,OPT7LIT),OLEN=1                                     
Option08 LKMAP F,47,CHAR,(*,OPT8LIT),OLEN=1                                     
Option09 LKMAP F,48,CHAR,(*,OPT9LIT),OLEN=1                                     
Option10 LKMAP F,49,CHAR,(*,OPTALIT),OLEN=1                                     
Option11 LKMAP F,50,CHAR,(*,OPTBLIT),OLEN=1                                     
Option12 LKMAP F,51,CHAR,(*,OPTCLIT),OLEN=1                                     
Option13 LKMAP F,52,CHAR,(*,OPTDLIT),OLEN=1                                     
Option14 LKMAP F,53,CHAR,(*,OPTELIT),OLEN=1                                     
Option15 LKMAP F,54,CHAR,(*,OPTFLIT),OLEN=1                                     
Option16 LKMAP F,55,CHAR,(*,OPTGLIT),OLEN=1                                     
                                                                                
         LKMAP E                                                                
                                                                                
         LKMAP X                                                                
                                                                                
SYS1LIT  DC    C'Agency system'                                                 
AGY1LIT  DC    C'       agency alpha id'                                        
LIMALIT  DC    C'       limit access'                                           
USIDLIT  DC    C'       user id#'                                               
PIDNLIT  DC    C'       PID#'                                                   
AGRPLIT  DC    C'       access group#'                                          
PAGYLIT  DC    C'       person agency'                                          
SSYSLIT  DC    C'       security system#'                                       
SPRGLIT  DC    C'       security program#'                                      
SYS2LIT  DC    C'Client system'                                                 
AGY2LIT  DC    C'       agency alpha id'                                        
MEDCLIT  DC    C'       media code'                                             
CLTCLIT  DC    C'       client code'                                            
PRDCLIT  DC    C'       product code'                                           
TOKNLIT  DC    C'       PC request token'                                       
OPT1LIT  DC    C'Download option  1'                                            
OPT2LIT  DC    C'Download option  2'                                            
OPT3LIT  DC    C'Download option  3'                                            
OPT4LIT  DC    C'Download option  4'                                            
OPT5LIT  DC    C'Download option  5'                                            
OPT6LIT  DC    C'Download option  6'                                            
OPT7LIT  DC    C'Download option  7'                                            
OPT8LIT  DC    C'Download option  8'                                            
OPT9LIT  DC    C'Download option  9'                                            
OPTALIT  DC    C'Download option 10'                                            
OPTBLIT  DC    C'Download option 11'                                            
OPTCLIT  DC    C'Download option 12'                                            
OPTDLIT  DC    C'Download option 13'                                            
OPTELIT  DC    C'Download option 14'                                            
OPTFLIT  DC    C'Download option 15'                                            
OPTGLIT  DC    C'Download option 16'                                            
                                                                                
* GELNKWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE GELNKWRK                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002GELNK13   11/29/12'                                      
         END                                                                    
