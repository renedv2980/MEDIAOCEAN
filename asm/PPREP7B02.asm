*          DATA SET PPREP7B02  AT LEVEL 035 AS OF 05/01/02                      
*PHASE PP7B02C,+0                  **** NOTE "C" PHASE                          
         TITLE 'PP7B02 -   PRINTPAK SHIPPING LABEL PRINT'                       
         PRINT NOGEN                                                            
PP79B    CSECT                                                                  
         NMOD1 0,PP7B02,RR=R9                                                   
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R8,SPACEND                                                       
         USING PP79BWK,R8                                                       
         CLI   MODE,PROCREQ                                                     
         BE    LABELS                                                           
EXIT     XIT1                                                                   
*                                                                               
LABELS   MVC   LBLNO,=A(LBLMAX)                                                 
         MVC   DUB+00(2),RCDATE+6                                               
         MVC   DUB+02(2),RCDATE+0                                               
         MVC   DUB+04(2),RCDATE+3                                               
         MVC   MYTODAY,DUB                                                      
         PACK  FULL(2),MYTODAY+4(3)                                             
         MVC   WRKDAY,FULL                                                      
         MVI   MAXLINES,99                                                      
         XC    FILID,FILID                                                      
         L     RF,=A(WRKBUFF)                                                   
         ST    RF,PP79FLN                                                       
LB1      DS    0H                                                               
LB2      DS    0H                                                               
         GOTO1 WORKER,DMCB,=C'INDEX',PP79FLN,FILID                              
*                                                                               
         TM    DMCB+8,X'80'                                                     
         BNZ   LB12                                                             
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R4,FILID                                                         
         USING UKRECD,R4                                                        
         CLC   UKSYSPRG,=C'P79'                                                 
         BNE   LB2                                                              
         CLC   UKDAY,WRKDAY                                                     
         BNE   LB2                                                              
*                                                                               
         DROP  R4                                                               
*                                                                               
LB4      DS    0H                                                               
LB8      DS    0H                                                               
         GOTO1 WORKER,DMCB,=C'READ',PP79FLN,,REC-4                              
         TM    DMCB+8,X'80'                                                     
         BNZ   LB10                                                             
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   REC,C'A'                                                         
         BE    ADDR                                                             
         CLI   REC,C'R'                                                         
         BE    RETR                                                             
         CLI   REC,C'L'                                                         
         BE    LOGR                                                             
         DC    H'0'                                                             
LB10     DS    0H                                                               
         MVC   LBLNO,=A(LBLMAX)                                                 
         BAS   RE,PRINT                                                         
         DS    0H                                                               
         GOTO1 WORKER,DMCB,=C'PURGE',PP79FLN,FILID                              
         B     LB2                                                              
*                                                                               
LB12     B     EXIT                                                             
         SPACE 3                                                                
LOGR     DS    0H                                                               
         L     R7,LOGOC                                                         
         USING LOGOD,R7                                                         
         MVC   LOGO1(121),REC+1                                                 
         MVI   LOGOTYPE,C'S'                                                    
         ZAP   LOGOREQS,=P'0'                                                   
         MVC   LOGOINFO,REC+1+LOGOINFO-LOGO1                                    
         ZAP   LOGOLINE,=P'68'                                                  
         MVC   LOGOPAGE,=F'1'                                                   
         GOTO1 LOGO,DMCB,(R7)                                                   
         MVI   LBL1,C'X'                                                        
         LA    R1,LBLLEN*LBLMAX-1                                               
         MOVE  (LBL1+1,(R1)),LBL1                                               
         LA    R3,1                                                             
LOGR2    DS    0H                                                               
         ST    R3,LBLNO                                                         
         BAS   RE,PRINT                                                         
         LA    R3,1(R3)                                                         
         C     R3,=A(LBLMAX)                                                    
         BNH   LOGR2                                                            
         MVI   LBL1,C' '                                                        
         LA    R1,LBLLEN*LBLMAX-1                                               
         MOVE  (LBL1+1,(R1)),LBL1                                               
         MVC   LBLNO,=F'1'                                                      
         B     LB8                                                              
         SPACE 3                                                                
RETR     DS    0H                                                               
         LA    R1,L'SLRREC                                                      
         MOVE  (SLRREC,(R1)),REC                                                
         B     LB8                                                              
         SPACE 2                                                                
ADDR     DS    0H                                                               
         MVC   SLAREC,REC                                                       
         ZAP   DUB,SLACOUNT                                                     
         CVB   R0,DUB                                                           
         ST    R0,PCOUNT                                                        
ADDR2    DS    0H                                                               
         L     RF,LBLNO                                                         
         BCTR  RF,R0                                                            
         MH    RF,=Y(LBLLEN)                                                    
         L     R9,=A(LBL1)                                                      
         AR    R9,RF                                                            
         USING LBLD,R9                                                          
*                                                                               
         MVC   LBLRNAM,SLRNAM                                                   
         MVC   LBLRLIN1,SLRLIN1                                                 
         MVC   LBLRLIN2,SLRLIN2                                                 
*                                                                               
         MVC   LBLSHP1,=C'----'                                                 
         MVC   LBLSHP2,=C'SHIP'                                                 
         MVC   LBLSHP3,=C' TO-'                                                 
         MVC   LBLSHP4,=C'----'                                                 
*                                                                               
         MVC   LBLCOM1,SLRCOM1                                                  
         MVC   LBLCOM2,SLRCOM2                                                  
         MVC   LBLCOM3,SLRCOM3                                                  
*                                                                               
         MVC   SLAREC,REC                                                       
*                                                                               
         MVC   LBLANAM,SLANAM                                                   
         MVC   LBLALIN1,SLALIN1                                                 
         MVC   LBLALIN2,SLALIN2                                                 
         MVC   LBLALIN3,SLALIN3                                                 
*                                                                               
         BAS   RE,PRINT                                                         
         CLC   PCOUNT,=F'10'                                                    
         BNH   LB8                                                              
         L     RF,PCOUNT                                                        
         SH    RF,=H'10'                                                        
         ST    RF,PCOUNT                                                        
         B     ADDR2                                                            
         SPACE 3                                                                
PRINT    NTR1                                                                   
         CLC   LBLNO,=A(LBLMAX)                                                 
         BE    PRT2                                                             
         L     RF,LBLNO                                                         
         LA    RF,1(RF)                                                         
         ST    RF,LBLNO                                                         
         B     PRTX                                                             
PRT2     DS    0H                                                               
         LA    R2,LBLLINS                                                       
         SR    R6,R6                                                            
PRT4     DS    0H                                                               
         LA    R3,LBLMAX                                                        
         LA    R4,LBLPOS1                                                       
         LA    R5,LBL1(R6)                                                      
PRT6     DS    0H                                                               
         LA    R7,P+1                                                           
         A     R7,0(R4)                                                         
         MVC   0(LBLCOLS,R7),0(R5)                                              
         LA    R5,LBLLEN(R5)                                                    
         LA    R4,4(R4)                                                         
         BCT   R3,PRT6                                                          
*                                                                               
         MVI   LINE,1              SO I'LL NEVER SKIP TO NEW PAGE               
         GOTO1 REPORT                                                           
         LA    R6,LBLCOLS(R6)                                                   
         BCT   R2,PRT4                                                          
*                                                                               
         MVI   LBL1,C' '                                                        
         LA    R1,LBLLEN*LBLMAX-1                                               
         MOVE  (LBL1+1,(R1)),LBL1                                               
         MVC   LBLNO,=F'1'                                                      
*                                                                               
PRTX     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
UTL      DS    0D                                                               
         DC    10F'0'                                                           
*                                                                               
SSB      DS    0D                                                               
         DC    10F'0'                                                           
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
LBLPOS1  DC    F'0'                                                             
LBLPOS2  DC    F'41'                                                            
LBL1     DS    16CL40                                                           
LBL2     DS    16CL40                                                           
LBLMAX   EQU   2                                                                
LBLLINS  EQU   16                                                               
LBLCOLS  EQU   40                                                               
LBLLEN   EQU   LBLLINS*LBLCOLS                                                  
*                                                                               
WRKBUFF  DS    0D                                                               
         DS    4096X                                                            
REGSAVE  DS    12000C                                                           
*                                                                               
         EJECT                                                                  
PP79BWK  DSECT                                                                  
PP79FLN  DS    A                                                                
MYTODAY  DS    CL6                                                              
LBLNO    DS    F                                                                
PCOUNT   DS    F                                                                
WRKDAY   DS    CL1                                                              
*                                                                               
FILID    DS    CL16                                                             
         DS    F                                                                
REC      DS    CL400                                                            
*                                                                               
         SPACE 3                                                                
*                                                                               
       ++INCLUDE PP79BRECS                                                      
*                                                                               
*                                                                               
         EJECT                                                                  
LBLD     DSECT                                                                  
         DS    CL40                01                                           
         DS    CL2                 02                                           
LBLRNAM  DS    CL30                                                             
         DS    CL8                                                              
         DS    CL2                 03                                           
LBLRLIN1 DS    CL30                                                             
         DS    CL8                                                              
         DS    CL2                 04                                           
LBLRLIN2 DS    CL30                                                             
         DS    CL8                                                              
         DS    CL40                05                                           
         DS    CL40                06                                           
         DS    CL2                 07                                           
LBLSHP1  DS    CL4                                                              
         DS    CL2                                                              
LBLANAM  DS    CL30                                                             
         DS    CL2                                                              
         DS    CL2                 08                                           
LBLSHP2  DS    CL4                                                              
         DS    CL2                                                              
LBLALIN1 DS    CL30                                                             
         DS    CL2                                                              
         DS    CL2                 09                                           
LBLSHP3  DS    CL4                                                              
         DS    CL2                                                              
LBLALIN2 DS    CL30                                                             
         DS    CL2                                                              
         DS    CL2                 10                                           
LBLSHP4  DS    CL4                                                              
         DS    CL2                                                              
LBLALIN3 DS    CL30                                                             
         DS    CL2                                                              
         DS    CL40                11                                           
         DS    CL2                 12                                           
LBLCOM1  DS    CL35                                                             
         DS    CL3                                                              
         DS    CL2                 13                                           
LBLCOM2  DS    CL35                                                             
         DS    CL3                                                              
         DS    CL2                 14                                           
LBLCOM3  DS    CL35                                                             
         DS    CL3                                                              
         DS    CL40                15                                           
         DS    CL40                16                                           
*                                                                               
       ++INCLUDE DMWRKRK                                                        
       ++INCLUDE DDLOGOD                                                        
         PRINT OFF                                                              
       ++INCLUDE PPWORKD                                                        
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPMODEQU                                                       
         SPACE 2                                                                
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035PPREP7B02 05/01/02'                                      
         END                                                                    
