*          DATA SET ACREP7502  AT LEVEL 054 AS OF 05/13/09                      
*PHASE AC7502A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'ACCOUNT NAME AND ADDRESS LISTINGS, WITH INTERNAL SORT'          
ACNMAD   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,ACNMADD,RR=R4                                                  
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING LISTD,RC                                                         
         ST    R4,RELO                                                          
         L     R9,=V(SORTER)                                                    
         A     R9,RELO                                                          
         ST    R9,SORTER                                                        
         CLI   MODE,REQFRST                                                     
         BNE   CLIMOD                                                           
         CLI   QSORT,C' '                                                       
         BE    CLIMOD                                                           
         L     R9,=A(SORTC)                                                     
         A     R9,RELO                                                          
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,(40,(R9))                           
         B     CLIMOD                                                           
SORTCARD DC    CL80'SORT FIELDS=(5,36,A),FORMAT=CH,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=500'                                   
         EJECT                                                                  
*              OUTPUT ROUTINES                                                  
         SPACE 1                                                                
CLIMOD   CLI   MODE,LEDGFRST                                                    
         BNE   LST4                                                             
         MVI   ACTSW,C'N'                                                       
         BAS   RE,EXTHEAD                                                       
         SPACE 1                                                                
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         B     XIT                                                              
         SPACE 1                                                                
LST4     CLI   MODE,PROCLEVA                                                    
         BE    LST5                                                             
         CLI   QOPT1,C'1'                                                       
         BE    LST8                                                             
         CLI   MODE,PROCLEVB                                                    
         BE    LST5                                                             
         CLI   QOPT1,C'2'                                                       
         BE    LST8                                                             
         CLI   MODE,PROCLEVC                                                    
         BE    LST5                                                             
         CLI   QOPT1,C'J'                                                       
         BE    LST8                                                             
         CLI   MODE,PROCLEVD                                                    
         BNE   LST8                                                             
*                                                                               
LST5     DS    0H                                                               
         CLI   QOPT1,C'J'                                                       
         BNE   LST5A                                                            
         CLI   MODE,PROCLEVC                                                    
         BNE   LST8                                                             
         SPACE 1                                                                
LST5A    L     RE,ADACC                                                         
         TM    ACTRSTAT-ACTRECD(RE),ACTSDRFT                                    
         BO    LST8                                                             
*                                                                               
         BAS   RE,EXTNMADD                                                      
         CLI   QSORT,C' '                                                       
         BE    LST6                                                             
         GOTO1 SORTER,DMCB,=C'PUT',LISTD                                        
         MVI   ACTSW,C'Y'                                                       
         B     XIT                                                              
         SPACE 1                                                                
LST6     BAS   RE,PRINTEM                                                       
         B     XIT                                                              
         SPACE 1                                                                
LST8     CLI   MODE,REQLAST        REQ LAST                                     
         BNE   XIT                                                              
         CLI   QSORT,C' '                                                       
         BE    XIT                                                              
         CLI   ACTSW,C'Y'                                                       
         BNE   *+8                                                              
         BAS   RE,LST20            GET/PRINT SORTED RECORDS                     
         GOTO1 SORTER,DMCB,=C'END'                                              
         B     XIT                                                              
         EJECT                                                                  
*              INPUT ROUTINES                                                   
         SPACE 1                                                                
LST20    NTR1                                                                   
         XC    AREA,AREA                                                        
NXTSRT   GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         LA    R2,AREAHEAD                                                      
         LA    R3,500                                                           
         SR    R5,R5                                                            
         ICM   R5,3,0(R4)                                                       
         MVCL  R2,R4                                                            
         SPACE 1                                                                
         BAS   RE,PRINTEM                                                       
         B     NXTSRT                                                           
         EJECT                                                                  
*              ROUTINE TO EXTRACT HEADER VALUES                                 
         SPACE 1                                                                
EXTHEAD  NTR1                                                                   
         SR    R3,R3                                                            
         LA    R4,SAVECOMP                                                      
         L     R2,ADCMPNAM                                                      
         BAS   RE,EXTNAME                                                       
         L     R2,ADUNTNAM                                                      
         BAS   RE,EXTNAME                                                       
         L     R2,ADLDGNAM                                                      
         BAS   RE,EXTNAME                                                       
         L     R2,ADLDGHIR                                                      
         MVC   SAVEHEIR,0(R2)                                                   
         B     XIT                                                              
         SPACE 1                                                                
EXTNAME  IC    R3,1(R2)                                                         
         USING ACNAMED,R2                                                       
         MVC   0(36,R4),SPACES                                                  
         SH    R3,=H'3'                                                         
         EX    R3,NAMOUT                                                        
         LA    R4,36(R4)                                                        
         BR    RE                                                               
         SPACE 1                                                                
NAMOUT   MVC   0(0,R4),ACNMNAME                                                 
         EJECT                                                                  
*              ROUTINE TO EXTRACT NAME AND ADDRESS                              
         SPACE 1                                                                
EXTNMADD NTR1                                                                   
         L     R2,ADACC                                                         
         CLI   MODE,PROCLEVA                                                    
         BNE   *+8                                                              
         L     R2,ADHEIRA                                                       
         CLI   MODE,PROCLEVB                                                    
         BNE   *+8                                                              
         L     R2,ADHEIRB                                                       
***                                                                             
         CLI   MODE,PROCLEVC                                                    
         BNE   *+8                                                              
         L     R2,ADHEIRC                                                       
***                                                                             
         MVC   AREANUM,0(R2)                                                    
         SR    R3,R3                                                            
         L     R2,ADACCNAM                                                      
         CLI   MODE,PROCLEVA                                                    
         BNE   *+8                                                              
         L     R2,ADLVANAM                                                      
         CLI   MODE,PROCLEVB                                                    
         BNE   *+8                                                              
         L     R2,ADLVBNAM                                                      
***                                                                             
         CLI   MODE,PROCLEVC                                                    
         BNE   *+8                                                              
         L     R2,ADLVCNAM                                                      
***                                                                             
         LA    R4,AREANAME                                                      
         BAS   RE,EXTNAME                                                       
EX4      DS    0H                                                               
         L     R2,ADACCSTA                                                      
         CLI   MODE,PROCLEVA                                                    
         BNE   *+8                                                              
         L     R2,ADLVASTA                                                      
         CLI   MODE,PROCLEVB                                                    
         BNE   *+8                                                              
         L     R2,ADLVBSTA                                                      
         USING RSTELD,R2                                                        
         MVC   AREAFIL1(AREAFILL),SPACES                                        
         MVC   AREAFIL1,RSTFILT1                                                
         CLI   AREAFIL1,C' '                                                    
         BNE   *+8                                                              
         MVI   AREAFIL1,C'.'                                                    
*                                                                               
         MVC   AREAFIL2,RSTFILT2                                                
         CLI   AREAFIL2,C' '                                                    
         BNE   *+8                                                              
         MVI   AREAFIL2,C'.'                                                    
*                                                                               
         MVC   AREAFIL3,RSTFILT3                                                
         CLI   AREAFIL3,C' '                                                    
         BNE   *+8                                                              
         MVI   AREAFIL3,C'.'                                                    
*                                                                               
         MVC   AREAFIL4,RSTFILT4                                                
         CLI   AREAFIL4,C' '                                                    
         BNE   *+8                                                              
         MVI   AREAFIL4,C'.'                                                    
*                                                                               
         CLI   RSTLN,RSTLN2Q                                                    
         BL    EXTHED                                                           
*                                                                               
         MVC   AREAFIL5,RSTFILT5                                                
         CLI   AREAFIL5,C' '                                                    
         BNE   *+8                                                              
         MVI   AREAFIL5,C'.'                                                    
*                                                                               
EXTHED   XC    AREAHEAD,AREAHEAD                                                
         MVC   AREAHEAD(2),=Y(LAREAHD)                                          
         MVC   AREAADD1(AREAADDL),SPACES                                        
         MVC   AREAHEAD+4(3),RCRQTOT                                            
         L     R2,ADACCADD                                                      
         CLI   MODE,PROCLEVA                                                    
         BNE   *+8                                                              
         L     R2,ADLVAADD                                                      
         CLI   MODE,PROCLEVB                                                    
         BNE   *+8                                                              
         L     R2,ADLVBADD                                                      
         LTR   R2,R2               ANY ADDRESS                                  
         BNZ   EXTNM2                                                           
         B     EXTKEY                                                           
         SPACE 1                                                                
EXTNM2   EQU   *                                                                
         USING ACADDD,R2                                                        
         IC    R3,ACADLNES         MUST BE 1-4                                  
         LTR   R3,R3               0=BAD 22 ELEMENT - SKIP PUTTING OUT          
         BZ    EXTKEY               ADDRESSES                                   
         LA    R4,ACADADD                                                       
         LA    R5,AREAADD1                                                      
         SPACE 1                                                                
EXTADD   MVC   0(26,R5),0(R4)                                                   
         LA    R4,26(R4)                                                        
         LA    R5,26(R5)                                                        
         BCT   R3,EXTADD                                                        
         SPACE 1                                                                
EXTKEY   MVC   AREAKEY,SPACES                                                   
         CLI   QSORT,C' '                                                       
         BE    XIT                                                              
         MVC   AREAKEY,AREANAME                                                 
         CLI   QSORT,C'A'                                                       
         BE    XIT                                                              
         MVC   AREAKEY(AREAFILL),AREAFIL1                                       
         MVC   AREAKEY+AREAFILL,AREANAME                                        
         CLI   QSORT,C'F'                                                       
         BE    XIT                                                              
         CLI   QSORT,C'1'                                                       
         BL    XIT                                                              
         CLI   QSORT,C'9'                                                       
         BH    XIT                                                              
         PACK  DUB,QSORT                                                        
         CVB   R2,DUB                                                           
         LA    R3,AREANUM                                                       
         LA    R2,2(R2,R3)                                                      
         MVC   AREAKEY(15),0(R2)                                                
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              PRINT ROUTINES                                                   
         SPACE 1                                                                
PRINTEM  NTR1                                                                   
         MVC   P+1(12),AREANUM+3                                                
         MVC   P+14(AREAFILL),AREAFIL1                                          
         MVC   P+20(36),AREANAME                                                
         MVC   P+58(26),AREAADD1                                                
         MVC   P+85(26),AREAADD2                                                
         MVC   PSECOND+58(26),AREAADD3                                          
         MVC   PSECOND+85(26),AREAADD4                                          
         MVC   PTHIRD+58(26),AREAADD5                                           
         MVC   HEAD5+9(1),AREANUM+1                                             
         MVC   HEAD6+9(1),AREANUM+2                                             
         MVC   HEAD4+11(36),SAVECOMP                                            
         MVC   HEAD5+11(36),SAVEUNIT                                            
         MVC   HEAD6+11(36),SAVELEDG                                            
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         SPACE 1                                                                
SORTER   DS    F                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
SORTC    CSECT                                                                  
         DS    0D                                                               
         DS    41000C                                                           
         SPACE 1                                                                
*              DSECT FOR MODULE                                                 
         SPACE 1                                                                
LISTD    DSECT                                                                  
AREAHEAD DS    D                                                                
AREA     DS    CL48                                                             
         ORG   AREA                                                             
AREAKEY  DS    CL38                                                             
AREANAME DS    CL36                                                             
AREANUM  DS    CL15                                                             
*                                                                               
AREAFIL1 DS    CL1                                                              
AREAFIL2 DS    CL1                                                              
AREAFIL3 DS    CL1                                                              
AREAFIL4 DS    CL1                                                              
AREAFIL5 DS    CL1                                                              
AREAFILL EQU   *-AREAFIL1                                                       
*                                                                               
AREAADD1 DS    CL26                                                             
AREAADD2 DS    CL26                                                             
AREAADD3 DS    CL26                                                             
AREAADD4 DS    CL26                                                             
AREAADD5 DS    CL26                                                             
AREAADDL EQU   *-AREAADD1                                                       
*                                                                               
LAREAHD  EQU   *-AREAHEAD                                                       
         ORG   AREAHEAD                                                         
         DS    CL500                                                            
         SPACE 1                                                                
SAVECOMP DS    CL36                                                             
SAVEUNIT DS    CL36                                                             
SAVELEDG DS    CL36                                                             
SAVEHEIR DS    CL66                                                             
RELO     DS    F                                                                
ACTSW    DS    CL1                                                              
         EJECT                                                                  
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054ACREP7502 05/13/09'                                      
         END                                                                    
