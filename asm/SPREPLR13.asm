*          DATA SET SPREPLR13  AT LEVEL 047 AS OF 05/01/02                      
*PHASE SPLR02T,+0,NOAUTO                                                        
         TITLE 'SPREPLR13 RADIO KEY CONVERSION PROGRAM'                         
SPLR13   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPLR13,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         STM   RA,RC,SPLRRA                                                     
         ST    R5,RELO                                                          
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         OPEN  (OUT,(OUTPUT))                                                   
         XC    BKLIST,BKLIST                                                    
         PACK  DUB,QBOOK1(2)                                                    
         CVB   RE,DUB                                                           
         STC   RE,BOOKFLTR                                                      
         PACK  DUB,QBOOK1+2(2)                                                  
         CVB   RE,DUB                                                           
         STC   RE,BOOKFLTR+1                                                    
*        MVC   BOOKFLTR,=X'560B'                                                
         XC    BOOKFLTR,=X'FFFF'                                                
         MVI   FORCEHED,C'Y'                                                    
         SPACE 2                                                                
         LA    R6,SPLKEY                                                        
         USING BSKEY,R6                                                         
         XC    SPLKEY,SPLKEY                                                    
         MVI   BSCODE,C'M'         SET TO READ PRIMARY DIRECTORY                
         MVI   BSMEDIA,C'R'                                                     
         MVI   BSSRC,C'A'          SET RATING SERVICE                           
         MVI   BSIND,X'02'                                                      
         MVC   SVSPLKEY,SPLKEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         B     SPLR3                                                            
         SPACE 2                                                                
SPLR2    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'DEMDIR',SVSPLKEY,SPLKEY               
SPLR3    CLI   BSMEDIA,C'R'                                                     
         BNE   END1                                                             
         CLI   BSSRC,C'A'                                                       
         BNE   END1                                                             
         CLI   BSIND,X'02'                                                      
         BNE   SPLR2                                                            
         OC    BOOKFLTR,BOOKFLTR                                                
         BZ    *+14                                                             
         CLC   BSBOOK,BOOKFLTR     FILTER ON BOOK                               
         BNE   SPLR2                                                            
*                                                                               
         LA    RE,BKLIST           SAVE A LIST OF BOOKS                         
SPLR4    CLI   0(RE),0                                                          
         BE    SPLR5                                                            
         CLC   0(2,RE),BSBOOK                                                   
         BE    SPLR5                                                            
         LA    RE,2(RE)                                                         
         B     SPLR4                                                            
SPLR5    MVC   0(2,RE),BSBOOK                                                   
*                                                                               
         LA    R5,WORK                                                          
         USING STATABD,R5                                                       
         MVC   SBOOK,BSBOOK                                                     
         MVC   SSTA,BSSTAT                                                      
         MVC   SRSMKT,BSRMKT                                                    
         MVC   SSPMKT,BSKMKT                                                    
         L     R8,COUNT                                                         
         GOTO1 BINSRCH,DMCB,(1,WORK),ASTATAB,(R8),11,(0,9),45450                
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   COUNT,8(R1)                                                      
         B     SPLR2                                                            
         EJECT                                                                  
*        SET TO BEGINNING OF TABLE                                              
*        FORM A RATING RECORD KEY FROM THE TABLE                                
*         EXTRACT THE STATION RECORDS                                           
*         LOOP UNTIL END                                                        
*        LOOP UNTIL END                                                         
END1     L     R8,COUNT                                                         
         L     R5,ASTATAB                                                       
         XC    OCOUNT,OCOUNT                                                    
END1A    LA    R6,SPLKEY                                                        
         USING DRKEY,R6                                                         
         XC    SPLKEY,SPLKEY                                                    
         MVI   DRCODE,C'D'                                                      
         MVI   DRMEDIA,C'R'                                                     
         MVI   DRSRC,C'A'                                                       
         MVC   DRSTAT,SSTA                                                      
         MVC   DRBOOK,SBOOK                                                     
         MVC   DRKMKT,SSPMKT                                                    
         MVC   SVSPLKEY,SPLKEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         MVC   SVDA,DRNDXDA                                                     
         CLC   SVSPLKEY(12),SPLKEY                                              
         BE    *+12                                                             
         BAS   RE,ERROR                                                         
         B     END1B                                                            
         BAS   RE,RTGREC                                                        
         MVC   P(5),DRSTAT                                                      
         EDIT  (B2,SRSMKT),(4,P+7)                                              
         EDIT  RCOUNT,(5,P+12)                                                  
         EDIT  DCOUNT,(5,P+18)                                                  
         EDIT  (B2,SSPMKT),(4,P+24)                                             
         GOTO1 REPORT                                                           
END1B    LA    R5,11(R5)                                                        
         BCT   R8,END1A                                                         
         SPACE 2                                                                
SPML     XC    OCOUNT,OCOUNT                                                    
         LA    R6,SPLKEY                                                        
         USING MLKEY,R6                                                         
         XC    SPLKEY,SPLKEY                                                    
         MVI   MLCODE,C'M'         SET TO READ PRIMARY DIRECTORY                
         MVI   MLMEDIA,C'R'                                                     
         MVI   MLSRC,C'A'          SET RATING SERVICE                           
         MVI   MLIND,X'00'                                                      
         MVC   SVSPLKEY,SPLKEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         B     SPML6                                                            
         SPACE 2                                                                
SPML1    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'DEMDIR',SVSPLKEY,SPLKEY               
SPML6    CLI   MLMEDIA,C'R'                                                     
         BNE   SPMLX                                                            
         CLI   MLSRC,C'A'                                                       
         BNE   SPMLX                                                            
         CLI   MLIND,X'00'                                                      
         BNE   SPML1                                                            
         OC    BOOKFLTR,BOOKFLTR                                                
         BZ    *+14                                                             
         CLC   MLBOOK,BOOKFLTR     FILTER ON BOOK                               
         BNE   SPML1                                                            
         MVI   MLHOME,C'H'         MODIFY FOR NEW KEY                           
         OC    MLKMKT,MLKMKT                                                    
         BZ    *+8                                                              
         MVI   MLHOME,C'S'                                                      
         MVC   MLKMKT,MLRMKT                                                    
         MVC   MLKEY+20(4),=X'FFFF0000'                                         
         MVC   RECIOLN,=X'00180000'                                             
         MVC   RECIO(24),MLKEY                                                  
         BAS   RE,PUTOUT                                                        
         EDIT  (B2,MLRMKT),(4,P)                                                
         MVC   P+5(5),MLSTAT                                                    
         EDIT  (B2,MLKMKT),(4,P+11)                                             
         MVC   P+16(1),MLHOME                                                   
         GOTO1 REPORT                                                           
         B     SPML1                                                            
SPMLX    DS    0H                                                               
         SPACE 2                                                                
SPBS1    L     R8,COUNT                                                         
         L     R5,ASTATAB                                                       
         LA    R6,SPLKEY                                                        
         USING BSKEY,R6                                                         
         XC    OCOUNT,OCOUNT                                                    
SPBS2    XC    SPLKEY,SPLKEY                                                    
         MVC   BSKEY(3),=C'MRA'                                                 
         MVC   BSBOOK,SBOOK                                                     
         MVI   BSIND,BSINDEQU                                                   
         MVC   BSSTAT,SSTA                                                      
         MVC   BSRMKT,SRSMKT                                                    
         MVC   BSKMKT,SSPMKT                                                    
         MVI   BSHOME,C'H'         MODIFY FOR NEW KEY                           
         OC    BSKMKT,BSKMKT                                                    
         BZ    *+8                                                              
         MVI   BSHOME,C'S'                                                      
         MVC   BSKMKT,BSRMKT                                                    
         MVC   BSKEY+20(4),=X'FFFF0000'                                         
         MVC   RECIOLN,=X'00180000'                                             
         MVC   RECIO(24),BSKEY                                                  
         BAS   RE,PUTOUT                                                        
         MVC   P(5),BSSTAT                                                      
         EDIT  (B2,BSRMKT),(4,P+6)                                              
         EDIT  (B2,BSKMKT),(4,P+11)                                             
         MVC   P+16(1),BSHOME                                                   
         GOTO1 REPORT                                                           
         LA    R5,11(R5)                                                        
         BCT   R8,SPBS2                                                         
         SPACE 2                                                                
         L     R8,COUNT                                                         
         L     R5,ASTATAB                                                       
         XC    OCOUNT,OCOUNT                                                    
END4A    LA    R6,SPLKEY                                                        
         USING DRKEY,R6                                                         
         XC    SPLKEY,SPLKEY                                                    
         MVI   DRCODE,C'R'                                                      
         MVI   DRMEDIA,C'R'                                                     
         MVI   DRSRC,C'A'                                                       
         MVC   DRSTAT,SSTA                                                      
         MVC   DRBOOK,SBOOK                                                     
         MVC   DRKMKT,SSPMKT                                                    
         MVC   SVSPLKEY,SPLKEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         MVC   SVDA,DRNDXDA                                                     
         CLC   SVSPLKEY(12),SPLKEY                                              
         BE    *+12                                                             
         BAS   RE,ERROR                                                         
         B     END4B                                                            
         BAS   RE,RTGREC                                                        
         MVC   P(5),DRSTAT                                                      
         EDIT  (B2,SRSMKT),(4,P+7)                                              
         EDIT  RCOUNT,(5,P+12)                                                  
         EDIT  DCOUNT,(5,P+18)                                                  
         GOTO1 REPORT                                                           
         OC    DCOUNT,DCOUNT                                                    
         BNZ   END4B                                                            
END4B    LA    R5,11(R5)                                                        
         BCT   R8,END4A                                                         
         CLOSE (OUT)                                                            
EXIT     XMOD1 1                                                                
         EJECT                                                                  
ERROR    NTR1                                                                   
         XC    P(132),P                                                         
         MVC   P(5),SSTA                                                        
         MVC   P+6(3),SVSPLKEY                                                  
         EDIT  SSPMKT,(4,P+10)                                                  
         GOTO1 REPORT                                                           
         XIT1                                                                   
         SPACE 2                                                                
RTGREC   NTR1                                                                   
         MVC   RECIO(20),SPLKEY                                                 
         XC    RCOUNT,RCOUNT                                                    
         XC    DCOUNT,DCOUNT                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMFIL ',SVDA,RECIO                   
         B     RTGREC2                                                          
RTGREC1  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'DEMFIL ',SVDA,RECIO                   
RTGREC2  CLI   8(R1),0                                                          
         BNE   RTGRECX                                                          
         LA    R6,RECIO                                                         
         USING DRKEY,R6                                                         
         LA    RE,DRFRSTEL                                                      
         USING MARELEM,RE                                                       
         CLC   MARNO,SRSMKT                                                     
         BNE   RTGREC1                                                          
         MVI   DRHOME,C'H'                                                      
         OC    DRKMKT,DRKMKT                                                    
         BZ    *+8                                                              
         MVI   DRHOME,C'S'                                                      
         MVC   DRKMKT,SRSMKT                                                    
         L     RF,RCOUNT                                                        
         LA    RF,1(RF)                                                         
         ST    RF,RCOUNT                                                        
         LA    R6,RECIO                                                         
         XC    RECIOLN,RECIOLN                                                  
         SR    RE,RE                                                            
         ICM   RE,3,DRRLEN                                                      
         LA    RE,4(RE)                                                         
         STCM  RE,3,RECIOLN                                                     
         SH    RE,=H'4'                                                         
         A     RE,DCOUNT                                                        
         ST    RE,DCOUNT                                                        
         BAS   RE,PUTOUT                                                        
*        C     RF,=F'30'                                                        
*        BL    *+6                                                              
*        DC    H'0'                                                             
*        GOTO1 HEXOUT,DMCB,RECIOLN,P,60                                         
*        GOTO1 REPORT                                                           
         B     RTGREC1                                                          
RTGRECX  XIT1                                                                   
         SPACE 2                                                                
PUTOUT   NTR1                                                                   
         LA    R5,RECIOLN                                                       
         L     R7,=A(OUT)                                                       
         PUT   (R7),(R5)                                                        
         L     RE,OCOUNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,OCOUNT                                                        
         C     RE,=F'30'                                                        
         BH    PUTOUTX                                                          
         GOTO1 HEXOUT,DMCB,RECIOLN,P,60                                         
*        GOTO1 REPORT                                                           
PUTOUTX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
COUNT    DC    F'0'                                                             
OCOUNT   DC    F'0'                                                             
RCOUNT   DC    F'0'                                                             
DCOUNT   DC    F'0'                                                             
SPLRRA   DC    F'0'                                                             
SPLRRB   DC    F'0'                                                             
SPLRRC   DC    F'0'                                                             
BOOKFLTR DS    CL2                                                              
BKLIST   DS    CL30                                                             
         SPACE 2                                                                
         EJECT                                                                  
RELO     DC    F'0'                                                             
*                                                                               
ASTATAB  DC    A(STATAB)                                                        
SVDA     DS    F                                                                
SPLKEY   DS    CL24                                                             
SVSPLKEY DS    CL24                                                             
RPTYPE   DS    C                                                                
RECIOLN  DS    CL4                                                              
RECIO    DS    2000C                                                            
         SPACE 2                                                                
OUT      DCB   DDNAME=OUT,DSORG=PS,RECFM=VB,LRECL=2000,                X        
               BLKSIZE=32760,MACRF=PM                                           
STATAB   DS    500000C                                                          
         SPACE 2                                                                
STATABD  DSECT                                                                  
SMS      DS    0CL11                                                            
SSTA     DS    CL5                                                              
SBOOK    DS    CL2                                                              
SRSMKT   DS    CL2                                                              
SSPMKT   DS    CL2                                                              
STATEND  DS    0C                                                               
STATLEN  EQU   STATEND-SBOOK                                                    
         SPACE 2                                                                
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047SPREPLR13 05/01/02'                                      
         END                                                                    
