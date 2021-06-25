*          DATA SET SRWRK02    AT LEVEL 009 AS OF 01/06/14                      
*PHASE T15102A                                                                  
         TITLE '$WK - FILE DATA RECORD FUNCTIONS'                               
         PRINT NOGEN                                                            
WKDISP02 CSECT                                                                  
         NMOD1 000,**$WK2**,RA                                                  
         LR    RC,R1                                                            
         USING WRKWKD,RC                                                        
         L     R2,APARM                                                         
         USING SRPARMD,R2          R2=A(S/R PARAM LIST)                         
         L     R3,SRPARM6                                                       
         USING SRWRKFFD,R3         R3=A(TWA)                                    
         LR    R8,R3                                                            
         USING WRKSVD,R8           R8=A(TWA SAVE DATA)                          
         L     R9,SRPARM4                                                       
         USING COMFACSD,R9         R9=A(COM FAC LIST)                           
         EJECT                                                                  
**********************************************************************          
* INITIALIZE                                                                    
**********************************************************************          
RDNDX    LA    R4,SRVP2H           READ WKFILE INDEX FOR FILE                   
         L     R5,ACXREC                                                        
         USING WKRECD,R5           R5=A(WKFILE BUFFER)                          
         LA    R6,WKFNDX                                                        
         USING UKRECD,R6           R6=A(WKFILE INDEX)                           
         XC    UKINDEX,UKINDEX                                                  
         MVC   UKBKEY,IFUSRID                                                   
         MVC   UKFILNO,IFFILNO                                                  
         GOTO1 CDATAMGR,DMCB,(X'08',=C'INDEX'),WKFILE,(R6),AWKFREC,(R5)         
         CLI   DMCB+8,0                                                         
         BNE   ERR1                FILE NOT FOUND                               
         L     R7,DWKSAVE                                                       
         AR    R7,R5                                                            
         USING SKBUFFD,R7          R7=A(WKFILE BUFFER SAVE)                     
*                                                                               
RDREC    LA    R4,SRVP1H           READ RECORD FOR DISPLAY ACTION               
         CLI   ACTN,ACTRDIS                                                     
         BNE   RCHA                                                             
         L     RF,AWKFREC                                                       
         MVC   0(4,RF),IFREC                                                    
         GOTO1 CDATAMGR,DMCB,(X'00',=C'RANDOM')                                 
         CLI   DMCB+8,0                                                         
         BE    RDREC1                                                           
         TM    DMCB+8,X'90'                                                     
         BNZ   ERR2                RECORD NOT FOUND                             
         TM    DMCB+8,X'41'                                                     
         BO    ERR3                FILE FORMAT ERROR                            
         B     ERR4                FILE DISK ERROR                              
*                                                                               
RDREC1   L     RF,AWKFREC                                                       
         CLC   IFSTR,0(RF)                                                      
         BNL   ERR5                START BYTE TOO BIG                           
         EJECT                                                                  
**********************************************************************          
* DISPLAY RECORD DATA                                                           
**********************************************************************          
RDIS     MVI   FLAG,C'D'           DISPLAY RECORD DATA                          
         BAS   RE,RDISP                                                         
*                                                                               
RDIS1    LA    R4,SRVFCH           DISPLAY RECORD SAVE DATA                     
         USING WKSVD,R4                                                         
         MVC   WKSVFDA,=C'FD='     FILE DEFN                                    
         LA    R0,L'WKSVFDH/2                                                   
         GOTO1 CHEXOUT,DMCB,IFDEFN,WKSVFDH,(R0),=C'MIX'                         
         MVI   WKSVRDA-1,C','                                                   
         MVC   WKSVRDA,=C'RD='     RECD DEFN                                    
         LA    R0,L'WKSVRDH/2                                                   
         GOTO1 (RF),(R1),IFREC,WKSVRDH,(R0)                                     
         MVI   WKSVRAA-1,C','                                                   
         MVC   WKSVRAA,=C'RA='     RECD ADDR                                    
         MVC   SAVE(4),SKADDR                                                   
         MVC   SAVE+4(2),SKDISP                                                 
         MVC   SAVE+6(2),SKLEN                                                  
         LA    R0,L'WKSVRAH/2                                                   
         GOTO1 (RF),(R1),SAVE,WKSVRAH,(R0)                                      
*                                                                               
RDIS2    MVC   MSG(35),=C'Record data displayed - Reclen=NNNN'                  
         L     RF,AWKFREC                                                       
         SR    R0,R0                                                            
         ICM   R0,3,0(RF)                                                       
         CVD   R0,DUB                                                           
         UNPK  DUB(4),DUB+5(3)                                                  
         OI    DUB+3,X'F0'                                                      
         MVC   MSG+31(4),DUB                                                    
         MVC   SRVMSG,MSG                                                       
         OI    SRVMSGH+6,X'80'                                                  
         OI    SRVP1H+6,X'40'                                                   
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* CHANGE RECORD DATA                                                            
**********************************************************************          
RCHA     LA    R4,SRVFCH           CHECK OUTPUT SAVE DATA FORMAT                
         USING WKSVD,R4                                                         
         CLI   5(R4),WKSVDL                                                     
         BNE   RCHAE1                                                           
         CLC   WKSVFDA,=C'FD='                                                  
         BNE   RCHAE1                                                           
         CLC   WKSVRDA,=C'RD='                                                  
         BNE   RCHAE1                                                           
         CLC   WKSVRAA,=C'RA='                                                  
         BNE   RCHAE1                                                           
*                                                                               
RCHA1    LA    R0,L'WKSVFDH        FILE ID MUST BE THE SAME                     
         GOTO1 CHEXIN,DMCB,WKSVFDH,SAVE,(R0)                                    
         OC    12(4,R1),12(R1)                                                  
         BZ    RCHAE1                                                           
         CLC   SAVE(10),IFDEFN                                                  
         BNE   RCHAE1                                                           
         LA    R0,L'WKSVRDH        RECORD IS MUST BE THE SAME                   
         GOTO1 (RF),(R1),WKSVRDH,SAVE,(R0)                                      
         OC    12(4,R1),12(R1)                                                  
         BZ    RCHAE1                                                           
         CLC   SAVE(8),IFREC                                                    
         BNE   RCHAE1                                                           
*                                                                               
RCHA2    LA    R0,L'WKSVRAH        GET SAVE DISK ADDR AND READ REC              
         GOTO1 (RF),(R1),WKSVRAH,SAVE,(R0)                                      
         OC    12(4,R1),12(R1)                                                  
         BZ    RCHAE1                                                           
         MVC   CIADDR,SAVE                                                      
         OC    CIADDR(2),CIADDR    CHECK DISK ADDR LOOKS OK                     
         BZ    RCHAE1                                                           
         CLI   CIADDR+2,0                                                       
         BE    RCHAE1                                                           
         CLI   CIADDR+3,0                                                       
         BNE   RCHAE1                                                           
         GOTO1 CDATAMGR,DMCB,(X'00',DMREAD),WKFILE,CIADDR,(R5)                  
         CLI   DMCB+8,0                                                         
         BNE   RCHAE1                                                           
         CLC   IFUSRID(09),WKUSRID CHECK KEY IN RECORD MATCHES                  
         BNE   RCHAE1                                                           
*                                                                               
RCHA3    MVC   SKADDR,CIADDR       SET BUFFER SAVE                              
         MVC   SKFSTCI(2),UKCIADDR                                              
         MVC   SKCHRS,IFREC                                                     
         MVC   SKDISP,SAVE+4                                                    
         MVC   SKLEN,SAVE+6                                                     
         XC    SKXADDR,SKXADDR                                                  
         LH    R1,SKLEN            MOVE OUT RECORD                              
         L     RF,AWKFREC                                                       
         LR    RE,R5                                                            
         AH    RE,SKDISP                                                        
         SR    RE,R1                                                            
         MOVE  ((RF),(R1)),(RE)                                                 
         L     RF,AWKFREC                                                       
         CLC   SKLEN,0(RF)         CHECK LENGTH LOOKS OK                        
         BNE   RCHAE1                                                           
*                                                                               
RCHA4    MVI   FLAG,C'V'           VALIDATE HEX INPUT DATA                      
         BAS   RE,RDISP                                                         
         L     R4,FULL             TEST FOR INVALID HEX                         
         LTR   R4,R4                                                            
         BNZ   RCHAE2                                                           
         L     RF,AWKFREC                                                       
         CLC   SKLEN,0(RF)         TEST FOR CHANGE IN RECORD LENGTH             
         BNE   RCHAE3                                                           
         GOTO1 CDATAMGR,DMCB,(X'00',=C'WRI'),WKFILE,WKFNDX,AWKFREC,(R5)         
         CLI   DMCB+8,0                                                         
         BNE   RCHAE4                                                           
*                                                                               
RCHA5    MVI   FLAG,C'D'           REDISPLAY NEW RECORD DATA                    
         BAS   RE,RDISP                                                         
         MVC   MSG(33),=C'Record data changed - Reclen=NNNN'                    
         L     RF,AWKFREC                                                       
         SR    R0,R0                                                            
         ICM   R0,3,0(RF)                                                       
         CVD   R0,DUB                                                           
         UNPK  DUB(4),DUB+5(3)                                                  
         OI    DUB+3,X'F0'                                                      
         MVC   MSG+29(4),DUB                                                    
         MVC   SRVMSG,MSG                                                       
         OI    SRVMSGH+6,X'80'                                                  
         OI    SRVP1H+6,X'40'                                                   
         B     EXIT                                                             
*                                                                               
RCHAE1   LA    R4,SRVP1H                                                        
         MVC   MSG(33),=C'Change params differ from display'                    
         B     RCHAEX                                                           
RCHAE2   MVC   MSG(19),=C'Invalid hexadecimal'                                  
         B     RCHAEX                                                           
RCHAE3   LA    R4,SRVFD1H                                                       
         MVC   MSG(25),=C'Cant change record length'                            
         B     RCHAEX                                                           
RCHAE4   LA    R4,SRVP1H                                                        
         MVC   MSG(33),=C'File disk error on write'                             
         B     RCHAEX                                                           
RCHAEX   XC    SRVMSG,SRVMSG                                                    
         MVC   SRVMSG(8),=C'ED/9999 '                                           
         MVC   SRVMSG+2(1),SYSCH                                                
         MVC   SRVMSG+8(48),MSG                                                 
         OI    SRVMSGH+6,X'80'                                                  
         OI    6(R4),X'40'                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
ERR1     MVC   MSG(14),=C'File not found'                                       
         B     ERRX                                                             
ERR2     MVC   MSG(16),=C'Record not found'                                     
         B     ERRX                                                             
ERR3     MVC   MSG(17),=C'File format error'                                    
         B     ERRX                                                             
ERR4     MVC   MSG(15),=C'File disk error'                                      
         B     ERRX                                                             
ERR5     MVC   MSG(30),=C'Record start byte out of range'                       
         B     ERRX                                                             
*                                                                               
ERRX     XC    SRVMSG,SRVMSG                                                    
         MVC   SRVMSG(8),=C'ED/9999 '                                           
         MVC   SRVMSG+2(1),SYSCH                                                
         MVC   SRVMSG+8(48),MSG                                                 
ERRX1    OI    SRVMSGH+6,X'80'                                                  
         OI    6(R4),X'40'         POSN CURSOR TO ERROR FIELD                   
         LA    R4,SRVFCAH                                                       
         LH    RE,2(R4)            TRUNCATE SCREEN WITH TAB FIELD               
         LA    RE,9(RE)                                                         
         MVC   0(12,R4),=X'090000000000800100000100'                            
         STH   RE,2(R4)                                                         
         B     EXIT                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
* SUBROUTINE TO DISPLAY/VALIDATE RECORD DATA - FLAG=D(DISP) OR V(VALI)          
**********************************************************************          
RDISP    NTR1                                                                   
         LA    R4,SRVFD1H          R4=A(SCREEN LINE)                            
         USING WKSLD,R4                                                         
         L     R5,AWKFREC          R5=A(RECORD DATA)                            
         AH    R5,IFSTR                                                         
         LH    R6,IFEND            R6=LEN OF RECORD DATA                        
         LTR   R6,R6                                                            
         BNZ   RDISP0                                                           
         L     RF,AWKFREC                                                       
         SR    R6,R6                                                            
         ICM   R6,3,0(RF)                                                       
         BCTR  R6,0                                                             
RDISP0   SH    R6,IFSTR                                                         
         LA    R6,1(R6)                                                         
         XC    FULL,FULL           FULL=A(FIRST ERROR FIELD)                    
*                                                                               
RDISP1   LA    R2,L'WKSLA          R2=LEN OF DATA IN LINE                       
         CR    R2,R6                                                            
         BL    *+6                                                              
         LR    R2,R6                                                            
         CLI   FLAG,C'D'           TEST FOR DISPLAY MODE                        
         BE    RDISP3                                                           
*                                                                               
RDISP2   ZIC   RF,WKSLHH+5         VALIDATE HEXADECIMAL INPUT                   
         LTR   RF,RF                                                            
         BZ    RDISP6                                                           
         LR    R0,R2                                                            
         SLL   R0,1                                                             
         CR    R0,RF               LENGTH INPUT MUST = LENGTH OUTPUT            
         BNE   RDISP2A                                                          
         GOTO1 CHEXIN,DMCB,WKSLH,(R5),(R0)                                      
         OC    12(4,R1),12(R1)                                                  
         BNZ   RDISP6                                                           
RDISP2A  LA    R0,WKSLHH           EXIT WITH FULL=A(ERROR HEX FIELD)            
         ST    R0,FULL                                                          
         B     RDISPX                                                           
*                                                                               
RDISP3   LR    RF,R5               OUTPUT STR-END BYTES                         
         L     RE,AWKFREC                                                       
         SR    RF,RE                                                            
         CVD   RF,DUB                                                           
         UNPK  DUB(4),DUB+5(3)                                                  
         OI    DUB+3,X'F0'                                                      
         MVC   WKSLB(3),DUB+1                                                   
         MVI   WKSLB+3,C'-'                                                     
         AR    RF,R2                                                            
         BCTR  RF,0                                                             
         CVD   RF,DUB                                                           
         UNPK  DUB(4),DUB+5(3)                                                  
         OI    DUB+3,X'F0'                                                      
         MVC   WKSLB+4(3),DUB+1                                                 
         OI    WKSLBH+6,X'80'                                                   
*                                                                               
RDISP4   XC    WKSLH,WKSLH         OUTPUT HEXADECIMAL BYTES                     
         LR    R1,R2                                                            
         SLL   R1,1                                                             
         STC   R1,WKSLHH+5                                                      
         GOTO1 CHEXOUT,DMCB,(R5),WKSLH,(R2),=C'MIX'                             
         OI    WKSLHH+6,X'80'                                                   
*                                                                               
RDISP5   XC    WKSLA,WKSLA         OUTPUT ALPHA BYTES                           
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WKSLA(0),0(R5)                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         TR    WKSLA(0),RDISPT                                                  
         OI    WKSLAH+6,X'80'                                                   
*                                                                               
RDISP6   SR    R6,R2               BUMP TO NEXT SCREEN LINE                     
         BZ    RDISP6A                                                          
         AR    R5,R2                                                            
         LA    R4,WKSLL(R4)                                                     
         CLI   WKSLBH,9            TEST END OF SCREEN                           
         BH    RDISP1                                                           
         B     RDISPX                                                           
RDISP6A  LA    R4,WKSLL(R4)        TRUNC SCREEN IF END OF DATA                  
         CLI   WKSLBH,9                                                         
         BNH   RDISPX                                                           
         LH    RE,2(R4)                                                         
         LA    RE,9(RE)                                                         
         MVC   0(12,R4),=X'090000000000800100000100'                            
         STH   RE,2(R4)                                                         
*                                                                               
RDISPX   XIT1                                                                   
*                                                                               
RDISPT   DC    CL32'................................'                           
         DC    CL32'................................'                           
         DC    CL32' ...........<(+.&&..........$*);.'                          
         DC    CL32'-/.........,%.>............#@''="'                          
         DC    CL32'.abcdefghi.......jklmnopqr......'                           
         DC    CL32'..stuvwxyz......................'                           
         DC    CL32'.ABCDEFGHI.......JKLMNOPQR......'                           
         DC    CL32'..STUVWXYZ......0123456789......'                           
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                                     
***********************************************************************         
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DSECTS                                                                        
***********************************************************************         
WKSLD    DSECT                     SCREEN DISPLAY LINE                          
WKSLBH   DS    CL8                                                              
WKSLB    DS    CL07                                                             
WKSLHH   DS    CL8                                                              
WKSLH    DS    CL40                                                             
WKSLAH   DS    CL8                                                              
WKSLA    DS    CL20                                                             
WKSLL    EQU   *-WKSLD                                                          
         SPACE 2                                                                
WKSVD    DSECT                     SCREEN SAVE LINE                             
         DS    CL8                                                              
WKSVFDA  DS    CL3                                                              
WKSVFDH  DS    CL24                                                             
         DS    CL1                                                              
WKSVRDA  DS    CL3                                                              
WKSVRDH  DS    CL16                                                             
         DS    CL1                                                              
WKSVRAA  DS    CL3                                                              
WKSVRAH  DS    CL16                                                             
WKSVDL   EQU   *-WKSVFDA                                                        
         SPACE 2                                                                
*DMWRKRK                                                                        
       ++INCLUDE DMWRKRK                                                        
         EJECT                                                                  
*SRWRKWK                                                                        
       ++INCLUDE SRWRKWK                                                        
         EJECT                                                                  
SRWRKFFD DSECT                                                                  
         DS    CL64                                                             
*SRWRKFFD                                                                       
       ++INCLUDE SRWRKFFD                                                       
         EJECT                                                                  
*DMWRKRD                                                                        
       ++INCLUDE DMWRKRD                                                        
         EJECT                                                                  
*DMWRKRS                                                                        
       ++INCLUDE DMWRKRS                                                        
         EJECT                                                                  
*DDCOMFACS                                                                      
       ++INCLUDE DDCOMFACS                                                      
         SPACE 2                                                                
*FASRPARM                                                                       
       ++INCLUDE FASRPARM                                                       
         EJECT                                                                  
*DDFLDHDR                                                                       
       ++INCLUDE DDFLDHDR                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SRWRK02   01/06/14'                                      
         END                                                                    
