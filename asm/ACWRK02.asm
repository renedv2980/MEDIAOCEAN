*          DATA SET ACWRK02    AT LEVEL 004 AS OF 05/01/02                      
*PHASE T61702A                                                                  
         TITLE 'ACC WRK - FILE DATA RECORD FUNCTIONS'                           
         PRINT NOGEN                                                            
WKRDIS   CSECT                                                                  
         NMOD1 000,**RWK2**                                                     
         LR    RC,R1                                                            
         USING WRKWKD,RC                                                        
         L     R2,APARM            R2=A(PARAM LIST)                             
         L     R3,4(R2)                                                         
         USING ACWRKFFD,R3         R3=A(TWA)                                    
         LR    R8,R3                                                            
         USING WRKSVD,R8           R8=A(TWA SAVE DATA)                          
         L     R9,16(R2)                                                        
         USING COMFACSD,R9         R9=A(COM FAC LIST)                           
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING WKRDIS+4096,RA      RA=SECOND BASE REG                           
         SPACE 2                                                                
         LA    R5,CXREC            R5=A(WKFILE BUFFER)                          
         USING WKRECD,R5                                                        
         LA    R6,WKFNDX                                                        
         USING UKRECD,R6           R6=A(WKFILE INDEX)                           
         L     R7,DWKSAVE          R7=A(WKFILE BUFFER SAVE)                     
         AR    R7,R5                                                            
         USING SKBUFFD,R7                                                       
*                                                                               
         CLI   ACTN,ACTRCHA                                                     
         BE    RCHA                                                             
         CLI   MODE,0              READ INDEX FOR FIRST RECORD DISP             
         BE    RDNDX                                                            
         CLC   IFDEFN,LIFDEFN      READ INDEX FOR NEW FILE ID                   
         BNE   RDNDX                                                            
         MVC   SKBUFFD(L'LBUFFSV),LBUFFSV                                       
         MVC   SKLABEL+3(4),MYSIN  RESTORE BUFF SAVE WITH NEW SIN               
         MVI   SKLCTRL,0                                                        
         XC    SKADDR,SKADDR                                                    
         XC    SKXADDR,SKXADDR                                                  
         B     RDREC                                                            
         EJECT                                                                  
RDNDX    LA    R4,SRVP2H           READ WKFILE INDEX FOR FILE                   
         MVI   MODE,0                                                           
         XC    UKINDEX,UKINDEX                                                  
         MVC   UKKEY,IFUSRID                                                    
         MVC   UKFILNO,IFFILNO                                                  
         GOTO1 CDATAMGR,DMCB,(X'08',=C'INDEX'),WKFILE,(R6),WKFREC,(R5)          
         CLI   DMCB+8,0                                                         
         BNE   ERR1                FILE NOT FOUND                               
         MVI   MODE,1              SET MODE TO FILE FOUND                       
         MVC   LBUFFSV,SKBUFFD                                                  
         MVC   LIFDEFN,IFDEFN                                                   
         XC    LIFREC(8),LIFREC                                                 
         MVC   LACTN,ACTN                                                       
         SPACE 2                                                                
RDREC    LA    R4,SRVP1H           READ RECORD FOR DISPLAY ACTION               
         MVI   MODE,1                                                           
         MVC   WKFREC(4),IFREC                                                  
         GOTO1 CDATAMGR,DMCB,(X'00',=C'RANDOM'),WKFILE,(R6),WKFREC,(R5)         
         CLI   DMCB+8,0                                                         
         BE    RDREC1                                                           
         TM    DMCB+8,X'90'                                                     
         BNZ   ERR2                RECORD NOT FOUND                             
         TM    DMCB+8,X'41'                                                     
         BO    ERR3                FILE FORMAT ERROR                            
         B     ERR4                FILE DISK ERROR                              
RDREC1   CLC   IFSTR,WKFREC                                                     
         BNL   ERR5                START BYTE TOO BIG                           
         MVI   MODE,2              SET MODE TO RECORD FOUND                     
         MVC   LBUFFSV,SKBUFFD                                                  
         MVC   LIFDEFN,IFDEFN                                                   
         MVC   LIFREC(8),IFREC                                                  
         B     RDIS                                                             
         EJECT                                                                  
RDIS     MVI   FLAG,C'D'           DISPLAY RECORD DATA                          
         BAS   RE,RDISP                                                         
*                                                                               
RDIS1    LA    R4,SRVECH           DISPLAY RECORD SAVE DATA                     
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
         OI    SRVECH+6,X'80'                                                   
*                                                                               
RDIS2    MVC   MSG(35),=C'RECORD DATA DISPLAYED - RECLEN=NNNN'                  
         LH    R0,WKFREC                                                        
         CVD   R0,DUB                                                           
         UNPK  DUB(4),DUB+5(3)                                                  
         OI    DUB+3,X'F0'                                                      
         MVC   MSG+31(4),DUB                                                    
         MVC   SRVMSG,MSG                                                       
         OI    SRVMSGH+6,X'80'                                                  
         OI    SRVP1H+6,X'40'                                                   
         B     EXIT                                                             
         EJECT                                                                  
RCHA     LA    R4,SRVP1H           CHANGE RECORD DATA                           
         CLI   MODE,2                                                           
         BNE   RCHAE1              NO VALID RECORD DATA DISPLAYED               
         CLC   LIFREC(8),IFREC                                                  
         BNE   RCHAE2              RECORD DEFN CHANGED                          
         LA    R4,SRVP2H                                                        
         CLC   LIFDEFN,IFDEFN                                                   
         BNE   RCHAE3              FILE DEFN CHANGED                            
         LA    R4,SRVP1H                                                        
*                                                                               
RCHA1    MVC   SKBUFFD(L'LBUFFSV),LBUFFSV                                       
         MVC   SKLABEL+3(4),MYSIN  RESTORE BUFF SAVE WITH NEW SIN               
         MVC   CIADDR,SKADDR                                                    
         OC    CIADDR(2),CIADDR                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 CDATAMGR,DMCB,(X'00',DMREAD),WKFILE,CIADDR,(R5)                  
         CLI   DMCB+8,0                                                         
         BNE   RCHAE4                                                           
         CLC   IFUSRID,WKUSRID     CHECK BUFFER STILL LOOKS OK                  
         BNE   RCHAE1                                                           
         CLC   IFFILNO,WKFILNO                                                  
         BNE   RCHAE1                                                           
*                                                                               
RCHA3    LH    R1,SKLEN            MOVE RECORD OUT OF BUFFER                    
         LA    RF,WKFREC                                                        
         LR    RE,R5                                                            
         AH    RE,SKDISP                                                        
         SR    RE,R1                                                            
         MOVE  ((RF),(R1)),(RE)                                                 
         CLC   SKLEN,WKFREC        CHECK RECORD LOOKS OK                        
         BNE   RCHAE1                                                           
*                                                                               
RCHA4    MVI   FLAG,C'V'           VALIDATE HEX INPUT DATA                      
         BAS   RE,RDISP                                                         
         L     R4,FULL             TEST FOR INVALID HEX                         
         LTR   R4,R4                                                            
         BNZ   RCHAE5                                                           
         LA    R4,SRVED1H                                                       
         CLC   SKLEN,WKFREC        TEST FOR CHANGE IN RECORD LENGTH             
         BNE   RCHAE6                                                           
         LA    R4,SRVP1H                                                        
         GOTO1 CDATAMGR,DMCB,(X'00',=C'WRI'),WKFILE,WKFNDX,WKFREC,(R5)          
         CLI   DMCB+8,0                                                         
         BNE   RCHAE7                                                           
*                                                                               
RCHA5    MVI   FLAG,C'D'           REDISPLAY NEW RECORD DATA                    
         BAS   RE,RDISP                                                         
         MVC   MSG(33),=C'RECORD DATA CHANGED - RECLEN=NNNN'                    
         LH    R0,WKFREC                                                        
         CVD   R0,DUB                                                           
         UNPK  DUB(4),DUB+5(3)                                                  
         OI    DUB+3,X'F0'                                                      
         MVC   MSG+29(4),DUB                                                    
         MVC   SRVMSG,MSG                                                       
         OI    SRVMSGH+6,X'80'                                                  
         OI    SRVP1H+6,X'40'                                                   
         B     EXIT                                                             
         EJECT                                                                  
ERR1     MVC   MSG(14),=C'FILE NOT FOUND'                                       
         B     ERRX                                                             
ERR2     MVC   MSG(16),=C'RECORD NOT FOUND'                                     
         B     ERRX                                                             
ERR3     MVC   MSG(17),=C'FILE FORMAT ERROR'                                    
         B     ERRX                                                             
ERR4     MVC   MSG(15),=C'FILE DISK ERROR'                                      
         B     ERRX                                                             
ERR5     MVC   MSG(30),=C'RECORD START BYTE OUT OF RANGE'                       
         B     ERRX                                                             
RCHAE1   MVC   MSG(30),=C'NO VALID RECORD DATA DISPLAYED'                       
         B     ERRX                                                             
RCHAE2   MVC   MSG(29),=C'RECORD DEFINITION HAS CHANGED'                        
         B     ERRX                                                             
RCHAE3   MVC   MSG(27),=C'FILE DEFINITION HAS CHANGED'                          
         B     ERRX                                                             
RCHAE4   MVC   MSG(20),=C'FILE DISK READ ERROR'                                 
         B     ERRX                                                             
RCHAE5   MVC   MSG(19),=C'INVALID HEXADECIMAL'                                  
         B     ERRX                                                             
RCHAE6   MVC   MSG(25),=C'CANT CHANGE RECORD LENGTH'                            
         B     ERRX                                                             
RCHAE7   MVC   MSG(21),=C'FILE DISK WRITE ERROR'                                
         B     ERRX                                                             
               SPACE 2                                                          
ERRX     MVC   SRVMSG(12),=C'***ERROR*** '                                      
         MVC   SRVMSG+12(48),MSG                                                
ERRX1    OI    SRVMSGH+6,X'80'                                                  
         OI    6(R4),X'40'         POSN CURSOR TO ERROR FIELD                   
         B     EXIT                                                             
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
* SUBROUTINE TO DISPLAY/VALIDATE RECORD DATA - FLAG=D(DISP) OR V(VALI)          
*                                                                               
RDISP    NTR1                                                                   
         LA    R4,SRVED1H          R4=A(SCREEN LINE)                            
         USING WKSLD,R4                                                         
         LA    R5,WKFREC           R5=A(RECORD DATA)                            
         AH    R5,IFSTR                                                         
         LH    R6,IFEND            R6=LEN OF RECORD DATA                        
         LTR   R6,R6                                                            
         BNZ   *+10                                                             
         LH    R6,WKFREC                                                        
         BCTR  R6,0                                                             
         SH    R6,IFSTR                                                         
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
         LA    RE,WKFREC                                                        
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
         OC    WKSLB,WKSLB                                                      
         BZ    RDISP6A                                                          
         XC    WKSLB,WKSLB                                                      
         XC    WKSLH,WKSLH                                                      
         XC    WKSLA,WKSLA                                                      
         MVI   WKSLHH+5,0                                                       
         OI    WKSLBH+6,X'80'                                                   
         OI    WKSLHH+6,X'80'                                                   
         OI    WKSLAH+6,X'80'                                                   
         B     RDISP6A                                                          
*                                                                               
RDISPX   XIT1                                                                   
*                                                                               
RDISPT   DC    CL32'................................'                           
         DC    CL32'................................'                           
         DC    CL32' ...........<(+.&&..........$*);.'                          
         DC    CL32'-/.........,%.>............#@''="'                          
         DC    CL32'................................'                           
         DC    CL32'................................'                           
         DC    CL32'.ABCDEFGHI.......JKLMNOPQR......'                           
         DC    CL32'..STUVWXYZ......0123456789......'                           
         EJECT                                                                  
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
WKFILE   DC    CL8'ACCWRK'                                                      
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
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
*ACWRKWK                                                                        
       ++INCLUDE ACWRKWK                                                        
         EJECT                                                                  
ACWRKFFD DSECT                                                                  
         DS    CL64                                                             
*ACWRKFFD                                                                       
       ++INCLUDE ACWRKFFD                                                       
*                                                                               
         ORG   SRVFCAH                                                          
*ACWRKFED                                                                       
       ++INCLUDE ACWRKFED                                                       
         EJECT                                                                  
*DMWRKRD                                                                        
       ++INCLUDE DMWRKRD                                                        
         EJECT                                                                  
*DMWRKRS                                                                        
       ++INCLUDE DMWRKRS                                                        
         EJECT                                                                  
*DDCOMFACS                                                                      
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
*DDFLDHDR                                                                       
       ++INCLUDE DDFLDHDR                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACWRK02   05/01/02'                                      
         END                                                                    
