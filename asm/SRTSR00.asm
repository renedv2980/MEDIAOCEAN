*          DATA SET SRTSR00    AT LEVEL 029 AS OF 10/03/13                      
*PHASE T13F00A                                                                  
         SPACE 2                                                                
*=======================================================*                       
* TO MAKE THIS REALLY USEFUL, SHOULD DO AN AUTOMATIC    *                       
* RESTORE BEFORE ANY COMMAND AND A SAVE AFTER IT        *                       
*=======================================================*                       
         SPACE 2                                                                
*=================================================================*             
* REMEMBER THAT THE CRITICAL FIELDS THAT MUST BE SUPPLIED BY USER *             
* ARE TSPAGL ==> LOW PAGE NUMBER                                  *             
*     TSPAGN ==> NUMBER OF PAGES                                  *             
*     TSINDS ==> TSIALLOC BIT (USE TEMPEST)                       *             
* AND TO TELL THE USERS THAT BUFFERS ARE NO LONGER NEEDED         *             
*=================================================================*             
         TITLE '$TSR - TEST PROGRAM FOR TSARDINE'                               
TSARTREK CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKX-WRKD,TSARTREK,RR=RE,CLEAR=YES                               
         USING WRKD,RC                                                          
         ST    RE,RELO             SAVE PROGRAM RELOCATION FACTOR               
         ST    RD,BASERD                                                        
*                                                                               
         USING SRPARMD,R1                                                       
         MVC   SRPARS,0(R1)                                                     
         DROP  R1                                                               
*                                                                               
         L     RA,SRPAR6           A(TWA)                                       
         USING SRTSRFFD,RA                                                      
         LA    R2,TSRMSGH                                                       
         OI    6(R2),X'80'                                                      
         MVC   8(8,R2),=C'TSARTREK'                                             
*                                                                               
         L     R4,SRPAR1           A(SYSFAC)                                    
         USING SYSFACD,R4                                                       
*                                                                               
         L     R4,SRPAR4                                                        
         USING COMFACSD,R4                                                      
         MVC   ACALLOV,CCALLOV                                                  
         MVC   AHEXOUT,CHEXOUT                                                  
         MVC   ASWITCH,CSWITCH                                                  
         DROP  R4                                                               
*                                                                               
         XC    DMCB,DMCB                                                        
         GOTO1 ACALLOV,DMCB,,X'D9000A5D'                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ATSAR,0(R1)                                                      
*                                                                               
         MVC   XXXKEY,=C'*KEY*KEY'                                              
         MVC   XXXDATA,=C'DATADATA'                                             
         MVC   XXXTSAR,=C'TSARTSAR'                                             
         MVC   XXXIOA,=C'*IOA*IOA'                                              
         MVC   XXXBHDR,=C'BHDRBHDR'                                             
*                                                                               
         LA    R9,TSARBUFF                                                      
         USING TSARD,R9                                                         
         LA    RE,IOA                                                           
         ST    RE,TSAREC           SET ADDRESS OF IOA                           
         MVC   TSACOM,SRPAR4       SET ADDRESS OF COMFACS                       
*========>                                                                      
         OI    TSIND2,TSI2DATA     GET BUFFER DATA BACK                         
*========>                                                                      
         EJECT                                                                  
         LA    RE,TSARBHDR                                                      
         ST    RE,TSABUF           TSAR BUFFER DATA RETURNED HERE               
*                                                                               
         SR    R0,R0                                                            
         LA    R2,TSRMSGH          FORCE ALL UNP FLDHDRS                        
*                                                                               
TSR2     TM    1(R2),X'20'         TEST PROT                                    
         BO    *+12                                                             
         OI    1(R2),X'01'         FORCE MODIFY                                 
         MVI   6(R2),X'80'         AND XMT                                      
*                                                                               
         ICM   R0,1,0(R2)                                                       
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   TSR2                                                             
*                                                                               
         XC    DMCB,DMCB                                                        
         GOTO1 ASWITCH,DMCB,X'00FFFFFF'  GET ADDRESS OF TCB                     
         L     RE,0(R1)                                                         
         CLI   6(RE),C'1'          TEST TASK 1                                  
         BNE   BADTASK                                                          
         L     RF,TCBTSAR-TCBD(RE)                                              
         A     RF,=A(14*1024)                                                   
         ST    RF,SVBUFFAD                                                      
         LA    RE,SETTSRX                                                       
         O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
SETTSRX  MVC   0(4,RF),=C'TSRX'                                                 
         MVC   4(28,RF),0(RF)                                                   
         LA    RE,*+6                                                           
         BSM   0,RE                                                             
*                                                                               
         LA    R2,TSRFILH                                                       
         CLI   5(R2),0                                                          
         BNE   TSR3A                                                            
         MVI   5(R2),7                                                          
         MVC   8(7,R2),=C'TEMPSTR'                                              
         LA    R2,TSRRECTH                                                      
         MVI   5(R2),1                                                          
         MVI   8(R2),C'F'                                                       
         LA    R2,TSRKEYLH                                                      
         MVI   4(R2),X'08'                                                      
         MVI   5(R2),2                                                          
         MVC   8(2,R2),=C'10'                                                   
         LA    R2,TSRRECLH                                                      
         MVI   4(R2),X'08'                                                      
         MVI   5(R2),2                                                          
         MVC   8(2,R2),=C'32'                                                   
         B     TSR4                                                             
*                                                                               
TSR3A    CLC   =C'EF',8(R2)                                                     
         BNE   TSR3B                                                            
         MVI   5(R2),7                                                          
         MVC   8(7,R2),=C'TEMPEST'                                              
         LA    R2,TSRPGLH                                                       
         MVI   4(R2),X'08'                                                      
         MVI   5(R2),1                                                          
         MVI   8(R2),C'1'                                                       
         LA    R2,TSRPGNH                                                       
         MVI   4(R2),X'08'                                                      
         MVI   5(R2),2                                                          
         MVC   8(2,R2),=C'12'                                                   
         LA    R2,TSRRECTH                                                      
         MVI   5(R2),1                                                          
         MVI   8(R2),C'F'                                                       
         LA    R2,TSRKEYLH                                                      
         MVI   4(R2),X'08'                                                      
         MVI   5(R2),2                                                          
         MVC   8(2,R2),=C'10'                                                   
         LA    R2,TSRRECLH                                                      
         MVI   4(R2),X'08'                                                      
         MVI   5(R2),2                                                          
         MVC   8(2,R2),=C'32'                                                   
*                                                                               
TSR3B    DS    0H                                                               
*                                                                               
TSR4     LA    R2,TSRFILH          FILE NAME                                    
         CLC   =C'TEMPSTR',8(R2)                                                
         BE    TSR10                                                            
         OI    TSINDS,TSIALLOC     SET TO USE TEMPEST                           
         CLC   =C'TEMPEST',8(R2)                                                
         BE    TSR10                                                            
         B     INVALID                                                          
         EJECT                                                                  
TSR10    LA    R2,TSRPGLH          LOW PAGE NUMBER                              
         CLC   =C'TEMPSTR',TSRFIL  TEST USING TEMPSTR                           
         BNE   TSR20               NO                                           
         MVI   TSPAGL,1            SET TO USE PAGE 1                            
         MVI   TSPAGN,1            AND ASSUME 1 PAGE                            
*                                                                               
         LA    R2,TSRPGNH          NUMBER OF PAGES                              
         CLI   5(R2),0             TEST ENTERED                                 
         BE    TSR40               NO                                           
         BAS   RE,GETNUM                                                        
         CH    R0,=H'4'            MAX 4 PAGES                                  
         BH    INVALID                                                          
         STC   R0,TSPAGN                                                        
         B     TSR40               ELSE CONTINUE                                
*                                                                               
TSR20    CLI   5(R2),0             TEST LOW PAGE ENTERED                        
         BNE   TSR22               YES                                          
         CLI   TSRACTN,C'I'        ELSE ACTION BETTER BE INIT                   
         BNE   INVALID                                                          
*                                                                               
TSR22    BAS   RE,GETNUM                                                        
         STC   R0,TSPAGL                                                        
*                                                                               
TSR30    LA    R2,TSRPGNH          NUMBER OF PAGES                              
         BAS   RE,GETNUM                                                        
         STC   R0,TSPAGN                                                        
         CH    R0,=H'12'                                                        
         BH    INVALID                                                          
         OI    TSINDS,TSIXTTWA                                                  
*                                                                               
TSR40    LA    R2,TSRRECTH         RECORD TYPE                                  
         CLI   8(R2),C'F'                                                       
         BE    TSR50                                                            
         OI    TSRECI,TSRVAR                                                    
         CLI   8(R2),C'V'                                                       
         BE    TSR50                                                            
         B     INVALID                                                          
*                                                                               
TSR50    LA    R2,TSRKEYLH         KEY LENGTH                                   
         CLI   5(R2),0                                                          
         BE    INVALID                                                          
         BAS   RE,GETNUM                                                        
         STC   R0,TSKEYL                                                        
         CH    R0,=H'255'                                                       
         BH    INVALID                                                          
*                                                                               
TSR60    LA    R2,TSRRECLH                                                      
         BAS   RE,GETNUM                                                        
         STH   R0,TSRECL                                                        
         EJECT                                                                  
TSR70    LA    R2,TSRACTNH                                                      
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         LA    R1,ACTTAB                                                        
         LA    R0,(ACTTABX-ACTTAB)/L'ACTTAB                                     
*                                                                               
TSR72    EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),0(R1)                                                    
         BE    TSR74                                                            
         LA    R1,L'ACTTAB(R1)                                                  
         BCT   R0,TSR72                                                         
         B     INVALID                                                          
*                                                                               
TSR74    L     RF,4(R1)                                                         
         A     RF,RELO                                                          
         BASR  RE,RF               NO RETURN EXPECTED BUT HATE BR               
         DC    H'0'                                                             
         DS    0D                                                               
ACTTAB   DS    0XL8                                                             
         DC    CL4'INI ',AL4(TSRINI)                                            
         DC    CL4'ADD ',AL4(TSRADD)                                            
**NOP    DC    CL4'WRT ',AL4(TSRWRT)                                            
         DC    CL4'PUT ',AL4(TSRPUT)                                            
         DC    CL4'DEL ',AL4(TSRDEL)                                            
         DC    CL4'RDH ',AL4(TSRRDH)                                            
         DC    CL4'GET ',AL4(TSRGET)                                            
         DC    CL4'SAV ',AL4(TSRSAV)                                            
         DC    CL4'RES ',AL4(TSRRES)                                            
ACTTABX  EQU   *                                                                
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
GETNUM   NTR1                                                                   
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BZ    INVALID                                                          
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
         XIT1  REGS=(R0)                                                        
*                                                                               
GETKEY   NTR1                                                                   
         LA    R2,TSRKEYH                                                       
         CLI   5(R2),0                                                          
         BE    INVALID                                                          
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8              MOVE TO KEY AREA                             
         B     *+10                                                             
         MVC   KEY(0),8(R2) *EXECUTED*                                          
* NOW PROPAGATE THROUGH ENTIRE KEY                                              
         ZIC   R0,5(R2)            GET DATA LENGTH ENTERED                      
         LA    RE,KEY                                                           
         AR    RE,R0               SET 'TO' ADDRESS                             
         LA    RF,L'KEY                                                         
         SR    RF,R0                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),KEY                                                      
         B     EXIT                                                             
         EJECT                                                                  
GETDATA  NTR1                                                                   
         LA    R2,TSRDATAH                                                      
         CLI   5(R2),0                                                          
         BE    INVALID                                                          
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8              MOVE TO DATA                                 
         B     *+10                                                             
         MVC   DATA,8(R2) *EXECUTED*                                            
* NOW PROPAGATE THROUGH ENTIRE DATA                                             
         ZIC   R0,5(R2)            GET DATA LENGTH ENTERED                      
         LA    RE,DATA                                                          
         AR    RE,R0               SET 'TO' ADDRESS                             
         LA    RF,256                                                           
         SR    RF,R0                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),DATA                                                     
         MVC   DATA+256(256),DATA                                               
* NEED DATA LENGTH FOR VARIABLE LEN RECORDS                                     
         TM    TSRECI,TSRVAR       TEST VARIABLE LEN RECORDS                    
         BZ    EXIT                                                             
         LA    R2,TSRDATLH                                                      
         CLI   5(R2),0                                                          
         BE    INVALID                                                          
         BAS   RE,GETNUM                                                        
         STH   R0,DATALEN                                                       
         B     EXIT                                                             
*                                                                               
INVALID  OI    6(R2),X'40'         POSITION CURSOR                              
         MVC   TSRMSG(38),=C'THIS INPUT IS NOT TOO GOOD - YOU IDIOT'            
         L     RD,BASERD                                                        
         XIT1                                                                   
*                                                                               
NOTFOUND OI    6(R2),X'40'         POSITION CURSOR                              
         MVC   TSRMSG(38),=C'SORRY - REQUESTED RECORD IS NOT THERE '            
         L     RD,BASERD                                                        
         XIT1                                                                   
BADTASK  LA    R2,TSRDATLH                                                      
         OI    6(R2),X'40'         POSITION CURSOR                              
         MVC   TSRMSG(22),=C'WRONG TASK - TRY AGAIN'                            
         L     RD,BASERD                                                        
         XIT1                                                                   
         EJECT                                                                  
TSRINI   DS    0H                                                               
         MVI   TSACTN,TSAINI                                                    
         GOTO1 ATSAR,TSARD                                                      
         TM    TSINDS,TSIINIOK                                                  
         BO    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DSPBHDR                                                       
         B     TSRX                                                             
*                                                                               
TSRADD   BAS   RE,GETKEY                                                        
         BAS   RE,GETDATA                                                       
         BAS   RE,BLDIOA                                                        
*                                                                               
         MVI   TSACTN,TSAADD                                                    
         GOTO1 ATSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DSPBHDR                                                       
         B     TSRX                                                             
*                                                                               
TSRRDH   BAS   RE,GETKEY                                                        
         BAS   RE,BLDIOA                                                        
*                                                                               
         MVI   TSACTN,TSARDH                                                    
         GOTO1 ATSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    TSRRDH2                                                          
         CLI   TSERRS,TSERNF                                                    
         BE    NOTFOUND                                                         
         DC    H'0'                                                             
TSRRDH2  BAS   RE,DSPBHDR                                                       
         BAS   RE,DSPIOA                                                        
         B     TSRX                                                             
*                                                                               
TSRGET   LA    R2,TSRKEYH                                                       
         BAS   RE,GETNUM                                                        
         STH   R0,TSRNUM                                                        
*                                                                               
         MVI   TSACTN,TSAGET                                                    
         GOTO1 ATSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DSPBHDR                                                       
         BAS   RE,DSPIOA                                                        
         B     TSRX                                                             
*                                                                               
TSRPUT   DS    0H                                                               
         BAS   RE,GETKEY                                                        
         BAS   RE,BLDIOA                                                        
*                                                                               
         MVI   TSACTN,TSAGET                                                    
         GOTO1 ATSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 ATSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DSPBHDR                                                       
         BAS   RE,DSPIOA                                                        
         B     TSRX                                                             
*                                                                               
TSRDEL   BAS   RE,GETKEY                                                        
         BAS   RE,BLDIOA                                                        
*                                                                               
         MVI   TSACTN,TSARDH                                                    
         GOTO1 ATSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TSACTN,TSADEL                                                    
         GOTO1 ATSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,DSPBHDR                                                       
         BAS   RE,DSPIOA                                                        
         B     TSRX                                                             
*                                                                               
TSRX     LA    R2,TSRACTNH         POINT TO ACTION                              
         OI    6(R2),X'40'         INSERT CURSOR                                
*                                                                               
         LA    RE,TSRXX                                                         
         O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
TSRXX    L     RF,SVBUFFAD                                                      
         CLC   0(4,RF),=C'TSRX'                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,EXIT                                                          
         BSM   0,RE                                                             
         EJECT                                                                  
TSRSAV   DS    0H                                                               
         MVI   TSACTN,TSASAV                                                    
         GOTO1 ATSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DSPBHDR                                                       
         B     EXIT                                                             
TSRRES   DS    0H                                                               
         MVI   TSACTN,TSARES                                                    
         GOTO1 ATSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DSPBHDR                                                       
         B     EXIT                                                             
         EJECT                                                                  
* FIXED LENGTH FORMAT IS KEY/DATA                                               
BLDIOA   NTR1                                                                   
         TM    TSRECI,TSRVAR                                                    
         BO    BLDIOA10                                                         
         ZIC   RE,TSKEYL                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   IOA(0),KEY                                                       
*                                                                               
         ZIC   R0,TSKEYL                                                        
         LA    R1,IOA                                                           
         AR    R0,R1               GET 'TO' ADDRESS                             
         LA    R1,512              SET 'TO' LEN TO MAX POSSIBLE                 
         LA    RE,DATA             SET 'FROM' ADDRESS                           
         LR    RF,R1               SET 'FROM' LEN                               
         MVCL  R0,RE                                                            
         B     EXIT                                                             
*                                                                               
* VARIABLE LEN RECORD FORMAT IS DATALEN(2)/KEY/DATA                             
*                                                                               
BLDIOA10 MVC   IOA(2),DATALEN                                                   
         ZIC   RE,TSKEYL                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   IOA+2(0),KEY                                                     
*                                                                               
         OC    DATALEN,DATALEN     TEST NO DATALEN                              
         BZ    EXIT                                                             
         ZIC   R0,TSKEYL                                                        
         LA    R1,IOA+2                                                         
         AR    R0,R1               GET 'TO' ADDRESS                             
         LA    R1,512              SET 'TO' LEN TO MAX POSSIBLE                 
         LA    RE,DATA             SET 'FROM' ADDRESS                           
         LR    RF,R1               SET 'FROM' LEN                               
         MVCL  R0,RE                                                            
         B     EXIT                                                             
         EJECT                                                                  
DSPIOA   NTR1                                                                   
         LA    R2,TSRKEYH                                                       
         BAS   RE,DSPXC            RF CONTAINS FLDLEN-1 ON EXIT                 
* SET LENGTH TO SMALLER OF FIELD OR KEY LENGTH                                  
         ZIC   RE,TSKEYL                                                        
         BCTR  RE,0                                                             
         CR    RE,RF                                                            
         BL    *+6                                                              
         LR    RE,RF                                                            
*                                                                               
         LA    R1,IOA                                                           
         TM    TSRECI,TSRVAR                                                    
         BZ    *+8                                                              
         LA    R1,2(R1)            VARIABLE LEN REC                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(R1)                                                    
*                                                                               
         LA    R2,TSRDATLH                                                      
         BAS   RE,DSPXC                                                         
         LA    R2,TSRDATAH                                                      
         BAS   RE,DSPXC                                                         
*                                                                               
         LA    RE,IOA                                                           
         TM    TSRECI,TSRVAR                                                    
         BZ    DSPIOA10                                                         
* DISPLAY DATA LENGTH FOR VARIABLE LENGTH RECORDS                               
         LA    R2,TSRDATLH                                                      
         LH    R0,0(RE)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(8,R2),DUB                                                      
         OI    6(R2),X'80'                                                      
         LA    RE,2(RE)            POINT TO KEY                                 
*                                                                               
DSPIOA10 DS    0H                                                               
         ZIC   R0,TSKEYL           GET KEY LENGTH                               
         AR    RE,R0               POINT RE TO DATA                             
*                                                                               
         LA    R2,TSRDATAH                                                      
         ZIC   R0,0(R2)                                                         
         SH    R0,=H'8'            GET SCREEN FIELD LENGTH                      
         LH    RF,TSRECL           GET RECLEN OF FIXED LEN REC                  
         TM    TSRECI,TSRVAR       TEST VARIABLE LEN                            
         BZ    *+8                                                              
         LH    RF,IOA              GET RECORD LENGTH FROM IOA                   
         CR    RF,R0                                                            
         BL    *+6                                                              
         LR    RF,R0               USE SMALLER LEN                              
*                                                                               
         BCTR  RF,0                SET FOR EX                                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(RE)                                                    
         B     EXIT                                                             
*                                                                               
DSPXC    DS    0H                                                               
         ZIC   RF,0(R2)                                                         
         SH    RF,=H'9'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         BR    RE                                                               
DSPBHDR  NTR1                                                                   
         LA    R2,TSRPGLH                                                       
         ZIC   R0,TSPAGL                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(2,R2),DUB                                                      
*                                                                               
         LA    R2,TSRPGNH                                                       
         ZIC   R0,TSPAGN                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(2,R2),DUB                                                      
*                                                                               
         LA    R2,TSROUT1H                                                      
         L     R4,TSABUF           GET TSAR BUFFER ADDRESS                      
         LA    R0,160(R4)                                                       
         ST    R0,DUB              SAVE BUFFER END                              
*                                                                               
DSPBH2   ST    R2,FULL                                                          
         LA    R2,8(R2)                                                         
         LA    R0,8                SET FOR 32 BYTES/LINE                        
*                                                                               
DSPBH4   DS    0H                                                               
         GOTO1 AHEXOUT,DMCB,(R4),(R2),4,=C'TOG'                                 
         LA    R2,9(R2)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,DSPBH4                                                        
*                                                                               
         L     R2,FULL                                                          
         OI    6(R2),X'80'         SET TO XMT                                   
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         C     R4,DUB                                                           
         BL    DSPBH2                                                           
         XIT1                                                                   
*                                                                               
WRKD     DSECT                                                                  
BASERD   DS    A                                                                
RELO     DS    A                                                                
*                                                                               
SRPARS   DS    0CL24                                                            
SRPAR1   DS    A                                                                
SRPAR2   DS    A                                                                
SRPAR3   DS    A                                                                
SRPAR4   DS    A                                                                
SRPAR5   DS    A                                                                
SRPAR6   DS    A                                                                
*                                                                               
*                                                                               
DMCB     DS    0CL24                                                            
DM1      DS    A                                                                
DM2      DS    A                                                                
DM3      DS    A                                                                
DM4      DS    A                                                                
DM5      DS    A                                                                
DM6      DS    A                                                                
*                                                                               
AHEXOUT  DS    A                                                                
ACALLOV  DS    A                                                                
ATSAR    DS    A                                                                
ASWITCH  DS    A                                                                
SVBUFFAD DS    A                                                                
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
WORK     DS    CL24                                                             
DATALEN  DS    H                                                                
*                                                                               
XXXKEY   DS    D                                                                
KEY      DS    CL64                                                             
XXXDATA  DS    D                                                                
DATA     DS    512C                                                             
DATAX    EQU   *                                                                
XXXTSAR  DS    D                                                                
TSARBUFF DS    768C                                                             
XXXBHDR  DS    D                                                                
TSARBHDR DS    256C                DATA FROM TSAR BUFFER HEADER                 
XXXIOA   DS    D                                                                
IOA      DS    1024C                                                            
WRKX     EQU   *                                                                
         EJECT                                                                  
SRTSRFFD DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE SRTSRFFD                                                       
         EJECT                                                                  
* DDTSARD                                                                       
       ++INCLUDE DDTSARD                                                        
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029SRTSR00   10/03/13'                                      
         END                                                                    
