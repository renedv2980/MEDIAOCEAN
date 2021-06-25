*          DATA SET T00A08     AT LEVEL 032 AS OF 05/01/02                      
*PHASE T00A08A,+0,NOAUTO                                                        
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
         TITLE 'PAV FILE UPGRADE ROUTINES'                                      
T00A08   CSECT                                                                  
BASE     DS    6000C                                                            
         ORG   BASE                                                             
         PRINT NOGEN                                                            
         NMOD1 200,T00A08                                                       
         USING DEMUPW,RC                                                        
         L     R2,4(R1)                                                         
         USING RAVLUEL,R2                                                       
         MVC   AWORK,0(R1)                                                      
         L     RF,=V(DEMEX)        CHECK IF ONLINE                              
         LTR   RF,RF                                                            
         BZ    ONLINE               YES                                         
         ST    RF,VDEMEX            NO - SET V TYPE CONSTANTS                   
         L     RF,=V(PAVEXPL)                                                   
         ST    RF,VPAVEXPL                                                      
         L     RF,=V(PAVCOND)                                                   
         ST    RF,VPAVCOND                                                      
         B     OFFLINE                                                          
         SPACE 2                                                                
ONLINE   MVC   VCALLOV,8(R1)                                                    
         CLI   0(R2),X'05'                                                      
         BNE   *+14                                                             
         L     RF,8(R1)                                                         
         MVC   VCALLOV,4(RF)                                                    
         OC    VCALLOV,VCALLOV                                                  
         BNZ   *+6                                                              
         DC    H'0'                NEED ADDRESS OF CALL OVERLAY                 
         MVC   UPDMCB+4(4),=X'D9000A05'                                         
         GOTO1 VCALLOV,UPDMCB,0                                                 
         L     RF,UPDMCB                                                        
         ST    RF,VDEMEX                                                        
         MVC   UPDMCB+4(4),=X'D9000A0A'                                         
         GOTO1 VCALLOV,UPDMCB,0                                                 
         L     RF,UPDMCB                                                        
         ST    RF,VPAVCOND                                                      
         MVC   UPDMCB+4(4),=X'D9000A0B'                                         
         GOTO1 VCALLOV,UPDMCB,0                                                 
         L     RF,UPDMCB                                                        
         ST    RF,VPAVEXPL                                                      
         L     RF,=V(HELLO)                                                     
         AR    RF,RB                                                            
         ST    RF,VHELLO                                                        
         SPACE 2                                                                
OFFLINE  ST    R2,AUPELEM          SAVE PARAMETERS                              
         MVC   AFRSTEL,AWORK                                                    
         SPACE 2                                                                
         L     RF,AWORK            GET START OF RECORD                          
         SH    RF,=H'34'                                                        
         ST    RF,ARECORD                                                       
         MVC   FILNAM,=C'REPFILE'                                               
         CLI   0(R2),X'1F'                                                      
         BL    FILCHK                                                           
         L     RF,AWORK                                                         
         SH    RF,=H'16'                                                        
         MVC   FILNAM,=C'PAVFL'                                                 
         ST    RF,ARECORD                                                       
         SPACE 2                                                                
FILCHK   CLI   0(RF),X'12'         CHECK RECORD IDENTIFIER                      
         BE    FILOK                                                            
         CLI   0(RF),C'P'                                                       
         BE    FILOK                                                            
         DC    H'0'                UNKNOWN RECORD                               
         SPACE 2                                                                
FILOK    DS    0C                                                               
         LA    RF,WORKREC                                                       
         ST    RF,AWORK                                                         
         L     RE,AWORK                                                         
         L     RF,=F'550'                                                       
         XCEF                                                                   
         MVI   SRC,C'E'                                                         
         MVC   BOOK,=X'4E01'                                                    
HAVSRC   L     R6,AFRSTEL                                                       
         L     R7,AWORK                                                         
         GOTO1 VPAVEXPL,UPDMCB,SRC,(R6),(R7)                                    
         EJECT                                                                  
         LA    RF,UPINF1                                                        
         XC    0(16,RF),0(RF)      SET INPUT FIELDS                             
         ZIC   RE,RAVLULEN                                                      
         AR    RE,R2                                                            
         LA    R9,RAVLUOPS                                                      
         CLI   0(R2),5                                                          
         BNE   *+8                                                              
         LA    R2,2(R2)                                                         
INLOOP   MVC   2(2,RF),0(R9)       DECODE ELEMENT                               
         LA    RF,4(RF)                                                         
         LA    R9,2(R9)                                                         
         CR    R9,RE                                                            
         BL    INLOOP                                                           
         CLI   RAVLUCAT,0                                                       
         BNE   INDEMO                                                           
         GOTO1 VDEMEX,UPDMCB,(C'U',WORKREC),(1,UNIV)                            
         CLI   RAVLUTYP,1                                                       
         BNE   *+8                                                              
         BAS   RE,HOMES            HOMES FACTORS                                
         CLI   RAVLUTYP,2                                                       
         BNE   *+8                                                              
         BAS   RE,RTG              RATING FACTORS                               
         CLI   RAVLUTYP,3                                                       
         BNE   *+8                                                              
         BAS   RE,HUT              HUT FACTORS                                  
         CLI   RAVLUTYP,4                                                       
         BNE   *+8                                                              
         BAS   RE,INDEX            INDEX GIVEN                                  
         CLI   RAVLUTYP,5                                                       
         BE    EXITX                                                            
INLOOP1  XC    ADJLIST(255),ADJLIST                                             
         MVC   ADJLIST(8),ADJALL                                                
         EJECT                                                                  
         BAS   RE,ADJ              FACTORS SET - NOW ADJUST RECORD              
         L     R6,AWORK                                                         
         GOTO1 VPAVCOND,UPDMCB,SRC,(R6),OUTEL                                   
         L     R6,AFRSTEL                                                       
DELEL    CLI   0(R6),0                                                          
         BE    DELDONE                                                          
         CLI   0(R6),X'30'                                                      
         BL    DELEL1                                                           
         CLI   0(R6),X'60'                                                      
         BH    DELEL1                                                           
         ZIC   R7,0(R6)                                                         
         L     R8,ARECORD                                                       
         GOTO1 VHELLO,UPDMCB,(C'D',FILNAM),((R7),(R8)),0                        
         B     DELEL                                                            
DELEL1   ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         B     DELEL                                                            
DELDONE  DS    0H                                                               
         SPACE 2                                                                
         LA    R9,OUTEL                                                         
         L     R8,ARECORD                                                       
ADDEL    CLI   0(R9),0                                                          
         BE    EXIT                                                             
         GOTO1 VHELLO,UPDMCB,(C'P',FILNAM),(0,(R8)),(R9)                        
         ZIC   RF,1(R9)                                                         
         AR    R9,RF                                                            
         B     ADDEL                                                            
EXIT     L     R8,ARECORD                                                       
         CLI   FILNAM,C'R'                                                      
         BNE   EXIT2                                                            
         USING RAVLREC,R8                                                       
         MVC   UPDMCB(2),RAVLLEN                                                
         LH    RF,UPDMCB                                                        
         BCTR  RF,0                                                             
         STH   RF,UPDMCB                                                        
         MVC   RAVLLEN,UPDMCB                                                   
         B     EXITX                                                            
         DROP  R8                                                               
         SPACE 2                                                                
EXIT2    CLI   FILNAM,C'P'                                                      
         BNE   EXIT3                                                            
         USING PRKEY,R8                                                         
         MVC   UPDMCB(2),PRRLEN                                                 
         LH    RF,UPDMCB                                                        
         BCTR  RF,0                                                             
         STH   RF,UPDMCB                                                        
         MVC   PRRLEN,UPDMCB                                                    
         DROP  R8                                                               
EXIT3    DS    0H                                                               
EXITX    XMOD1 1                                                                
ADJALL   DC    X'01',AL1(88),AL2(PWIK611-PWDATA)                                
         DC    4X'00'                                                           
         EJECT                                                                  
* DECODE DEMO INPUT STREAM                                                      
INDEMO   MVC   DEMONO,RAVLUCAT                                                  
         MVC   DEMOTYP,RAVLUTYP                                                 
         ZIC   R9,DEMONO                                                        
         GOTO1 VDEMEX,UPDMCB,(C'U',WORKREC),((R9),UNIV)                         
         BAS   RE,DEMO                                                          
         B     INLOOP1                                                          
         SPACE 2                                                                
*              CONVERT PERCENTAGES INTO VIEWERS                                 
VWR      L     RF,0(R1)                                                         
         SR    RE,RE                                                            
         M     RE,UNIV                                                          
         SLDA  RE,1                                                             
         D     RE,=F'100'                                                       
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,0(R1)                                                         
         BR    R9                                                               
         EJECT                                                                  
HUT      NTR1                                                                   
         L     RF,UPINF2                                                        
         MH    RF,=H'10'                                                        
         ST    RF,UPINF2                                                        
         GOTO1 VDEMEX,UPDMCB,(C'M',WORKREC),(1,UPFULL)                          
         MVC   UPI1,=F'1000'                                                    
         MVC   UPI2(12),UPI1                                                    
         OC    UPFULL,UPFULL                                                    
         BZ    HUTX                                                             
         OC    UPINF1,UPINF1                                                    
         BZ    HUT3                                                             
         MVC   UPFULL2,UPINF1                                                   
         LA    R1,UPFULL2                                                       
         BAS   R9,VWR                                                           
         L     RF,UPFULL2                                                       
         SR    RE,RE                                                            
         M     RE,=F'1000'                                                      
         SLDA  RE,1                                                             
         D     RE,UPFULL                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,UPI1                                                          
         MVC   UPI2(12),UPI1                                                    
HUT2     OC    UPINF2,UPINF2                                                    
         BZ    HUTX                                                             
         LA    R1,UPINF1                                                        
         BAS   R9,VWR                                                           
         L     RF,UPINF1                                                        
         SR    RE,RE                                                            
         M     RE,UPINF2                                                        
         MH    RF,=H'10'           SET DECIMAL PRECISION                        
         SR    RE,RE                                                            
         ST    RF,UPFULL                                                        
         GOTO1 VDEMEX,UPDMCB,(C'Y',WORKREC),(1,UPFULL2)                         
         OC    UPFULL2,UPFULL2                                                  
         BZ    HUTX                                                             
         LA    R1,UPFULL                                                        
         SR    RE,RE                                                            
         L     RF,UPFULL                                                        
         SLDA  RE,1                                                             
         D     RE,UPFULL2                                                       
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         ST    RF,UPI1                                                          
         ST    RF,UPI2                                                          
         ST    RF,UPI4                                                          
         B     HUTX                                                             
         SPACE 2                                                                
*              GET EXISTING HUTS                                                
HUT3     GOTO1 VDEMEX,UPDMCB,(C'P',WORKREC),(1,UPINF1)                          
         B     HUT2                                                             
HUTX     XIT1                                                                   
         EJECT                                                                  
INDEX    NTR1                                                                   
         MVC   UPI1,UPINF1                                                      
         L     RF,UPI1                                                          
         MH    RF,=H'10'                                                        
         ST    RF,UPI1                                                          
         MVC   UPI2(12),UPI1                                                    
         XIT1                                                                   
         EJECT                                                                  
RTG      NTR1                                                                   
         L     RF,UPINF1                                                        
         MH    RF,=H'10'                                                        
         ST    RF,UPINF1                                                        
         GOTO1 VDEMEX,UPDMCB,(C'A',WORKREC),(1,UPFULL)                          
         MVC   UPI1,=F'1000'                                                    
         MVC   UPI2(12),UPI1                                                    
         OC    UPFULL,UPFULL                                                    
         BZ    RTGX                                                             
         OC    UPINF1,UPINF1                                                    
         BZ    RTGX                                                             
         OC    UPINF2,UPINF2                                                    
         BNZ   RTG1                                                             
         LA    R1,UPFULL                                                        
         LA    R1,UPINF1                                                        
         BAS   R9,VWR                                                           
         L     RF,UPINF1                                                        
         SR    RE,RE                                                            
         M     RE,=F'10'                                                        
         SLDA  RE,1                                                             
         D     RE,UPFULL                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,UPI1                                                          
         MVC   UPI2(12),UPI1                                                    
         SPACE 2                                                                
* CHECK FOR SHARE INPUT                                                         
RTG1     OC    UPINF2,UPINF2                                                    
         BZ    RTGX                                                             
         L     RF,UPINF1                                                        
         SR    RE,RE                                                            
         M     RE,=F'10'                                                        
         SLDA  RE,1                                                             
         D     RE,UPINF2                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,UPINF1                                                        
         BAS   RE,HUT                                                           
RTGX     XIT1                                                                   
         EJECT                                                                  
DEMO     NTR1                                                                   
         CLI   DEMOTYP,C'R'                                                     
         BE    DEMO1                                                            
         CLI   DEMOTYP,C'S'                                                     
         BE    DEMO1                                                            
         B     DEMO2                                                            
DEMO1    DS    0C                  ADJUST FOR DECIMAL PRECISION                 
         L     RF,UPINF1                                                        
         MH    RF,=H'10'                                                        
         ST    RF,UPINF1                                                        
DEMO2    DS    0C                                                               
         ZIC   R9,DEMONO                                                        
         IC    R8,DEMOTYP                                                       
         GOTO1 VDEMEX,UPDMCB,((R8),WORKREC),((R9),UPFULL)                       
         MVC   UPI1,=F'1000'                                                    
         MVC   UPI2(12),UPI1                                                    
         OC    UPFULL,UPFULL                                                    
         BZ    DEMOX                                                            
         OC    UPINF1,UPINF1                                                    
         BZ    DEMOX                                                            
         L     RF,UPINF1                                                        
         SR    RE,RE                                                            
         M     RE,=F'1000'                                                      
         SLDA  RE,1                                                             
         D     RE,UPFULL                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,UPI1                                                          
         MVC   UPI2(12),UPI1                                                    
         SPACE 2                                                                
DEMOX    XIT1                                                                   
         EJECT                                                                  
HOMES    NTR1                                                                   
         L     RF,UPINF2                                                        
         MH    RF,=H'10'                                                        
         ST    RF,UPINF2                                                        
         L     RF,UPINF3                                                        
         MH    RF,=H'10'                                                        
         ST    RF,UPINF3                                                        
         GOTO1 VDEMEX,UPDMCB,(C'T',WORKREC),(1,UPFULL)                          
         GOTO1 VDEMEX,UPDMCB,(C'A',WORKREC),(1,UPFULL2)                         
         L     RF,UPFULL           CALCULATE CURRENT TSA/ADI RATIO              
         SR    RE,RE                                                            
         M     RE,=F'200'                                                       
         D     RE,UPFULL2                                                       
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,UPFULL                                                        
         L     RF,UPINF1           TSA FACTOR                                   
         SR    RE,RE                                                            
         MVC   UPI1,=F'1000'                                                    
         MVC   UPI2(12),UPI1                                                    
         L     R8,UPFULL                                                        
         LTR   R8,R8                                                            
         BZ    HOMES1                                                           
         M     RE,=F'1000'                                                      
         SLDA  RE,1                                                             
         DR    RE,R8                                                            
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         ST    RF,UPI1                                                          
HOMES1   OC    UPINF2,UPINF2                                                    
         BZ    HOMES2                                                           
         GOTO1 VDEMEX,UPDMCB,(C'R',WORKREC),(1,UPFULL)                          
         MVC   UPI2,=F'1000'        ADI FACTOR                                  
         MVC   UPI3(8),UPI2                                                     
         L     R8,UPFULL                                                        
         LTR   R8,R8                                                            
         BZ    HOMES2                                                           
         SR    RE,RE                                                            
         L     RF,UPINF2                                                        
         M     RE,=F'1000'                                                      
         SLDA  RE,1                                                             
         DR    RE,R8                                                            
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,UPI2                                                          
         MVC   UPI3(8),UPI2                                                     
         SPACE 2                                                                
* METRO ADJUSTMENT                                                              
HOMES2   OC    UPINF3,UPINF3                                                    
         BZ    HOMES3                                                           
         GOTO1 VDEMEX,UPDMCB,(C'R',WORKREC),(2,UPFULL)                          
         MVC   UPI4,=F'1000'                                                    
         L     R8,UPFULL                                                        
         LTR   R8,R8                                                            
         BZ    HOMES3                                                           
         L     RF,UPINF3                                                        
         SR    RE,RE                                                            
         M     RE,=F'1000'                                                      
         SLDA  RE,1                                                             
         DR    RE,R8                                                            
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,UPI4                                                          
         SPACE 2                                                                
* PUT ADJUSTMENT                                                                
HOMES3   OC    UPINF4,UPINF4                                                    
         BZ    HOMESX                                                           
         L     RF,UPINF2                                                        
         SR    RE,RE                                                            
         M     RE,=F'10'                                                        
         SLDA  RE,1                                                             
         D     RE,UPINF4                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,UPFULL2                                                       
         GOTO1 VDEMEX,UPDMCB,(C'P',WORKREC),(1,UPFULL)                          
         L     RF,UPFULL2                                                       
         SR    RE,RE                                                            
         M     RE,=F'2000'                                                      
         D     RE,UPFULL                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,UPI3                                                          
HOMESX   L     RF,UPI1             ADJUST TSA ADJUSTMENT BY ADI FACTOR          
         SR    RE,RE                                                            
         M     RE,UPI2                                                          
         SLDA  RE,1                                                             
         D     RE,=F'1000'                                                      
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,UPI1                                                          
         XIT1                                                                   
         EJECT                                                                  
ADJ      NTR1                                                                   
         LA    RE,ADJLIST                                                       
         LA    R3,WORKREC                                                       
         USING PWREC,R3                                                         
ADJ1     TM    0(RE),X'F0'         FIELD 1-4                                    
         BZ    ADJN                                                             
         ZIC   R6,0(RE)             NO - GET SINGLE FIELD                       
         SRL   R6,4                                                             
         BCTR  R6,0                                                             
         LA    R6,UPI1(R6)                                                      
         LH    R7,2(RE)                                                         
         LA    R7,PWDATA(R7)       SET DATA ADDRESS                             
         L     R9,0(R7)            GET ORIGINAL DATA                            
         SR    R8,R8                                                            
         M     R8,0(R6)            ADJUST DATA                                  
         SLDA  R8,1                                                             
         D     R8,=F'1000'                                                      
         A     R9,=F'1'                                                         
         SRA   R9,1                                                             
         ST    R9,0(R7)            STORE DATA                                   
         B     ADJNEXT                                                          
         SPACE 2                                                                
ADJN     LH    R7,2(RE)            SET DATA ADDRESS                             
         LA    R7,PWDATA(R7)                                                    
         ZIC   R5,1(RE)            SET BCT LOOP FOR FORMULA                     
         SRL   R5,2                                                             
ADJ2     LA    R6,UPI1                                                          
         LA    R4,3                SET BCT LOOP FOR ITEM                        
         MVI   METROSW,0                                                        
         LA    RF,PWIMET                                                        
         CR    RF,R7                                                            
         BNE   *+8                                                              
         MVI   METROSW,1                                                        
ADJ3     L     R9,0(R7)                                                         
         SR    R8,R8                                                            
         CLI   METROSW,1                                                        
         BNE   *+8                                                              
         LA    R6,UPI4                                                          
         M     R8,0(R6)                                                         
         SLDA  R8,1                                                             
         D     R8,=F'1000'                                                      
         LTR   R9,R9                                                            
         BM    *+8                                                              
         A     R9,=F'1'                                                         
         SRA   R9,1                                                             
         ST    R9,0(R7)                                                         
         LA    R7,4(R7)                                                         
         LA    R6,4(R6)                                                         
         BCT   R4,ADJ3                                                          
         LA    R7,4(R7)                                                         
         BCT   R5,ADJ2                                                          
         SPACE 2                                                                
ADJNEXT  LA    RE,4(RE)            GET NEXT FORMULA                             
         CLI   0(RE),0                                                          
         BNE   ADJ1                                                             
         LA    R7,PWRPO2           ADJUST SPECIAL DEMOS                         
         LA    R5,19                                                            
ADJR     OC    0(4,R7),0(R7)                                                    
         BZ    ADJR1                                                            
         L     RF,0(R7)                                                         
         SR    RE,RE                                                            
         M     RE,UPI2                                                          
         SLDA  RE,1                                                             
         D     RE,=F'1000'                                                      
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,0(R7)                                                         
ADJR1    LA    R7,4(R7)                                                         
         BCT   R5,ADJR                                                          
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
DEMUPW   DSECT                                                                  
UPPARA   DS    0CL8                                                             
UPAREC   DS    F                                                                
UPAUP    DS    F                                                                
UPDMCB   DS    6F                                                               
UPFULL   DS    F                                                                
UPFULL2  DS    F                                                                
AWORK    DS    F                                                                
ARECORD  DS    F                                                                
AFRSTEL  DS    F                                                                
AUPELEM  DS    F                                                                
VHELLO   DS    F                                                                
VCALLOV  DS    F                                                                
VPAVEXPL DS    F                                                                
VDEMEX   DS    F                                                                
VPAVCOND DS    F                                                                
UPI1     DS    F                   INDEX FOR ADI                                
UPI2     DS    F                   INDEX FOR TSA                                
UPI3     DS    F                   INDEX FOR PVT                                
UPI4     DS    F                   INDEX FOR METRO                              
UPINF1   DS    F                   INPUT                                        
UPINF2   DS    F                                                                
UPINF3   DS    F                                                                
UPINF4   DS    F                                                                
UPINF5   DS    F                                                                
UNIV     DS    F                                                                
UPINCD   DS    C                                                                
UPINDEM  DS    C                                                                
FILNAM   DS    CL8                                                              
METROSW  DS    C                                                                
DEMOTYP  DS    C                                                                
DEMONO   DS    C                                                                
SRC      DS    CL1                                                              
BOOK     DS    CL2                                                              
OUTEL    DS    500C                                                             
WORKREC  DS    CL550                                                            
ADJLIST  DS    CL300                                                            
       ++INCLUDE DMPAVWORKD                                                     
RINVD    DSECT                                                                  
       ++INCLUDE REGENINV                                                       
       ++INCLUDE REGENAVL                                                       
       ++INCLUDE DMPAVFILED                                                     
