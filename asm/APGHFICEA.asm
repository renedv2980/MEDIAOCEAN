*          DATA SET APGHFICEA  AT LEVEL 002 AS OF 05/01/02                      
*                                                                               
*PHASE ACHFICEA,+0                                                              
         TITLE 'APG HOOK FOR CLIENT PL'                                         
ACHFICEA CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8                                                    
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         L     R5,HOOKAREC         ADDR OF SORT REC                             
         USING SRTRECD,R5                                                       
         CLI   HOOK1ST,C'Y'                                                     
         BNE   HOOKSORT                                                         
*                                                                               
* BUILD SUPER LEDGER TABLE                                                      
*                                                                               
         MVI   HOOK1ST,C'N'                                                     
SUP00    LA    R6,SUPLDGTB                                                      
         USING SPLDGD,R6                                                        
         LA    R3,MYIO                                                          
         USING ACKEYD,R3                                                        
         MVC   ACBTKACC(42),SPACES                                              
         MVI   0(R3),X'54'                                                      
         MVC   1(2,R3),=C'FT'                                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD  ',=C'ACCOUNT',(R3),(R3)                  
         CLI   DMCB+8,0            THERE'S GOT TO BE THIS SUPER LEDGER          
         BE    *+6                                                              
         DC    H'0'                ELSE DIE                                     
SUP10    TM    ACSTATUS,X'80'      DON'T READ DELETED ONES                      
         BO    SUP95                                                            
*                                                                               
         CLC   1(2,R3),=C'FT'      HAS TO BE FT SUPER LEDGER                    
         BNE   HOOKSORT                                                         
         CLI   QOPT1,C' '                                                       
         BE    SUP10O                                                           
         CLI   QOPT1,C'O'                                                       
         BE    SUP10O                                                           
         CLI   QOPT1,C'C'                                                       
         BE    SUP10C                                                           
         CLI   QOPT1,C'A'                                                       
         BNE   SUP95                                                            
*                                                                               
         CLI   3(R3),C'A'          JUST A'S?                                    
         BNE   SUP95                                                            
         B     SUP10X                                                           
SUP10C   CLI   3(R3),C'C'          JUST C'S?                                    
         BNE   SUP95                                                            
         B     SUP10X                                                           
SUP10O   CLI   3(R3),C'A'          EVERYTHING ELSE                              
         BE    SUP95                                                            
         CLI   3(R3),C'C'                                                       
         BE    SUP95                                                            
*                                                                               
SUP10X   MVI   GOT15,C'N'                                                       
         LR    RF,R3                                                            
         LR    RE,R3                                                            
         AH    RE,DATADISP         WANT TO FIND NAME ELEMENT                    
SUP15    CLI   0(RE),0             END OF RECORD                                
         BE    SUP17                                                            
         CLI   0(RE),X'20'         NAME ELEMENT                                 
         BE    SUP19                                                            
         ZIC   R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     SUP15                                                            
*                                                                               
SUP17    LR    RE,R3                                                            
*                                                                               
         USING ACNAMED,RE                                                       
SUP19    AH    RF,DATADISP                                                      
SUP20    CLI   0(RF),0                                                          
         BE    SUP90                                                            
         MVC   SPREPNAM,3(R3)      WE'RE EXCLUDING COMP + FT                    
         MVC   SPDESCRP,SPACES                                                  
         CLI   0(RF),X'15'         GENERAL LEDGER ELEMENT                       
         BNE   SUP29                                                            
         MVI   GOT15,C'Y'          GOT A X'15' ELEMENT                          
         USING ACGENLD,RF                                                       
         MVC   SPCDNAME,ACGLACC    LOOKING FOR THIS NAME                        
*                                                                               
         LA    R1,14               DEFAULT LENGTH                               
         LA    R4,ACGLACC+13                                                    
SUP20L   CLI   0(R4),C' '          FIRST NON-SPACE BACKWARDS IS END             
         BNE   SUP20X                                                           
         BCTR  R4,0                                                             
         BCT   R1,SUP20L                                                        
SUP20X   STC   R1,SPCDNMEL                                                      
*                                                                               
         MVI   SPROWNUM,3          LOWER LEVEL                                  
         CR    RE,R3               NO DESCRIPTION?                              
         BE    SUP25                                                            
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SPDESCRP(0),ACNMNAME                                             
SUP25    LA    R6,SPLDGLEN(R6)                                                  
SUP29    ZIC   R1,1(RF)                                                         
         AR    RF,R1                                                            
         B     SUP20                                                            
*                                                                               
SUP90    CLI   GOT15,C'Y'          HAVE TO ADD ONE TO TABLE                     
         BE    SUP95                 IF THERE WASN'T A 15 ELEMENT               
         MVI   SPROWNUM,1                                                       
         CLI   4(R3),C' '                                                       
         BE    *+8                                                              
         MVI   SPROWNUM,2                                                       
         CR    RE,R3               NO DESCRIPTION?                              
         BE    SUP25                                                            
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SPDESCRP(0),ACNMNAME                                             
SUP92    LA    R6,SPLDGLEN(R6)                                                  
SUP95    GOTO1 DATAMGR,DMCB,=C'DMRSEQ  ',=C'ACCOUNT',(R3),(R3)                  
         B     SUP10                                                            
         EJECT                                                                  
*--------------------------------------------------------------------           
*        HOOKSORT - GOT TO SEE IF ACTUAL ACCOUNT IS IN TABLE                    
*                     NO, DON'T USE IT                                          
*                    YES, USE IT, BUT CHANGE SORT RECORD                        
*--------------------------------------------------------------------           
HOOKSORT DS    0H                                                               
         LA    R6,SUPLDGTB                                                      
         USING SPLDGD,R6                                                        
HKSRT10  CLI   SPROWNUM,0          EOT                                          
         BZ    HKSRTNO                                                          
         CLI   SPROWNUM,1          ROW 1 NAME?                                  
         BNE   *+16                                                             
         MVC   SAVNAME1,SPDESCRP                                                
         MVC   SAVNAME2,SPACES                                                  
         CLI   SPROWNUM,2          ROW 2 NAME?                                  
         BNE   *+10                                                             
         MVC   SAVNAME2,SPDESCRP                                                
         ZIC   R1,SPCDNMEL                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SPCDNAME(0),ACTACC+1   IS IT IN THE TABLE?                       
         BE    HKSRT20                                                          
HKSRT15  LA    R6,SPLDGLEN(R6)                                                  
         B     HKSRT10                                                          
*                                                                               
HKSRT20  MVI   DMCB,0                                                           
         BAS   RE,DUMPSRT                                                       
         LR    RF,R6                                                            
         CLI   SRTREPN,3                                                        
         BL    HKSRTYES                                                         
         MVC   SRTCDE1,SPREPNAM                                                 
         MVC   SRTNME1,SAVNAME1                                                 
         CLI   SRTREPN,5                                                        
         BL    HKSRTYES                                                         
         MVC   SRTCDE2,SPREPNAM+1                                               
         MVC   SRTNME2,SAVNAME2                                                 
         CLI   SRTREPN,7                                                        
         BL    HKSRTYES                                                         
         MVC   SRTCDE3,SPREPNAM+4                                               
         MVC   SRTNME3,SPDESCRP                                                 
         B     HKSRTYES                                                         
*                                                                               
HKSRTNO  LTR   RE,RE                                                            
         B     XIT                                                              
HKSRTYES MVI   DMCB,1                                                           
         BAS   RE,DUMPSRT                                                       
         SR    RE,RE                                                            
XIT      XIT1                                                                   
*--------------------------------------------------------------------           
DUMPSRT  NTR1                                                                   
         CLI   QOPT5,C'Y'                                                       
         BNE   XIT                                                              
         CLI   DMCB,0                                                           
         BNE   DUMPSRT2                                                         
         LA    RF,=C'BEFORE'                                                    
         B     *+8                                                              
DUMPSRT2 LA    RF,=C'AFTER '                                                    
         ST    RF,DMCB                                                          
         MVI   DMCB,6                                                           
         L     RF,HOOKAREC                                                      
         GOTO1 PRNTBL,DMCB,,(RF),C'DUMP',S1LEN,=C'1D',(C'P',PRINT)              
         B     XIT                                                              
*--------------------------------------------------------------------           
         EJECT                                                                  
*                                                                               
HOOK1ST  DC    C'Y'                READ SUPER LEDGER ONCE                       
SAVNAME1 DC    CL36' '                                                          
SAVNAME2 DC    CL36' '                                                          
         LTORG                                                                  
GOT15    DS    CL1                                                              
MYIO     DS    CL1000                                                           
SUPLDGTB DS    (50*SPLDGLEN)C                                                   
         EJECT                                                                  
SPLDGD   DSECT                     SUPER LEDGER TABLE DSECT                     
SPROWNUM DS    XL1                 LEVEL NUMBER                                 
SPCDNMEL DS    XL1                 CD NAME LENGTH                               
SPCDNAME DS    CL14                                                             
SPREPNAM DS    CL10                COMPANY & FT EXCLUDED                        
SPDESCRP DS    CL36                DESCRIPTION                                  
SPLDGLEN EQU   *-SPLDGD                                                         
         EJECT                                                                  
*              DSECT TO COVER REPTAB                                            
*                                                                               
SRTRECD  DSECT                     SORT RECORD DSECT                            
SRTROW1  DS    XL2                 REPORT NUMBER/# OF COPIES                    
SRTCDE1  DS    CL1                                                              
         DS    CL13                                                             
SRTROW2  DS    XL2                                                              
SRTCDE2  DS    CL3                                                              
         DS    CL11                                                             
SRTROW3  DS    XL2                                                              
SRTCDE3  DS    CL3                                                              
         DS    CL11                                                             
SRTROW4  DS    XL2                                                              
SRTCDE4  DS    CL2                                                              
         DS    CL12                                                             
SRTROW5  DS    XL2                                                              
SRTCDE5  DS    CL1                                                              
         DS    CL13                                                             
SRTROW6  DS    XL2                                                              
SRTCDE6  DS    CL3                                                              
         DS    CL11                                                             
*                                                                               
SRTREPN  DS    CL1                                                              
SRTCOPIS DS    CL1                                                              
         DS    XL2                                                              
SRTNME1  DS    CL36                                                             
SRTNME2  DS    CL36                                                             
SRTNME3  DS    CL36                                                             
SRTNME4  DS    CL36                                                             
SRTNME5  DS    CL36                                                             
*                                                                               
SRTCOLS  DS    26PL8                                                            
*                                                                               
S1LEN    EQU   *-SRTRECD           LENGTH OF SORT RECORD                        
         EJECT                                                                  
*        INCLUDED HERE                                                          
*        ACREPWORKD                                                             
*        ACPGWORKD                                                              
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACAPGWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002APGHFICEA 05/01/02'                                      
         END                                                                    
