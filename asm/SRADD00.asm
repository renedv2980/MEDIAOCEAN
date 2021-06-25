*          DATA SET SRADD00    AT LEVEL 036 AS OF 02/23/09                      
*PHASE T12900A                                                                  
*INCLUDE QSORT                                                                  
         TITLE '$ADDRESS - LIST SYSTEM CORE ADDRESSES'                          
         PRINT NOGEN                                                            
ADDRESS  RSECT                                                                  
         NMOD1 ADDWRKX-ADDWRK,**$ADD**,CLEAR=YES,RR=R2                          
         USING ADDWRK,RC           RC=A(W/S)                                    
         ST    R2,RELO                                                          
         MVC   SRPARS,0(R1)                                                     
         L     R3,SRPAR6                                                        
         USING SRADDFFD,R3         R3=A(TWA)                                    
         L     R4,SRPAR4                                                        
         USING COMFACSD,R4         R4=A(COMFACS)                                
         L     RA,SRPAR1                                                        
         USING SYSFACD,RA          RA=A(SYSFACS)                                
*                                                                               
         L     R2,VSSB                                                          
         USING SSBD,R2                                                          
         MVC   LOWADD,SSBLOADR     SET LOW CORE ADDRESS AND                     
         MVC   RELADD,SSBLOADR     DEFAULT RELOCATION FACTOR                    
         DROP  R2                                                               
         LA    R2,SRVP3H                                                        
         CLI   5(R2),0                                                          
         BE    AD0                                                              
         TM    4(R2),X'02'                                                      
         BZ    ERR1                                                             
         CLI   5(R2),6                                                          
         BH    ERR2                                                             
         SR    R8,R8               RIGHT ALIGN INTO DUB                         
         IC    R8,5(R2)                                                         
         LA    R9,8                                                             
         SR    R9,R8                                                            
         LA    R9,DUB(R9)                                                       
         MVC   DUB,=8C'0'                                                       
         BCTR  R8,0                                                             
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R9),8(R2)                                                    
         LA    R1,RELADD                                                        
         ST    R1,DMCB+4                                                        
         GOTO1 CHEXIN,DMCB,DUB,,8                                               
*                                                                               
AD0      DS    0H                                                               
         LA    R2,SRVP1H                                                        
         MVI   DISPIT,FACPAKQ      DEFAULT DISPLAY IS FACPAK                    
         CLI   5(R2),0                                                          
         BE    AD1                                                              
         LA    R5,DISPTAB                                                       
*                                                                               
AD0A     CLI   0(R5),X'FF'         CHECK FOR VALID DISPLAY TYPE                 
         BE    ERR3                                                             
         ZIC   R1,8(R5)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),8(R2)                                                    
         BE    *+12                                                             
         LA    R5,L'DISPTAB(R5)                                                 
         B     AD0A                                                             
         MVC   DISPIT,9(R5)                                                     
*                                                                               
AD1      DS    0H                                                               
         LA    R9,ADDTAB           R9=A(NEXT TABLE ENTRY)                       
         SR    R2,R2               R2=# ENTRIES IN TABLE                        
         EJECT                                                                  
*                                                                               
* ADDRESS FROM SYSFAC/COMFAC/SRPARM                                             
*                                                                               
         TM    DISPIT,FACPAKQ      TEST REQUIRED                                
         BZ    AD6                                                              
*                                                                               
         SR    R8,R8               EX "LEN"                                     
         L     R5,=A(BASTAB)       R5=A(BASIC TABLE)                            
         A     R5,RELO                                                          
AD2      CLI   0(R5),X'FF'         END OF TABLE                                 
         BE    AD6                                                              
         EX    R8,0(R5)            GET A(ENTRY) IN R6 (LOAD R6,VTYPE)           
         LTR   R6,R6                                                            
         BZ    AD4                                                              
         MVC   0(16,R9),4(R5)      MOVE FIELD NAME                              
         ST    R6,16(R9)                                                        
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
*                                                                               
AD4      LA    R5,L'BASTAB(R5)                                                  
         B     AD2                                                              
AD6      DS    0H                                                               
*                                                                               
* ADDRESSES FROM SELIST                                                         
*                                                                               
         TM    DISPIT,SELISTQ      TEST REQUIRED                                
         BZ    AD10                                                             
*                                                                               
         L     R5,VSELIST                                                       
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING SELISTD,R5                                                       
         MVI   LSTOVSYS,X'FF'      NE ON FIRST COMPARE                          
*                                                                               
AD8      MVC   0(16,R9),=C'        PRGMS   '                                    
         MVC   0(7,R9),SENAME                                                   
         MVC   16(4,R9),SEPGMS                                                  
         MVI   16(R9),0            CLEAR HOB                                    
         CLC   LSTOVSYS,SEOVSYS                                                 
         MVC   LSTOVSYS,SEOVSYS                                                 
         BE    *+12                                                             
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
         MVC   0(16,R9),=C'        FILES   '                                    
         MVC   0(7,R9),SENAME                                                   
         MVC   16(4,R9),SEFILES                                                 
         OC    SEFILES,SEFILES                                                  
         BNZ   *+14                                                             
         XC    0(L'ADDTAB,R9),0(R9)                                             
         B     *+12                                                             
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
         BXLE  R5,R6,AD8                                                        
AD10     DS    0H                                                               
*                                                                               
* ADDRESSES FROM TCB                                                            
*                                                                               
         TM    DISPIT,TASKQ        TEST REQUIRED                                
         BZ    AD14                                                             
*                                                                               
         L     R5,VTCB                                                          
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING TCBD,R5                                                          
*                                                                               
AD12     MVC   0(16,R9),=C'       WRK AREA '                                    
         MVC   0(6,R9),TCBID+1                                                  
         MVC   16(4,R9),TCBWRKA                                                 
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
                                                                                
         MVC   0(16,R9),=C'       TIA AREA '                                    
         MVC   0(6,R9),TCBID+1                                                  
         MVC   16(4,R9),TCBTIA                                                  
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
                                                                                
         MVC   0(16,R9),=C'       PGM AREA '                                    
         MVC   0(6,R9),TCBID+1                                                  
         MVC   16(4,R9),TCBPGMA                                                 
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
                                                                                
         MVC   0(16,R9),=C'       MAP AREA '                                    
         MVC   0(6,R9),TCBID+1                                                  
         MVC   16(4,R9),TCBMAP                                                  
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
                                                                                
         MVC   0(16,R9),=C'       I/O AREA '                                    
         MVC   0(6,R9),TCBID+1                                                  
         MVC   16(4,R9),TCBFILES                                                
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
                                                                                
         MVC   0(16,R9),=C'       TWA AREA '                                    
         MVC   0(6,R9),TCBID+1                                                  
         MVC   16(4,R9),TCBTWA                                                  
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
         BXLE  R5,R6,AD12                                                       
AD14     DS    0H                                                               
*                                                                               
* ADDRESSES FROM SSB                                                            
*                                                                               
         TM    DISPIT,FACPAKQ      TEST REQUIRED                                
         BZ    AD15                                                             
*                                                                               
         L     R5,VSSB                                                          
         USING SSBD,R5                                                          
         MVC   0(16,R9),=CL16'HIGH CORE'                                        
         MVC   16(4,R9),SSBHIADR                                                
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
         MVC   0(16,R9),=CL16'JOBTAB'                                           
         MVC   16(4,R9),SSBAJOB                                                 
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
         MVC   0(16,R9),=CL16'CTRYTAB'                                          
         MVC   16(4,R9),SSBACTRY                                                
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
         MVC   0(16,R9),=CL16'LANGTAB'                                          
         MVC   16(4,R9),SSBALANG                                                
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
         MVC   0(16,R9),=CL16'BCTAB'                                            
         MVC   16(4,R9),SSBABC                                                  
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
         MVC   0(16,R9),=CL16'DICTTAB'  TABLE OF DICTIONARY ADDRESSES           
         MVC   16(4,R9),SSBADICT                                                
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
AD15     DS    0H                                                               
*                                                                               
* ADDRESSES FROM SYSFACS                                                        
*                                                                               
         TM    DISPIT,SYSFACQ      TEST REQUIRED                                
         BZ    AD24                                                             
*                                                                               
         SR    R8,R8               EX "LEN"                                     
         L     R5,=A(SFATAB)       R5=A(SYSFAC TABLE)                           
         A     R5,RELO                                                          
AD20     CLI   0(R5),X'FF'         END OF TABLE                                 
         BE    AD24                                                             
         EX    R8,0(R5)            GET A(ENTRY) IN R6 (LOAD R6,VTYPE)           
         LTR   R6,R6                                                            
         BZ    AD22                                                             
         MVC   0(16,R9),4(R5)      MOVE FIELD NAME                              
         ST    R6,16(R9)                                                        
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
*                                                                               
AD22     LA    R5,L'SFATAB(R5)                                                  
         B     AD20                                                             
AD24     DS    0H                                                               
*                                                                               
* ADDRESSES FROM COMFACS                                                        
*                                                                               
         TM    DISPIT,COMFACQ      TEST REQUIRED                                
         BZ    AD34                                                             
*                                                                               
         SR    R8,R8               EX "LEN"                                     
         L     R5,=A(CFATAB)       R5=A(COMFAC TABLE)                           
         A     R5,RELO                                                          
AD30     CLI   0(R5),X'FF'         END OF TABLE                                 
         BE    AD34                                                             
         EX    R8,0(R5)            GET A(ENTRY) IN R6 (LOAD R6,VTYPE)           
         LTR   R6,R6                                                            
         BZ    AD32                                                             
         MVC   0(16,R9),4(R5)      MOVE FIELD NAME                              
         ST    R6,16(R9)                                                        
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
*                                                                               
AD32     LA    R5,L'CFATAB(R5)                                                  
         B     AD30                                                             
AD34     DS    0H                                                               
*                                                                               
* ADDRESSES FROM LNKTAB                                                         
*                                                                               
         TM    DISPIT,LNKTABQ      TEST REQUIRED                                
         BZ    AD44                                                             
*                                                                               
         SR    R8,R8               EX "LEN"                                     
         L     R7,VLNKTAB                                                       
         USING LNKTABD,R7          R7 MUST ALSO BE "USED" @ LNKTAB              
         L     R5,=A(LNKTAB)       R5=A(LNKTAB TABLE)                           
         A     R5,RELO                                                          
AD40     CLI   0(R5),X'FF'         END OF TABLE                                 
         BE    AD44                                                             
         EX    R8,0(R5)            GET A(ENTRY) IN R6 (LOAD R6,VTYPE)           
         LTR   R6,R6                                                            
         BZ    AD42                                                             
         MVC   0(16,R9),4(R5)      MOVE FIELD NAME                              
         ST    R6,16(R9)                                                        
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
*                                                                               
AD42     LA    R5,L'LNKTAB(R5)                                                  
         B     AD40                                                             
AD44     DS    0H                                                               
         DROP  R7                                                               
********************************************************                        
** SORT ADDTAB INTO EITHER NAME OR CORE ADDRESS ORDER **                        
********************************************************                        
AD915    LA    R9,ADDTAB                                                        
         CLI   SRVP2H+5,0          TEST SORT ON NAME REQUESTED                  
         BNE   AD920               YES                                          
         GOTO1 =V(QSORT),DMCB,(R9),(R2),24,6,16,RR=RELO                         
         B     AD930                                                            
*                                                                               
AD920    GOTO1 =V(QSORT),DMCB,(R9),(R2),24,16,0,RR=RELO                         
         ZIC   RE,SRVP2H+5                                                      
         BCTR  RE,0                                                             
AD925    EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R9),SRVP2       FIND FIRST NAME >= START STRING              
         BNL   AD930               WE'VE GOT IT - START DISPLAYING HERE         
         LA    R9,L'ADDTAB(R9)                                                  
         BCT   R2,AD925                                                         
AD930    DS    0H                                                               
*******************                                                             
** FORMAT SCREEN **                                                             
*******************                                                             
         LA    R2,SRVL1H                                                        
         BAS   RE,FORMAT                                                        
         LA    R2,SRVL2H                                                        
         BAS   RE,FORMAT                                                        
         LA    R2,SRVL3H                                                        
         BAS   RE,FORMAT                                                        
* EXIT                                                                          
*                                                                               
         MVC   SRVMSG(L'MSG),MSG                                                
         FOUT  SRVMSGH                                                          
         OI    SRVIDH+6,X'40'                                                   
         B     EXIT                                                             
*                                                                               
ERR1     MVC   SRVMSG(L'MSG1),MSG1                                              
         B     ERRX                                                             
*                                                                               
ERR2     MVC   SRVMSG(L'MSG2),MSG2                                              
         B     ERRX                                                             
*                                                                               
ERR3     MVC   SRVMSG(L'MSG3),MSG3                                              
         B     ERRX                                                             
*                                                                               
ERRX     FOUT  SRVMSGH                                                          
         NI    SRVIDH+6,X'BF'                                                   
         OI    SRVP2H+6,X'40'                                                   
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
* FORMAT 4 BYTE ADDRESS INTO 6 BYTE OUTPUT                                      
*                                                                               
CVHEX    NTR1                                                                   
         L     R6,DUB              RELOCATE ADCON                               
         S     R6,LOWADD                                                        
         A     R6,RELADD                                                        
         ST    R6,DUB                                                           
         GOTO1 CHEXOUT,DMCB,DUB,WORK,4,=C'TOG'                                  
         MVC   16(8,R9),WORK                                                    
         CLC   16(2,R9),=C'00'                                                  
         BNE   *+10                                                             
         MVC   16(2,R9),=C'  '                                                  
         XIT1                                                                   
* BUILD SCREEN                                                                  
*                                                                               
FORMAT   NTR1                                                                   
         LA    R3,16                                                            
*                                                                               
FORMAT2  MVC   DUB(4),16(R9)                                                    
         CLI   0(R9),0                                                          
         BE    *+8                                                              
         BAS   RE,CVHEX                                                         
         MVC   8(24,R2),0(R9)                                                   
         FOUT  (R2)                                                             
         LA    R2,96(R2)           BUMP 3 LINE ON SCREEN                        
         LA    R9,L'ADDTAB(R9)                                                  
         BCT   R3,FORMAT2                                                       
         XIT1  REGS=(R9)                                                        
*                                                                               
*                                                                               
MSG      DC    C'CORE ADDRESSES DISPLAYED - ENTER SVC REQUEST'                  
MSG1     DC    C'** ERROR ** INVALID HEXADECIMAL'                               
MSG2     DC    C'** ERROR ** FIELD LENGTH EXCEEDS MAXIMUM'                      
MSG3     DC    C'** ERROR ** INVALID INPUT FIELD'                               
*                                                                               
* DISPLAY FLAGS                                                                 
*                                                                               
FACPAKQ  EQU   X'80'                                                            
SYSFACQ  EQU   X'40'                                                            
SELISTQ  EQU   X'20'                                                            
TASKQ    EQU   X'10'                                                            
COMFACQ  EQU   X'08'                                                            
LNKTABQ  EQU   X'04'                                                            
*                                                                               
*        TABLE OF VALID DISPLAYS                                                
*  8 BYTE KEYWORD                                                               
*  1 BYTE MIN MATCH LENGTH-1                                                    
*  1 BYTE DISPLAY FLAG                                                          
*                                                                               
DISPTAB  DS    0CL10                                                            
         DC    C'FACPAK  ',AL1(0),AL1(FACPAKQ)                                  
         DC    C'SYSFAC  ',AL1(1),AL1(SYSFACQ)                                  
         DC    C'SELIST  ',AL1(0),AL1(SELISTQ)                                  
         DC    C'TSK     ',AL1(0),AL1(TASKQ)                                    
         DC    C'COMFAC  ',AL1(0),AL1(COMFACQ)                                  
         DC    C'LNKTAB  ',AL1(0),AL1(LNKTABQ)                                  
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* TABLE OF BASIC ADDRESSES FROM SRPARM/SYSFACS/COMFACS                          
* BYTES 0-03=EXECUTABLE LOAD INSTRUCTION                                        
*       4-21=NAME OF A-TYPE                                                     
*                                                                               
         CNOP  0,4                                                              
BASTAB   DS    0CL20                                                            
         L     R6,SRPAR1                                                        
         DC    CL16'SYSFACS'                                                    
         L     R6,SRPAR4                                                        
         DC    CL16'COMFACS'                                                    
         L     R6,VDATAMGR                                                      
         DC    CL16'DATAMGR'                                                    
         L     R6,VDMOD000                                                      
         DC    CL16'DMFILES'                                                    
         L     R6,VDADDS                                                        
         DC    CL16'DADDS'                                                      
         L     R6,VLCWRITE                                                      
         DC    CL16'LCWRITE'                                                    
         L     R6,VSSB                                                          
         DC    CL16'SSB'                                                        
         L     R6,VUTL                                                          
         DC    CL16'UTL'                                                        
         L     R6,VSELIST                                                       
         DC    CL16'SELIST'                                                     
         L     R6,VUPDTAB                                                       
         DC    CL16'UPDATIVE SOONS'                                             
         L     R6,VTCB                                                          
         DC    CL16'TCB'                                                        
         L     R6,VPHLIST                                                       
         DC    CL16'PHLIST'                                                     
         L     R6,VPRQ                                                          
         DC    CL16'PRINTER QUEUES'                                             
         L     R6,VPRQENTS                                                      
         DC    CL16'PRQ ENTRIES   '                                             
         L     R6,VCHKPT2                                                       
         DC    CL16'REQADDR LISTS'                                              
         L     R6,VADRBUFF                                                      
         DC    CL16'ADRBUFF'                                                    
         L     R6,LOWADD                                                        
         DC    CL16'LOW CORE'                                                   
         L     R6,VTSTTAB                                                       
         DC    CL16'TSTTAB'                                                     
         L     R6,VLOCKTAB                                                      
         DC    CL16'LOCKTAB'                                                    
         L     R6,VTICTOCT                                                      
         DC    CL16'TIMER TABLE'                                                
         DC    X'FFFF'                                                          
         EJECT                                                                  
* TABLE OF SYSFAC ADDRESSES                                                     
* BYTES 0-03=EXECUTABLE LOAD INSTRUCTION                                        
*       4-21=NAME OF A-TYPE                                                     
*                                                                               
         CNOP  0,4                                                              
SFATAB   DS    0CL20                                                            
         L     R6,SRPAR1                                                        
         DC    CL16'SYSFACS'                                                    
         L     R6,VDATAMGR                                                      
         DC    CL16'DATAMGR'                                                    
         L     R6,VDMOD000                                                      
         DC    CL16'DMFILES'                                                    
         L     R6,VDADDS                                                        
         DC    CL16'DADDS'                                                      
         L     R6,VRKEY                                                         
         DC    CL16'RKEY'                                                       
         L     R6,VWKEY                                                         
         DC    CL16'WKEY'                                                       
         L     R6,VEKEY                                                         
         DC    CL16'EKEY'                                                       
         L     R6,VREAD                                                         
         DC    CL16'READ'                                                       
         L     R6,VWRITE                                                        
         DC    CL16'WRITE'                                                      
         L     R6,VWRTAFT                                                       
         DC    CL16'WRTAFT'                                                     
         L     R6,VOPEN                                                         
         DC    CL16'OPEN'                                                       
         L     R6,VOPENIS                                                       
         DC    CL16'OPENIS'                                                     
         L     R6,VFINDSYS                                                      
         DC    CL16'FINDSYS'                                                    
         L     R6,VOPENSYS                                                      
         DC    CL16'OPENSYS'                                                    
         L     R6,VCLSESYS                                                      
         DC    CL16'CLSESYS'                                                    
         L     R6,VRDID                                                         
         DC    CL16'RDID'                                                       
         L     R6,VWTID                                                         
         DC    CL16'WTID'                                                       
         L     R6,VWTCKD                                                        
         DC    CL16'WTCKD'                                                      
         L     R6,VDABACK                                                       
         DC    CL16'DABACK'                                                     
         L     R6,VDATRNS                                                       
         DC    CL16'DATRNS'                                                     
         L     R6,VADDAFT                                                       
         DC    CL16'ADDAFT'                                                     
         L     R6,VFNDEOF                                                       
         DC    CL16'FNDEOF'                                                     
         L     R6,VDACPUID                                                      
         DC    CL16'DACPUID'                                                    
         L     R6,VISCPUID                                                      
         DC    CL16'ISCPUID'                                                    
         L     R6,VDARPT                                                        
         DC    CL16'DARPT'                                                      
         L     R6,VDTFIOA                                                       
         DC    CL16'DTFIOA'                                                     
         L     R6,VDMGRFLS                                                      
         DC    CL16'DMGRFLS'                                                    
         L     R6,VPRGMS                                                        
         DC    CL16'PRGMS  '                                                    
         L     R6,VPRTQUE                                                       
         DC    CL16'PRTQUE '                                                    
         L     R6,VADRFILE                                                      
         DC    CL16'ADRFILE'                                                    
         L     R6,VDMPFILE                                                      
         DC    CL16'DMPFILE'                                                    
         L     R6,VWKFILE                                                       
         DC    CL16'WKFILE '                                                    
         L     R6,VTEMPEST                                                      
         DC    CL16'TEMPEST'                                                    
         L     R6,VPHLIST                                                       
         DC    CL16'PHLIST'                                                     
         L     R6,VADRBUFF                                                      
         DC    CL16'ADRBUFF'                                                    
         L     R6,VTCB                                                          
         DC    CL16'TCB'                                                        
         L     R6,VSSB                                                          
         DC    CL16'SSB'                                                        
         L     R6,VSELIST                                                       
         DC    CL16'SELIST'                                                     
         L     R6,VTSTTAB                                                       
         DC    CL16'TSTTAB'                                                     
         L     R6,VCHKPT1                                                       
         DC    CL16'CHKPT1'                                                     
         L     R6,VCHKPT2                                                       
         DC    CL16'CHKPT2'                                                     
         L     R6,VUTL                                                          
         DC    CL16'UTL'                                                        
         L     R6,VCHKOUT                                                       
         DC    CL16'CHKOUT'                                                     
         L     R6,VDECBLST                                                      
         DC    CL16'DECBLST'                                                    
         L     R6,VUPDTAB                                                       
         DC    CL16'UPDTAB'                                                     
         L     R6,VLCBUFFS                                                      
         DC    CL16'LCBUFFS'                                                    
         L     R6,VLCWRITE                                                      
         DC    CL16'LCWRITE'                                                    
         L     R6,VLOCKTAB                                                      
         DC    CL16'LOCKTAB'                                                    
         L     R6,VENQDEQ                                                       
         DC    CL16'ENQDEQ'                                                     
         L     R6,VPOWWOW                                                       
         DC    CL16'POWWOW '                                                    
         L     R6,VSKIPA                                                        
         DC    CL16'SKIPA'                                                      
         L     R6,VPRQ                                                          
         DC    CL16'PRQ'                                                        
         L     R6,VTICTOC                                                       
         DC    CL16'TICTOC'                                                     
         L     R6,VTICTOCT                                                      
         DC    CL16'TICTOCT'                                                    
         L     R6,VLOGGER                                                       
         DC    CL16'LOGGER'                                                     
         L     R6,VSYSFAC0                                                      
         DC    CL16'SYSFAC0'                                                    
         L     R6,VSYSFAC1                                                      
         DC    CL16'SYSFAC1'                                                    
         L     R6,VSYSFAC2                                                      
         DC    CL16'SYSFAC2'                                                    
         L     R6,VSYSFAC3                                                      
         DC    CL16'SYSFAC3'                                                    
         L     R6,VSYSFAC4                                                      
         DC    CL16'SYSFAC4'                                                    
         L     R6,VVRSNTAB                                                      
         DC    CL16'VRSNTAB'                                                    
         L     R6,VTEMPTRC                                                      
         DC    CL16'TEMPTRC'                                                    
         L     R6,VTWASVR                                                       
         DC    CL16'TWASVR'                                                     
         L     R6,VLOCKSPC                                                      
         DC    CL16'LOCKSPC'                                                    
         L     R6,VLCM                                                          
         DC    CL16'LCM'                                                        
         L     R6,VTERMBLD                                                      
         DC    CL16'TERMBLD'                                                    
         L     R6,VPRQENTS                                                      
         DC    CL16'PRQENTS'                                                    
         DC    X'FFFF'                                                          
         EJECT                                                                  
* TABLE OF COMFAC ADDRESSES                                                     
* BYTES 0-03=EXECUTABLE LOAD INSTRUCTION                                        
*       4-21=NAME OF A-TYPE                                                     
*                                                                               
         CNOP  0,4                                                              
CFATAB   DS    0CL20                                                            
         L     R6,SRPAR4                                                        
         DC    CL16'COMFACS'                                                    
         L     R6,CDATAMGR                                                      
         DC    CL16'DATAMGR'                                                    
         L     R6,CCALLOV                                                       
         DC    CL16'CALLOV'                                                     
         L     R6,CGETMSG                                                       
         DC    CL16'GETMSG'                                                     
         L     R6,CGETTXT                                                       
         DC    CL16'GETTXT'                                                     
         L     R6,CSWITCH                                                       
         DC    CL16'SWITCH'                                                     
         L     R6,CHELLO                                                        
         DC    CL16'HELLO'                                                      
         L     R6,CSCANNER                                                      
         DC    CL16'SCANNER'                                                    
         L     R6,CUNSCAN                                                       
         DC    CL16'UNSCAN'                                                     
         L     R6,CHEXIN                                                        
         DC    CL16'HEXIN'                                                      
         L     R6,CHEXOUT                                                       
         DC    CL16'HEXOUT'                                                     
         L     R6,CCASHVAL                                                      
         DC    CL16'CASHVAL'                                                    
         L     R6,CDATVAL                                                       
         DC    CL16'DATVAL'                                                     
         L     R6,CDATCON                                                       
         DC    CL16'DATCON'                                                     
         L     R6,CTERMVAL                                                      
         DC    CL16'TERMVAL        '                                            
         L     R6,CSCUNKEY                                                      
         DC    CL16'SCUNKEY        '                                            
         L     R6,CADDAY                                                        
         DC    CL16'ADDAY          '                                            
         L     R6,CGETDAY                                                       
         DC    CL16'GETDAY         '                                            
         L     R6,CGETPROF                                                      
         DC    CL16'GETPROF        '                                            
         L     R6,CPERVERT                                                      
         DC    CL16'PERVERT        '                                            
         L     R6,CGETFACT                                                      
         DC    CL16'GETFACT        '                                            
         L     R6,CXSORT                                                        
         DC    CL16'XSORT          '                                            
         L     R6,CREQTWA                                                       
         DC    CL16'GETFLD         '                                            
         L     R6,CREQTWA                                                       
         DC    CL16'GETFLD         '                                            
*&&UK                                                                           
         L     R6,CPERVAL                                                       
         DC    CL16'CPERVAL        '                                            
         L     R6,CDLFLD                                                        
         DC    CL16'CDLFLD         '                                            
         L     R6,CGENERAL                                                      
         DC    CL16'CGENERAL       '                                            
         L     R6,CSEARCH                                                       
         DC    CL16'CSEARCH        '                                            
         L     R6,CCONVERT                                                      
         DC    CL16'CCONVERT       '                                            
         L     R6,CPRORATA                                                      
         DC    CL16'CPRORATA       '                                            
         L     R6,CLIMACC                                                       
         DC    CL16'CLIMACC        '                                            
         L     R6,CSRCHCAL                                                      
         DC    CL16'CSRCHCAL       '                                            
*&&                                                                             
*&&US                                                                           
         L     R6,CDEMADDR                                                      
         DC    CL16'DEMADDR       '                                             
         L     R6,CDEMOUT                                                       
         DC    CL16'DEMOUT         '                                            
         L     R6,CDEMEL                                                        
         DC    CL16'DEMEL          '                                            
         L     R6,CDEMAINT                                                      
         DC    CL16'DEMAINT        '                                            
         L     R6,CDEMAND                                                       
         DC    CL16'DEMAND         '                                            
         L     R6,CDEMOMTH                                                      
         DC    CL16'DEMOMTH        '                                            
         L     R6,CDEMOVAL                                                      
         DC    CL16'DEMOVAL        '                                            
         L     R6,CDEFINE                                                       
         DC    CL16'DEFINE         '                                            
         L     R6,CDEMTABS                                                      
         DC    CL16'DEMTABS        '                                            
         L     R6,CDEMOCON                                                      
         DC    CL16'DEMOCON        '                                            
         L     R6,CGENERAL                                                      
         DC    CL16'GENERAL        '                                            
         L     R6,CPERVAL                                                       
         DC    CL16'PERVAL         '                                            
         L     R6,CDLFLD                                                        
         DC    CL16'DLFLD          '                                            
         L     R6,CSEARCH                                                       
         DC    CL16'SEARCH         '                                            
         L     R6,CLIMACC                                                       
         DC    CL16'LIMACC         '                                            
         L     R6,CSRCHCAL                                                      
         DC    CL16'SRCHCAL        '                                            
*&&                                                                             
         L     R6,CGLOBBER                                                      
         DC    CL16'GLOBBER        '                                            
         L     R6,CMINIO                                                        
         DC    CL16'MINIO          '                                            
         L     R6,CPARSNIP                                                      
         DC    CL16'PARSNIP        '                                            
         L     R6,CDICTATE                                                      
         DC    CL16'DICTATE        '                                            
         L     R6,CEDITOR                                                       
         DC    CL16'EDITOR         '                                            
         L     R6,CGETHELP                                                      
         DC    CL16'GETHELP        '                                            
         L     R6,CCUREDIT                                                      
         DC    CL16'CUREDIT        '                                            
         L     R6,CGETRET                                                       
         DC    CL16'GETRET         '                                            
         L     R6,CREPORT                                                       
         DC    CL16'REPORT         '                                            
         L     R6,CBLDCUR                                                       
         DC    CL16'BLDCUR         '                                            
         L     R6,CGETCUR                                                       
         DC    CL16'GETCUR         '                                            
         L     R6,CGETNAR                                                       
         DC    CL16'GETNAR         '                                            
         L     R6,CDEJAVU                                                       
         DC    CL16'DEJAVU         '                                            
         L     R6,CSECRET                                                       
         DC    CL16'SECRET         '                                            
         L     R6,CBILLIT                                                       
         DC    CL16'BILLIT         '                                            
         L     R6,CLOCKET                                                       
         DC    CL16'LOCKET         '                                            
         L     R6,CPQPROF                                                       
         DC    CL16'PQPROF         '                                            
         L     R6,CSCRIPT                                                       
         DC    CL16'SCRIPT         '                                            
         L     R6,CDATTIM                                                       
         DC    CL16'DATTIM         '                                            
         L     R6,CBINSRCH                                                      
         DC    CL16'BINSRCH        '                                            
         DC    X'FFFF'                                                          
         EJECT                                                                  
* TABLE OF LNKTAB ADDRESSES                                                     
* BYTES 0-03=EXECUTABLE LOAD INSTRUCTION                                        
*       4-21=NAME OF A-TYPE                                                     
*                                                                               
         CNOP  0,4                                                              
         USING LNKTABD,R7                                                       
LNKTAB   DS    0CL20                                                            
         L     R6,LBLDCUR                                                       
         DC    CL16'BLDCUR         '                                            
         L     R6,LCARDS                                                        
         DC    CL16'CARDS          '                                            
         L     R6,LDS00900                                                      
         DC    CL16'DDS00900       '                                            
         L     R6,LDS00901                                                      
         DC    CL16'DDS00901       '                                            
         L     R6,LACCEMU                                                       
         DC    CL16'DMACCEMU         '                                          
         L     R6,LDALINK                                                       
         DC    CL16'DMDALINK         '                                          
         L     R6,LDANDX                                                        
         DC    CL16'DMDANDX          '                                          
         L     R6,LDAPTRS                                                       
         DC    CL16'DMDAPTRS         '                                          
         L     R6,LDTFIOA                                                       
         DC    CL16'DMDTFIOA         '                                          
         L     R6,LDTFS                                                         
         DC    CL16'DMDTFS/DMCNTL    '                                          
         L     R6,LDYNDD                                                        
         DC    CL16'DMDYNDD          '                                          
         L     R6,LENQCTL                                                       
         DC    CL16'DMENQCTL         '                                          
         L     R6,LISDDS                                                        
         DC    CL16'DMISDDS          '                                          
         L     R6,LLOCKER                                                       
         DC    CL16'DMLOCKER         '                                          
         L     R6,LDMPRTQ                                                       
         DC    CL16'DMPRTQ         '                                            
         L     R6,LDMPRTQO                                                      
         DC    CL16'DMPRTQO        '                                            
         L     R6,LDMRCVR                                                       
         DC    CL16'DMRCVR         '                                            
         L     R6,LDMWRKF                                                       
         DC    CL16'DMWRKF         '                                            
         L     R6,LDMWRKR                                                       
         DC    CL16'DMWRKR         '                                            
         L     R6,LDTCNV                                                        
         DC    CL16'DTCNV          '                                            
         L     R6,LABEND                                                        
         DC    CL16'FAABEND          '                                          
         L     R6,LFALOAD                                                       
         DC    CL16'FALOAD         '                                            
         L     R6,LFAMNTR                                                       
         DC    CL16'FAMNTR         '                                            
         L     R6,LMSGQIN                                                       
         DC    CL16'FAMSGQIN         '                                          
         L     R6,LSCRNCH                                                       
         DC    CL16'FASCRNCH         '                                          
         L     R6,LSECRET                                                       
         DC    CL16'FASECRET         '                                          
         L     R6,LFATAB                                                        
         DC    CL16'FATAB          '                                            
         L     R6,LTASKER                                                       
         DC    CL16'FATASKER         '                                          
         L     R6,LTIADDS                                                       
         DC    CL16'FATIADDS         '                                          
         L     R6,LFATISTR                                                      
         DC    CL16'FATISTR        '                                            
         L     R6,LTI3270                                                       
         DC    CL16'FATI3270         '                                          
         L     R6,LTOADDS                                                       
         DC    CL16'FATOADDS         '                                          
         L     R6,LTOSCRP                                                       
         DC    CL16'FATOSCRP         '                                          
         L     R6,LTOSTR                                                        
         DC    CL16'FATOSTR          '                                          
         L     R6,LTO3270                                                       
         DC    CL16'FATO3270         '                                          
         L     R6,LTWASVR                                                       
         DC    CL16'FATWASVR         '                                          
         L     R6,LHELEN                                                        
         DC    CL16'HELEN          '                                            
         L     R6,LLOADER                                                       
         DC    CL16'LOADER         '                                            
         L     R6,LRECUP                                                        
         DC    CL16'RECUP          '                                            
         DC    X'FFFF'                                                          
         DROP  R7                                                               
         EJECT                                                                  
*              DSECT TO COVER W/S                                               
*                                                                               
ADDWRK   DSECT                                                                  
SRPARS   DS    0CL24                                                            
SRPAR1   DS    F                                                                
SRPAR2   DS    F                                                                
SRPAR3   DS    F                                                                
SRPAR4   DS    F                                                                
SRPAR5   DS    F                                                                
SRPAR6   DS    F                                                                
*                                                                               
DMCB     DS    6F                                                               
*                                                                               
DUB      DS    D                                                                
RELO     DS    A                                                                
WORK     DS    CL20                                                             
LOWADD   DS    A                                                                
RELADD   DS    A                                                                
LSTOVSYS DS    C                                                                
DISPIT   DS    C                                                                
*                                                                               
ADDTAB   DS    300CL24                                                          
ADDWRKX  EQU   *                                                                
*                                                                               
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*                                                                               
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
* FALNKTAB                                                                      
       ++INCLUDE FALNKTAB                                                       
         EJECT                                                                  
SRADDFFD DSECT                                                                  
         DS    CL64                                                             
* SRADDFFD                                                                      
       ++INCLUDE SRADDFFD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036SRADD00   02/23/09'                                      
         END                                                                    
