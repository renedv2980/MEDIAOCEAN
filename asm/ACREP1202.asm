*          DATA SET ACREP1202  AT LEVEL 040 AS OF 03/02/04                      
*PHASE AC1202A                                                                  
*INCLUDE GETLOGO                                                                
*INCLUDE ACLABEL2                                                               
*INCLUDE ACLABEL3                                                               
         TITLE 'JOB LABEL PEEL AND PRINT/DIRECT CAPABILITY'                     
         USING ACWORKD,RA                                                       
         USING LINESD,R8                                                        
AC1202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC12**,RR=R5                                                 
         L     RA,0(,R1)                                                        
         L     R8,ADIO                                                          
                                                                                
         LR    RE,R8                                                            
         LA    RF,L'LINE1*7                                                     
         LA    R0,*                                                             
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0               CLEAR TO SPACES                              
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                INITIALIZATIONS FOR THE RUN.                 
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,PROCACC                                                     
         BE    PACC                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         CLI   MODE,DISKERR                                                     
         BE    DISK                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
*                                                                               
         USING REMOTED,R2                                                       
         USING MASTD,R4                                                         
         L     R4,ADMASTC                                                       
         MVC   UPSI,MCUPSI                                                      
         MVI   DIRECT,NO                                                        
         L     R7,=V(ACLABEL3)     USE 3 LABLES IF PRINTING @ DDS               
*&&US                                                                           
         TM    UPSI,UPSI2LBL                                                    
         BO    RUNF04                                                           
         ICM   R2,15,REMOTEC                                                    
         BZ    RUNF05                                                           
         OC    REMOTKEY,REMOTKEY                                                
         BZ    RUNF05                                                           
         CLC   REMOTFRM,=C'3LBL'  FORCE DIRECT JOB TO 3 LABEL                   
         BE    RUNF05              YES                                          
                                                                                
RUNF04   L     R7,=V(ACLABEL2)     USE 2 LABLE IF GOING TO QUEUE                
         MVI   DIRECT,YES                                                       
*&&                                                                             
                                                                                
RUNF05   AR    R7,R5                                                            
         ST    R7,ALABELS                                                       
         DROP  R2                                                               
                                                                                
         L     R6,PRINT                                                         
         BAS   RE,BLDBLK                                                        
         SR    R4,R4                                                            
*&&UK*&& LA    R4,LABLK                                                         
         CLI   DIRECT,YES                                                       
         BNE   *+8                                                              
         LA    R4,LABLK                                                         
*&&US*&& GOTO1 PRINT,DMCB,SPACES,=C'BC01'      SKIP TO CHANNEL 1                
         GOTO1 ALABELS,DMCB,=C'$OPEN',(R6),1,1,8,(R4)                           
                                                                                
RUNFX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
REQF     ZAP   LINE,=P'0'          RESET LINES SO DKERR MSG  DOESN'T            
         MVI   FORCEHED,C'N'       RESET FORCEHED ALSO                          
REQFX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCESS ACCOUNT                                                    *          
*              ROUTINE TO WRITE OUT STICKY LABELS                    *          
**********************************************************************          
         SPACE 1                                                                
         USING ACLELD,R2                                                        
         USING ACTRECD,R3                                                       
PACC     L     R2,ADLDGHIR                                                      
                                                                                
         LA    R2,ACLVALS                                                       
         USING ACLVALS,R2                                                       
                                                                                
         L     R3,ADACC                                                         
         LA    R4,LINE1                                                         
         ZIC   R5,ACLVLEN                                                       
         BCTR  R5,R0                                                            
         EXMVC R5,0(R4),ACTKACT    CLIENT CODE                                  
*                                                                               
         AHI   R2,L'ACLVALS                                                     
         LA    R4,LINE2                                                         
         LA    R3,1(R3,R5)         BUMP UP IN DISPLACEMENT TO PRODUCT           
         LA    R6,1(,R5)                                                        
         IC    R5,ACLVLEN          CLIENT/PRODUCT LENGTH                        
         SR    R5,R6               LESS CLIENT LENGTH                           
         BCTR  R5,R0                                                            
         EXMVC R5,0(R4),ACTKACT    PRODUCT CODE                                 
*                                                                               
         IC    R6,ACLVLEN          CLIENT/PRODUCT LENGTH                        
         AHI   R2,L'ACLVALS                                                     
         LA    R4,LINE3                                                         
         LA    R3,1(R3,R5)         BUMP UP TO JOB                               
         IC    R5,ACLVLEN          CLIENT/PRODUCT/JOB LENGTH=(12)               
         SR    R5,R6                                                            
         BCTR  R5,R0                                                            
         EXMVC R5,0(R4),ACTKACT                                                 
                                                                                
*--------------------------------*                                              
*        NOW EXTRACT NAMES       *                                              
*--------------------------------*                                              
         BAS   RE,CLINAME                                                       
         BAS   RE,PRDNAME                                                       
         BAS   RE,JOBNAME                                                       
         BAS   RE,LB                                                            
                                                                                
PACCX    B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
**********************************************************************          
* RUN LAST                                                           *          
**********************************************************************          
         SPACE 1                                                                
RUNL     DS    0H                                                               
*                                                                               
         GOTO1 ALABELS,DMCB,=C'$CLOSE'                                          
*                                                                               
RUNLX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* DISK ERROR                                                         *          
**********************************************************************          
         SPACE 1                                                                
DISK     DS    0H                                                               
         LA    R7,11                                                            
         MVC   P,SPACES                                                         
DISK10   GOTO1 PRINT,DMCB,P,PCTL                                                
         BCT   R7,DISK10                                                        
                                                                                
DISKX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LABNAME                                                            *          
**********************************************************************          
         SPACE 1                                                                
         USING NAMELD,R2                                                        
CLINAME  ST    RE,SVRE                                                          
         LA    R3,LINE1                                                         
         L     R2,ADLVANAM                                                      
         LA    RF,1                                                             
         J     GETNAME                                                          
                                                                                
PRDNAME  ST    RE,SVRE                                                          
         LA    R3,LINE2                                                         
         L     R2,ADLVBNAM                                                      
         LA    RF,1                                                             
         J     GETNAME                                                          
                                                                                
JOBNAME  ST    RE,SVRE                                                          
         LA    R3,LINE3                                                         
         L     R2,ADLVCNAM                                                      
         LA    RF,2                                                             
                                                                                
GETNAME  SR    R4,R4                                                            
         IC    R4,NAMLN                                                         
         SHI   R4,NAMLN1Q                                                       
         GOTO1 CHOPPER,DMCB,((R4),NAMEREC),(30,7(R3)),(L'LINE1,(RF))            
         L     RE,SVRE                                                          
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* LB - HANDLE PROFILES                                               *          
**********************************************************************          
         SPACE 1                                                                
         USING PPRELD,R2                                                        
LB       ST    RE,SVRE                                                          
         L     R2,ADPROFIL                                                      
         MVC   LINE5+0(24),=C'BILLING TYPE PROGRESSIVE'                         
         CLI   PPRBTYPE,C'P'                                                    
         BE    LB8                                                              
         MVC   LINE5+13(11),=C'UNBILLABLE '                                     
         CLI   PPRBTYPE,C'U'                                                    
         BE    LB8                                                              
         MVC   LINE5+13(11),=CL11'TOTAL'                                        
         CLI   PPRBTYPE,C'T'                                                    
         BE    LB8                                                              
         MVC   LINE5+13(11),=CL11'ONE-LINE'                                     
         CLI   PPRBTYPE,C'1'                                                    
         BE    LB8                                                              
         MVC   LINE5+13(11),=CL11'CLIENT'                                       
         CLI   PPRBTYPE,C'C'                                                    
         BE    LB8                                                              
         MVC   LINE5+13(11),SPACES                                              
         EDIT  (4,PPRBLAMT),(11,LINE5+13),2,ALIGN=LEFT                          
         CLC   LINE5+14(11),SPACES                                              
         BE    LB8                                                              
         LA    R4,LINE5+24                                                      
*                                                                               
LB6      CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R4,LB6                                                           
         LA    R4,2(R4)                                                         
         MVC   0(3,R4),=C'FEE'                                                  
         CLI   PPRBTYPE,C'F'                                                    
         BE    LB8                                                              
         MVC   0(7,R4),=C'SPECIAL'                                              
         CLI   PPRBTYPE,C'S'                                                    
         BE    LB8                                                              
         MVC   0(10,R4),=C'%ESTIMATE'                                           
         SPACE 2                                                                
LB8      GOTO1 DATCON,DMCB,(4,RCDATE),(8,LINE5+30)                              
*&&UK                                                                           
         MVC   LINE6+30(4),=C'UNIT'                                             
         MVC   LINE6+35(1),PPRUFORA                                             
*&&                                                                             
*&&US                                                                           
         MVC   LINE6+30(6),=C'OFFICE'                                           
         MVC   LINE6+37(2),PPRGAOFF                                             
*&&                                                                             
         CLC   PPRUWRK,SPACES                                                   
         BE    LB12                                                             
         OC    PPRUWRK,PPRUWRK                                                  
         BZ    LB12                                                             
         MVC   LINE6(12),=C'UNBILL CODES'                                       
         LA    R4,PPRUWRK                                                       
         LA    R5,LINE6+13                                                      
         LA    R6,6                                                             
*                                                                               
LB10     MVC   0(2,R5),0(R4)                                                    
         CLC   2(2,R4),SPACES                                                   
         BE    LB12                                                             
         CHI   R6,1                                                             
         BE    LB12                                                             
         CLI   1(R5),C' '                                                       
         BNE   *+6                                                              
         BCTR  R5,0                                                             
         MVI   2(R5),C','                                                       
         AHI   R4,2                                                             
         AHI   R5,3                                                             
         BCT   R6,LB10                                                          
*                                                                               
         USING JOBELD,R2                                                        
LB12     L     R2,ADACC                                                         
         AH    R2,DATADISP                                                      
*                                                                               
LB14     CLI   0(R2),0                                                          
         BE    LB16                                                             
         CLI   0(R2),JOBELQ        X'26'                                        
         BNE   LB20                                                             
         GOTO1 DATCON,DMCB,(1,JOBCDATE),(8,LINE7+13)                            
         MVC   LINE7(12),=C'CLOSING DATE'                                       
*                                                                               
         USING RSTELD,R2                                                        
LB16     L     R2,ADACCSTA                                                      
         TM    RSTSTAT,RSTSACIC    ACCOUNT CLOSED ?                             
         BZ    LBEND                    NO                                      
         MVC   LINE7+30(6),=C'CLOSED'   YES                                     
         B     LBEND                                                            
*                                                                               
LB20     SR    R3,R3                                                            
         IC    R3,1(,R2)                                                        
         AR    R2,R3                                                            
         B     LB14                                                             
*                                                                               
LBEND    GOTO1 ALABELS,DMCB,LINESD   WRITE OUT LABEL AND EXIT                   
                                                                                
         L     RE,SVRE                                                          
         BR    RE                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* BLDBLK - BUILD LABEL LINEUP BLOCK                                  *          
**********************************************************************          
         USING LBBLKD,R3                                                        
         USING LOGOD,R4                                                         
BLDBLK   NTR1                                                                   
         LA    R3,LABLK                                                         
         LA    R5,DMCB                                                          
         EXTRACT (5),'S',FIELDS=TIOT                                            
         L     R5,DMCB                                                          
         MVC   LBJNM,0(R5)         JOBNAME                                      
         MVC   LBSTPNM,8(R5)       STEPNAME                                     
         L     R4,LOGOC                                                         
         GOTO1 =V(GETLOGO),DMCB,ORIGINUM,(R4),DATAMGR                           
*&&UK*&& MVC   LBLOG2,LOGO2                                                     
         MVC   LBLOGNM,LOGONAME                                                 
         MVC   LBLOGAD,LOGOADD                                                  
*&&US                                                                           
         USING CTIREC,R2                                                        
         L     R2,=A(IO)                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,ORIGINUM                                                 
         GOTO1 DATAMGR,DMCB,=CL8'DMREAD',=CL8'CTFILE',(R2),(R2),0               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'00'               MUST HAVE ORIGIN RECORD                      
                                                                                
         LA    R1,CTIDATA                                                       
BLDBLK10 CLI   0(R1),EOR                                                        
         BNE   *+6                                                              
         DC    H'00'               MUST HAVE ORIGIN ID                          
         CLI   0(R1),CTDSCELQ      X'02'                                        
         BE    BLDBLK20                                                         
         ZIC   RF,1(,R1)                                                        
         AR    R1,RF                                                            
         B     BLDBLK10                                                         
                                                                                
         USING CTDSCD,R1                                                        
BLDBLK20 ZIC   RF,CTDSCLEN                                                      
         SHI   RF,(CTDSC-CTDSCD)                                                
         BP    *+6                                                              
         DC    H'00'               MUST HAVE ID                                 
                                                                                
         CHI   RF,L'LBORGNID                                                    
         BNH   *+8                                                              
         LA    RF,L'LBORGNID                                                    
         EXMVC RF,LBORGNID,CTDSC                                                
*&&                                                                             
         B     XIT                                                              
         DROP  R1,R3,R4                                                         
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                                                                    *          
**********************************************************************          
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
K        EQU   2024                                                             
EOR      EQU   0                                                                
                                                                                
ALABELS  DS    A                                                                
SVRE     DS    A                                                                
PCTL     DC    C'BL01'                                                          
DIRECT   DS    C                                                                
UPSI     DS    XL1                 JCL CONTROLED INDICATOR                      
UPSI2LBL EQU   X'80'               .  TEST LABELS 2 ACROSS                      
LABLK    DS    CL(LBBLKLNQ)                                                     
IO       DS    XL(2*K)                                                          
         EJECT                                                                  
*              DSECT TO COVER LABEL RECORD                                      
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
*               ++INCLUDES                                            *         
***********************************************************************         
* ACLABELD                                                                      
       ++INCLUDE ACLABELD                                                       
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*DDREMOTED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040ACREP1202 03/02/04'                                      
         END                                                                    
